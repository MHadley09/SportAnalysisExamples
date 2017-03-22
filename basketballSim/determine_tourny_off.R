source("comp_mle_split_home_ranking.R")

game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return ((.5 - (.0000270209*x^3) + (0.0352489*x)))
}

# game.value <- function(scoreA, scoreB)
# {
#   x <- pmin(pmax(.5, (scoreA / scoreB)^2.9),2)
#   return(.5 + (.692494*log(x)))
# }
# 

predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}


tourny.offset <- function()
{
  schedule <- read.csv(file = 'data\\competition\\regular_season_compact_results.csv', sep=',', header=T)
  tournament <- read.csv(file = 'data\\competition\\tourney_compact_results.csv', sep=',', header=T)
  
  all.rankings <- data.frame(Team = unique(schedule$wteam))
  all.rankings$adjustment <- 0

  for(y in 2000:2014)
  {
    tempRank <- calculate.rankings(y)
    missingTeams <- all.rankings[!all.rankings$Team %in% tempRank$Team,]$Team
    missing <- data.frame(Team=unique(missingTeams), Score=rep(0,length(unique(missingTeams))))
    missing$Score <- 0
    missing$Previous.Score <- 0
    missing$Rank <- 0
    
    tempRank <- rbind(tempRank, missing)
    
        
    data <- tournament[tournament$season == y,]
    
    data <- data.frame(Team = data$wteam, Score = data$wscore, 
                       Opp = data$lteam, Opp.Score = data$lscore)
    
    flipped <- data.frame(Team = data$Opp, Score = data$Opp.Score, 
                          Opp = data$Team, Opp.Score = data$Score)
    
    data <- rbind(data, flipped)
        
    teams <- unique(data$Team)
    
    deviation <- sd(game.value(data$Score, data$Opp.Score))
    
    for(i in 1:length(teams))
    {
      opps <- data[data$Team==teams[i],]$Opp
      scores <- data[data$Team==teams[i],]$Score
      oppScores <-  data[data$Team==teams[i],]$Opp.Score
      gameScores <- game.value(scores, oppScores)
      oppScore <- tempRank[tempRank$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      tempRank[tempRank$Team == teams[i],]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
      
    tempRank$Delta <- tempRank$Score - tempRank$Previous.Score
    tempRank$Score <- ifelse(tempRank$Previous.Score == 0, 0, tempRank$Previous.Score)
    tempRank$Previous.Score <- NULL
    tempRank$Rank <- NULL
    
    all.rankings <- merge(all.rankings, tempRank, by="Team")
    
    colnames(all.rankings)[colnames(all.rankings) == "Score"] <- paste0("Score",y)
    colnames(all.rankings)[colnames(all.rankings) == "Delta"] <- paste0("Delta",y)
    
    tempRank <- calculate.rankings.with.tourny(y)
    missingTeams <- all.rankings[!all.rankings$Team %in% tempRank$Team,]$Team
    missing <- data.frame(Team=unique(missingTeams), Score=rep(0,length(unique(missingTeams))))
    missing$Score <- 0
    missing$Previous.Score <- 0
    missing$Rank <- 0
    
    tempRank <- rbind(tempRank, missing) 
    
    tempRank$Previous.Score <- NULL
    tempRank$Rank <- NULL
    
    all.rankings <- merge(all.rankings, tempRank, by="Team")
    
    colnames(all.rankings)[colnames(all.rankings) == "Score"] <- paste0("WithTournyScore",y)
  }

  tempRank <- calculate.rankings(2015)
  missingTeams <- all.rankings[!all.rankings$Team %in% tempRank$Team,]$Team
  missing <- data.frame(Team=unique(missingTeams), Score=rep(0,length(unique(missingTeams))))
  missing$Score <- 0
  missing$Previous.Score <- 0
  missing$Rank <- 0
  
  tempRank <- rbind(tempRank, missing)
  
  all.rankings <- merge(all.rankings, tempRank, by="Team")
  
  colnames(all.rankings)[colnames(all.rankings) == "Score"] <- paste0("Score",2015)
  
  for(m in 1:nrow(all.rankings))
  {
    row <- all.rankings[m,]
    deltas <- as.list(row[,grepl("Delta*",colnames(row))])
    delta <- ifelse(length(unlist(deltas[deltas != 0]))==0,0,mean(unlist(deltas[deltas != 0])))
    all.rankings[m,]$adjustment <- delta
  }
  
  
  return(all.rankings)
}