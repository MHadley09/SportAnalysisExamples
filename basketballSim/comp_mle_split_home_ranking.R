# game.value <- function(scoreA, scoreB)
# {
#   x <- pmin(pmax(.5, (scoreA / scoreB)^2.9),2)
#   return(.5 + (.692494*log(x)))
# }

game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return ((.5 - (.0000270209*x^3) + (0.0352489*x)))
}

predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function(season)
{
  schedule <- read.csv(file = 'data\\competition\\regular_season_compact_results.csv', sep=',', header=T)
  
  schedule<- schedule[schedule$season == season,]

  data <- data.frame(Team = schedule$wteam, Score = schedule$wscore, 
                     Opp = schedule$lteam, Opp.Score = schedule$lscore, daynum=schedule$daynum)
  
  flipped <- data.frame(Team = schedule$lteam, Score = schedule$lscore, 
             Opp = schedule$wteam, Opp.Score = schedule$wscore, daynum=schedule$daynum)

  data <- rbind(data, flipped)
  
  teams <- unique(data$Team)
  last.step.score <- rep(1600, length(teams))
  score <- rep(1600, length(teams))
  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  
  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.step.score,score)
  for(j in 1:10)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
      gameScores <- game.value(scores, oppScores)
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }

  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

calculate.rankings.with.tourny <- function(season)
{
  schedule <- read.csv(file = 'data\\competition\\regular_season_compact_results.csv', sep=',', header=T)
  tournament <- read.csv(file = 'data\\competition\\tourney_compact_results.csv', sep=',', header=T)
  
  schedule <- rbind(schedule, tournament)
  schedule<- schedule[schedule$season == season,]
  
  data <- data.frame(Team = schedule$wteam, Score = schedule$wscore, 
                     Opp = schedule$lteam, Opp.Score = schedule$lscore, daynum=schedule$daynum)
  
  flipped <- data.frame(Team = schedule$lteam, Score = schedule$lscore, 
                        Opp = schedule$wteam, Opp.Score = schedule$wscore, daynum=schedule$daynum)
  
  data <- rbind(data, flipped)
  
  teams <- unique(data$Team)
  last.step.score <- rep(1600, length(teams))
  score <- rep(1600, length(teams))
  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  
  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.step.score,score)
  for(j in 1:10)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
      gameScores <- game.value(scores, oppScores)
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }
  
  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
 }
 