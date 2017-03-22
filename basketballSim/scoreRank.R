homeModifier <- 4.416245
game.value <- function(scoreA, scoreB)
{
  x <- scoreA - scoreB
  return(x)
}

predict <- function(eloA, eloB, homeStatus)
{
  eloA <-  ifelse(homeStatus == 1, eloA + homeModifier,eloA)
  return(eloA-eloB)
}

log.likelihood <- function(x, oppElo, homeStatus, game.value, deviation)
{
  return(abs((x-oppElo)-game.value))
  # return((dnorm(predict(x, oppElo, homeStatus), game.value, deviation)))
}

calculate.rankings <- function()
{
  #  homeModifer <- h
  schedule <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  schedule$Visitor.Team <- trim(schedule$Visitor.Team)
  schedule$Home.Team <- trim(schedule$Home.Team)
#   leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)
#   
#   schedule <- schedule[schedule$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
#   
#   schedule <- schedule[schedule$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
#   
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                     Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score, Neutral=schedule$Neutral)
  
  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                 Opp = data$Team, Opp.Score = data$Score, Neutral=schedule$Neutral))
  
  teams <- unique(data$Team)
  
  last.rank.score <-  rep(75, length(teams))
  score <- rep(75, length(teams))
  
  rm(schedule)
  
  rankings <- data.frame(Team=teams, Previous.Score = last.rank.score, Score = score)
  
  
  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.rank.score,score)
  for(j in 1:10)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
      gameScores <- game.value(scores, oppScores)
      site <- ifelse(data[data$Team==as.character(rankings[i,]$Team),]$Neutral %in% c("N", "NE"), 0, 1)
      
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores, Site=site), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Site, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,350))$minimum
    }
    #  print(sqrt(mean((rankings$Score - rankings$Previous.Score)^2)))
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
