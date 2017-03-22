game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-45, ((scoreA^2 - scoreB^2)/(scoreA + scoreB))),45)
  return(x)
}

predict <- function(eloA, eloB)
{
  x <- pmin(pmax(-45, ((eloA^2 - eloB^2)/(eloA + eloB))),45)
  return(x)
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function()
{
  schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  
  #  divisions <-read.csv(file = 'data\\2005\\divisionBreakdown.csv', sep=',', header=T)
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                     Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)
  
  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                 Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  last.step.score <- rep(21, length(teams))
  score <- rep(21, length(teams))
  
  rm(schedule)
  
  
  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  
  
  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.step.score,score)
  for(j in 1:3)
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
      rankings[i,]$Score <- optimize(f=func, interval=c(-80,80), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }
  
  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

calculate.FBS.Rankings <- function()
{
  rankings <- calculate.rankings()
  divisions <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)
  FBS <- data.frame(Team=divisions[divisions$Conference != "FCS",]$TEAM)
  FBS <- merge(FBS, rankings, by="Team")
  FBS$Rank <- (1 + length(FBS$Score) - rank(FBS$Score))
  return(FBS)
}
save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
}
