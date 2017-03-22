game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return (.5 - (.0000270209*x^3) + (0.0352489*x))
}

predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function()
{
  schedule <- read.csv(file = 'data\\2016\\perplaydata.csv', sep=',', header=T)

#  divisions <-read.csv(file = 'data\\2005\\divisionBreakdown.csv', sep=',', header=T)
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                   Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)

  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                  Opp = data$Team, Opp.Score = data$Score))
  
  Starting.Elo <- read.csv(file= 'data\\2016\\preseason\\preseason.csv', sep=',', header=T)

  teams <- unique(data$Team)
  last.step.score <- rep(1600, length(teams))
  score <- rep(1600, length(teams))
  
  rm(schedule)
  

  rankings <- data.frame(Team=teams)

  rankings <- merge(rankings, Starting.Elo, by="Team")
  rankings$Previous.Score <- rankings$Score

  
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
      rankings[i,]$Score <- optimize(f=func, interval=c(rankings[i,]$Previous.Score-300,rankings[i,]$Previous.Score+300), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }

  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

calculate.FBS.Rankings <- function()
{
  rankings <- calculate.rankings()
  divisions <-read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)
  temp <- rankings[!(rankings$Team %in% divisions[divisions$Division=="FCS",]$Team),]
  rankings <- rbind(temp, rankings[rankings$Team %in% divisions[divisions$Division=="FBS G5",]$Team,])
  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}
save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
}
