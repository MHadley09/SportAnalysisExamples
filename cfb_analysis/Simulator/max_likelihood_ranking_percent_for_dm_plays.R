predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function(schedule)
{

  data <- data.frame(Team = schedule$Team, Score = schedule$Score, 
                   Opp = schedule$Opponent, Opp.Score = 1-schedule$Score)

  teams <- unique(data$Team)
  last.step.rating <- rep(1600, length(teams))
  rating <- rep(1600, length(teams))
  
  rankings <- data.frame(Team=teams, Previous.Rating = last.step.rating, Rating = rating)
  

  deviation <- sd(data$Score)
  
  rm(teams,last.step.rating,rating)
  for(j in 1:5)
  {
    for(i in 1:length(rankings$Team))
    {
      teamData <- data[data$Team==as.character(rankings[i,]$Team),]
      oppData <- rankings[rankings$Team %in% teamData$Opp,]
      
      games <- merge(data.frame(Opp=teamData$Opp, Game.Value=teamData$Score), 
                     data.frame(Opp=oppData$Team, Opp.Value=oppData$Previous.Rating), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
    rankings$Previous.Rating <- rankings$Rating
  }

  rankings$Rank <- (1 + length(rankings$Rating) - rank(rankings$Rating))
  return(rankings)
}

calculate.FBS.Rankings <- function()
{
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  schedule <- read.csv(file = 'data\\evaluations.csv', sep=',', header=T)

  schedule$Team <- trim(schedule$Team)
  schedule$Opponent <- trim(schedule$Opponent)

  
  rankings <- calculate.rankings(schedule)

  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(rankings, fname, row.names=FALSE, sep=",") 
  
  return(rankings)
}
