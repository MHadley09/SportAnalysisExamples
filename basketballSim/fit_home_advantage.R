source("gambling_bell_predictor.R")

games <- read.csv(file = 'data\\gambling_scores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games$Spread <- 0

#rankings <- calculate.rankings()

fit.func <- function(adv)
{
  prediction <- data.frame(id=1:nrow(games), percent=1:nrow(games), correct=1:nrow(games))
  
  for(k in 1:nrow(games))
  {
    runs <- 1001
    realGame <- games[k,]
    if(realGame$Neutral %in% c("N", "NE"))
    {
      predictedGame <- neutral.predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team), realGame$Spread, runs)
    }  else  {
      predictedGame <- predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team), realGame$Spread, runs, adv)
    }
    wins <- max(predictedGame$Wins)
    percent <- wins/runs
    correct <- 0
    if(((realGame$Home.Score+realGame$Spread) > realGame$Visitor.Score &&
          (as.character(realGame$Home.Team)==as.character(predictedGame[predictedGame$Wins == wins,]$Team))) ||
         ((realGame$Home.Score+realGame$Spread) < realGame$Visitor.Score &&
            (as.character(realGame$Visitor.Team)==as.character(predictedGame[predictedGame$Wins == wins,]$Team))))
    {
      correct <- 1
    }
    
    prediction[k,]$percent <- percent
    prediction[k,]$correct <- correct
  }
  return(mean(prediction$correct))
}

best <- optimize(f = fit.func, interval = c(1,10), maximum=T)