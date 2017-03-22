source("play_by_play_points_predict_adjuster.r")
games <- read.csv(file = 'data\\perplaydata.csv', sep=',', header=T)
games <- games[games$Home.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
games <- games[games$Visitor.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
#games <- games[1:700,]
prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))

adjust <- function(adjustment)
{
  for(i in 1:nrow(games))
  {
    runs <- 251
    realGame <- games[i,]
    predictedGame <- neutral.predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team),runs, 0.8127365)
    wins <- max(predictedGame$WINS)
    percent <- wins/runs
    correct <- 0
    if((realGame$Home.Score > realGame$Visitor.Score &&
       (as.character(realGame$Home.Team)==as.character(predictedGame[predictedGame$WINS == wins,]$Team))) ||
         (realGame$Home.Score < realGame$Visitor.Score &&
            (as.character(realGame$Visitor.Team)==as.character(predictedGame[predictedGame$WINS == wins,]$Team))))
    {
         correct <- 1
    }
    prediction[i,]$percent <- percent
    prediction[i,]$correct <- correct
  }
  return(prediction)
}

max.correct <- function(x)
{
  pred <- adjust(x)
  return(mean(pred$correct))
}

fit <- optimize(f=max.correct, interval=c(0,2), maximum=T)