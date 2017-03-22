source("better_complex_predict.r")
games <- read.csv(file = 'data\\perplaydata.csv', sep=',', header=T)
Conferences <- read.csv(file = 'data\\conferenceWithFCS.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
games <- games[games$Visitor.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
#games <- games[1:700,]
prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))
for(i in 1:nrow(games))
{
  runs <- 251
  realGame <- games[i,]
  predictedGame <- neutral.predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team),runs)
  wins <- max(predictedGame$WINS)
  percent <- wins/runs
  correct <- 0
  if((realGame$Home.Score > realGame$Visitor.Score &&
     (as.character(realGame$Home.Team)==as.character(predictedGame[predictedGame$WINS == wins,]$TEAM))) ||
       (realGame$Home.Score < realGame$Visitor.Score &&
          (as.character(realGame$Visitor.Team)==as.character(predictedGame[predictedGame$WINS == wins,]$TEAM))))
  {
       correct <- 1
  }
  prediction[i,]$percent <- percent
  prediction[i,]$correct <- correct
}

agg <- aggregate(cbind(correct)~percent, data = prediction, FUN=mean)

breakdown <- agg[1:10,]
for(j in 1:10)
{
   per <- .5 + .05*j
   breakdown[j,]$percent <- paste(per-.05, per, sep="-")
   breakdown[j,]$correct<-mean(prediction[prediction$percent >= per-.05 & prediction$percent < per,]$correct)
}
breakdown2 <- agg[1:5,]
for(k in 1:5)
{
  per <- .5 + .1*k
  breakdown2[k,]$percent <- paste(per-.1, per, sep="-")
  breakdown2[k,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
}