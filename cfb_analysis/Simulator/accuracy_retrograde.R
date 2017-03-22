source("play_by_play_predict_with_no_zero.R")
#games <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)

games <- read.csv(file = 'data\\2016\\lastweek.csv', sep=',', header=T)
Conferences <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
games <- games[games$Visitor.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
#games <- games[250:469,]
prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))
prediction$Home.Team <- ""
prediction$Visitor.Team <- ""
for(i in 1:nrow(games))
{
  runs <- 1001
  realGame <- games[i,]
  predictedGame <- neutral.predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team),runs)
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
  
  prediction[i,]$Home.Team <-as.character(realGame$Home.Team)
  prediction[i,]$Visitor.Team <- as.character(realGame$Visitor.Team)
}

prediction4 <- prediction
agg <- aggregate(cbind(correct)~percent, data = prediction, FUN=mean)

breakdown7<- agg[1:10,]
for(j in 1:10)
{
   per <- .5 + .05*j
   breakdown7[j,]$percent <- paste(per-.05, per, sep="-")
   breakdown7[j,]$correct<-mean(prediction[prediction$percent >= per-.05 & prediction$percent < per,]$correct)
}
breakdown8 <- agg[1:5,]
for(k in 1:5)
{
  per <- .5 + .1*k
  breakdown8[k,]$percent <- paste(per-.1, per, sep="-")
  breakdown8[k,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
}

mean(prediction4$correct)