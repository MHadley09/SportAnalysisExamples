source("max_likelihood_ranking.R")
games <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)
Conferences <- read.csv(file = 'data\\2015\\ConferenceWithFCS.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
games <- games[games$Visitor.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
#games <- games[1:700,]
prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))
elo <- calculate.FBS.Rankings()
for(i in 1:nrow(games))
{
  realGame <- games[i,]
#  predictedGame <- predict(elo[elo$Team == as.character(realGame$Home.Team),]$Score,
#                           elo[elo$Team == as.character(realGame$Visitor.Team),]$Score)
  wins <- predict(elo[elo$Team == as.character(realGame$Home.Team),]$Score,
                  elo[elo$Team == as.character(realGame$Visitor.Team),]$Score)
  percent <- wins
  correct <- 0
  if(realGame$Home.Score > realGame$Visitor.Score && wins > .5)
  {
       correct <- 1
  }
  else if(realGame$Home.Score < realGame$Visitor.Score && wins < .5)
  {
    correct <- 1
    percent <- 1-wins
  }
  prediction[i,]$percent <- percent
  prediction[i,]$correct <- correct
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