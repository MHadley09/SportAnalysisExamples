source("skill_likelihood_ranking.R")
#games <- read.csv(file = 'data\\2015\\lastweek.csv', sep=',', header=T)
games <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)

Conferences <- read.csv(file = 'data\\2015\\ConferenceWithFCS.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
games <- games[games$Visitor.Team %in% as.character(Conferences[Conferences$Conference!="FCS",]$TEAM),]
#games <- games[1:700,]
prediction <- data.frame(id=1:nrow(games),predicted=1:nrow(games), correct=1:nrow(games), margin=1:nrow(games))
elo <- calculate.FBS.Rankings()
for(i in 1:nrow(games))
{
  realGame <- games[i,]
  #  predictedGame <- predict(elo[elo$Team == as.character(realGame$Home.Team),]$Score,
  #                           elo[elo$Team == as.character(realGame$Visitor.Team),]$Score)
  ascore <- predict.score(elo[elo$Team == as.character(realGame$Home.Team),]$Offense,
                  elo[elo$Team == as.character(realGame$Visitor.Team),]$Defense)
  bscore <- predict.score(elo[elo$Team == as.character(realGame$Visitor.Team),]$Offense,
                    elo[elo$Team == as.character(realGame$Home.Team),]$Defense)
  percent <- 0
  correct <- 0
  margin <- 0
  if(realGame$Home.Score > realGame$Visitor.Score && ascore > bscore)
  {
    margin <- realGame$Home.Score - realGame$Visitor.Score 
    predicted <- ascore-bscore
    correct <- 1
  }
  if(realGame$Home.Score < realGame$Visitor.Score && ascore < bscore)
  {
    correct <- 1
    margin <- realGame$Visitor.Score  - realGame$Home.Score
    predicted <- bscore-ascore
  }
  if(realGame$Home.Score < realGame$Visitor.Score && ascore > bscore)
  {
    margin <- realGame$Visitor.Score  - realGame$Home.Score
    predicted <- ascore-bscore
  }
  if(realGame$Home.Score > realGame$Visitor.Score && ascore < bscore)
  {
    margin <- realGame$Home.Score - realGame$Visitor.Score 
    predicted <- bscore-ascore
  }
  prediction[i,]$predicted <- predicted
  prediction[i,]$correct <- correct
  prediction[i,]$margin <- margin
}

prediction4 <- prediction
agg <- aggregate(cbind(correct, margin)~predicted, data = prediction, FUN=mean)
# 
# breakdown7<- agg[1:10,]
# for(j in 1:10)
# {
#   per <- .5 + .05*j
#   breakdown7[j,]$percent <- paste(per-.05, per, sep="-")
#   breakdown7[j,]$correct<-mean(prediction[prediction$percent >= per-.05 & prediction$percent < per,]$correct)
# }
# breakdown8 <- agg[1:5,]
# for(k in 1:5)
# {
#   per <- .5 + .1*k
#   breakdown8[k,]$percent <- paste(per-.1, per, sep="-")
#   breakdown8[k,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
# }