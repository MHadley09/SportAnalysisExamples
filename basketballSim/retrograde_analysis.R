source("bball_bell_predictor.r")

games <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games$Spread <- 0
prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))

for(k in 1:nrow(games))
{
  runs <- 251
  realGame <- games[k,]
  if(realGame$Neutral %in% c("N", "NE"))
  {
    predictedGame <- neutral.predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team), runs)
  }else  {
    predictedGame <- predict(as.character(realGame$Home.Team),as.character(realGame$Visitor.Team),  runs)
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

agg <- aggregate(cbind(correct)~percent, data = prediction, FUN=mean)

breakdown <- agg[1:10,]
for(j in 1:10)
{
  per <- .5 + .05*j
  breakdown[j,]$percent<-mean(prediction[prediction$percent >= per-.05 & prediction$percent < per,]$percent)
  breakdown[j,]$correct<-mean(prediction[prediction$percent >= per-.05 & prediction$percent < per,]$correct)
}
breakdown2 <- agg[1:5,]

breakdowncount <- breakdown2
breakdowncount$count <- 0
for(k in 1:5)
{
  per <- .5 + .1*k
  breakdown2[k,]$percent <- mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$percent)
  breakdown2[k,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
  
  breakdowncount[k,]$percent<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$percent)
  breakdowncount[k,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
  breakdowncount[k,]$count<-length(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
}

# breakdown3 <- agg[1:20,]
# for(j in 1:20)
# {
#   per <- .5 + .025*j
#   breakdown3[j,]$percent<-mean(prediction[prediction$percent >= per-.025 & prediction$percent < per,]$percent)
#   breakdown3[j,]$correct<-mean(prediction[prediction$percent >= per-.025 & prediction$percent < per,]$correct)
#   breakdowncount[j,]$percent<-mean(prediction[prediction$percent >= per-.025 & prediction$percent < per,]$percent)
#   breakdowncount[j,]$correct<-mean(prediction[prediction$percent >= per-.025 & prediction$percent < per,]$correct)
#   breakdowncount[j,]$count<-length(prediction[prediction$percent >= per-.025 & prediction$percent < per,]$correct)
# }
mean(prediction$percent)
mean(prediction$correct)

line <- lm(correct~percent,data=breakdown2)
plot(breakdown2)
abline(line,col="green")
summary(line,data=breakdown2)
