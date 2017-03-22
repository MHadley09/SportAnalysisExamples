
source("scoreRank.R")

games <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


prediction <- data.frame(rankDiff=1:nrow(games), scoreDiff=1:nrow(games))

rankings <- calculate.rankings()

for(k in 1:nrow(games))
{
  realGame <- games[k,]
  
  homeScore <- rankings[rankings$Team == as.character(realGame$Home.Team),]$Score
  awayScore <- rankings[rankings$Team == as.character(realGame$Visitor.Team),]$Score
  
  if(!(realGame$Neutral %in% c("N","NE")))
  {
    homeScore <- homeScore +  2.949199
  }  
  
  
  prediction[k,]$rankDiff <- (homeScore - awayScore)
  prediction[k,]$scoreDiff <- realGame$Home.Score - realGame$Visitor.Score
}


plot(prediction)
c <- lm(scoreDiff~rankDiff, data=prediction)
u <- lm(scoreDiff+(.5*sd(prediction$scoreDiff))~rankDiff, data=prediction)
l <- lm(scoreDiff-(.5*sd(prediction$scoreDiff))~rankDiff, data=prediction)
abline(c, col="red")
abline(u, col="green")
abline(l, col="green")
summary(c)

upInt <- as.numeric(u$coefficients[1])
lowInt <- as.numeric(l$coefficients[1])
lowC <- as.numeric(l$coefficients[2])
upC <- as.numeric(u$coefficients[2])
set <- prediction[(prediction$scoreDiff < upInt + prediction$rankDiff*upC)& (prediction$scoreDiff > lowInt + prediction$rankDiff*lowC),]
plot(set)
abline(c, col="red")
abline(u,  col="green")
abline(l,  col="green")