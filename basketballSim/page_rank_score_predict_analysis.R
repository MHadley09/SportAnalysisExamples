source("page_rank.R")

games <- read.csv(file = 'data\\pastweek.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


rankings <- calculate.rankings()

gamechecker <- merge(games, data.frame(Home.Team=rankings$Team, Home.Value = rankings$score), by="Home.Team")
gamechecker <- merge(gamechecker, data.frame(Visitor.Team=rankings$Team, Visitor.Value = rankings$score), by="Visitor.Team")
gamechecker[!gamechecker$Neutral %in% c("N","NE"),]$Home.Value <- (1.27396*gamechecker[!gamechecker$Neutral %in% c("N","NE"),]$Home.Value)+0.2069521
gamechecker$Home.Win <- 0
gamechecker[gamechecker$Home.Score > gamechecker$Visitor.Score,]$Home.Win <- 1
gamechecker$Visitor.Win <- 0
gamechecker[gamechecker$Home.Score < gamechecker$Visitor.Score,]$Visitor.Win <- 1
gamechecker$Home.Pred <- 0
gamechecker$Visitor.Pred <- 0
gamechecker[gamechecker$Home.Value > gamechecker$Visitor.Value,]$Home.Pred <- 1
gamechecker[gamechecker$Home.Value < gamechecker$Visitor.Value,]$Visitor.Pred <- 1

correct <- nrow(gamechecker[gamechecker$Home.Win == 1 & gamechecker$Home.Pred == 1,]) + nrow(gamechecker[gamechecker$Visitor.Win == 1 & gamechecker$Visitor.Pred == 1,])
correct/nrow(gamechecker)

  gamechecker$predDiff <- 0

for(i in 1:nrow(gamechecker))
{
  scoreA <- gamechecker$Home.Value
  scoreB <- gamechecker$Visitor.Value
  gamechecker[i,]$predDiff <- predict(scoreA, scoreB)
}

gamechecker$scoreDiff <- gamechecker$Home.Score - gamechecker$Visitor.Score


prediction <- data.frame(predDiff = gamechecker$predDiff, scoreDiff=gamechecker$scoreDiff )
plot(prediction)
abline(lm(scoreDiff~predDiff, data=prediction), col="red")
summary(lm(scoreDiff~predDiff, data=prediction))

