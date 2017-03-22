source("mle_ranking.R")

games <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


prediction <- data.frame(id=1:nrow(games),percent=1:nrow(games), correct=1:nrow(games))

rankings <- calculate.rankings()

fit.func <- function(adv)
{
  
  gamechecker <- merge(games, data.frame(Home.Team=rankings$Team, Home.Value = rankings$Score), by="Home.Team")
  gamechecker <- merge(gamechecker, data.frame(Visitor.Team=rankings$Team, Visitor.Value = rankings$Score), by="Visitor.Team")
  gamechecker[!gamechecker$Neutral %in% c("N","NE"),]$Home.Value <- (1.031595*gamechecker[!gamechecker$Neutral %in% c("N","NE"),]$Home.Value)
  gamechecker$Home.Win <- 0
  gamechecker[gamechecker$Home.Score > gamechecker$Visitor.Score,]$Home.Win <- 1
  gamechecker$Visitor.Win <- 0
  gamechecker[gamechecker$Home.Score < gamechecker$Visitor.Score,]$Visitor.Win <- 1
  gamechecker$Home.Pred <- 0
  gamechecker$Visitor.Pred <- 0
  gamechecker[gamechecker$Home.Value > gamechecker$Visitor.Value,]$Home.Pred <- 1
  gamechecker[gamechecker$Home.Value < gamechecker$Visitor.Value,]$Visitor.Pred <- 1
  
  correct <- nrow(gamechecker[gamechecker$Home.Win == 1 & gamechecker$Home.Pred == 1,]) + nrow(gamechecker[gamechecker$Visitor.Win == 1 & gamechecker$Visitor.Pred == 1,])
  return(correct/nrow(gamechecker))
  
}

best <- optimize(f = fit.func, interval = c(0,2), maximum=T)
# 
#
# for(k in 1:nrow(games))
# {
#   realGame <- games[k,]
#   winnerElo <- 0
#   loserElo <- 0
#   if(realGame$Home.Score > realGame$Visitor.Score)
#   {
#     winnerElo <- rankings[rankings$Team == as.character(realGame$Home.Team),]$Score
#     loserElo <- rankings[rankings$Team == as.character(realGame$Visitor.Team),]$Score
#     if(!realGame$Neutral %in% c("N", "NE"))
#     {
#       winnerElo <- (1.041*winnerElo)+41.56543
#     }
#     
#     
#   } else  {
#     winnerElo <- rankings[rankings$Team == as.character(realGame$Visitor.Team),]$Score
#     loserElo <- rankings[rankings$Team == as.character(realGame$Home.Team),]$Score
#     
#     if(!realGame$Neutral %in% c("N", "NE"))
#     {
#       loserElo <- (1.041*loserElo)+41.56543
#     }
#     
#   }
#   if(winnerElo > loserElo){
#     correct <- 1
#     percent <- predict(winnerElo, loserElo)
#   }else{
#     percent <- predict(loserElo, winnerElo)
#     correct <- 0
#     
#   }
# 
#   prediction[k,]$percent <- percent
#   prediction[k,]$correct <- correct
# }
# 
# agg <- aggregate(cbind(correct)~percent, data = prediction, FUN=mean)
# 
# breakdown <- agg[1:10,]
# for(j in 1:10)
# {
#   per <- .5 + .05*j
#   breakdown[j,]$percent <- paste(per-.05, per, sep="-")
#   breakdown[j,]$correct<-mean(prediction$percent >= per-.05 & prediction$percent < per,]$correct)
# }
# breakdown2 <- agg[1:5,]
# for(i in 1:5)
# {
#   per <- .5 + .1*i
#   breakdown2[i,]$percent <- paste(per-.1, per, sep="-")
#   breakdown2[i,]$correct<-mean(prediction[prediction$percent >= per-.1 & prediction$percent < per,]$correct)
# }
# mean(prediction$percent)
# mean(prediction$correct)
