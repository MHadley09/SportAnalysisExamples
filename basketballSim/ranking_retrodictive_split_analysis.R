source("mle_split_home_ranking.R")

games <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]



fit.func <- function(adv)
{
  rankings <- calculate.rankings()
  gamechecker <- merge(games, data.frame(Home.Team=rankings$Team, Home.Value = rankings$Home.Score, Neutral.Value = rankings$Score), by="Home.Team")
  gamechecker <- merge(gamechecker, data.frame(Visitor.Team=rankings$Team, Visitor.Value = rankings$Score), by="Visitor.Team")
  #gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Home.Value <- gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Neutral.Value
  gamechecker[gamechecker$Neutral %in% c("N","NE"),]$Home.Value <- gamechecker[gamechecker$Neutral %in% c("N","NE"),]$Neutral.Value  
  gamechecker$Home.Win <- 0
  gamechecker[gamechecker$Home.Score > gamechecker$Visitor.Score,]$Home.Win <- 1
  gamechecker$Visitor.Win <- 0
  gamechecker[gamechecker$Home.Score < gamechecker$Visitor.Score,]$Visitor.Win <- 1
  gamechecker$Home.Pred <- 0
  gamechecker$Visitor.Pred <- 0
  gamechecker[gamechecker$Home.Value > gamechecker$Visitor.Value,]$Home.Pred <- 1
  gamechecker[gamechecker$Home.Value < gamechecker$Visitor.Value,]$Visitor.Pred <- 1
  gamechecker$Winner.Prob <- predict(gamechecker$Home.Value,gamechecker$Visitor.Value)
  gamechecker$Home.Prob <- gamechecker$Winner.Prob 
  gamechecker[gamechecker$Visitor.Pred == 1,]$Winner.Prob <- predict(gamechecker[gamechecker$Visitor.Pred == 1,]$Visitor.Value,
                                                                   gamechecker[gamechecker$Visitor.Pred == 1,]$Home.Value)
  gamechecker$Predict.Diff <- 0

  gamechecker$correct <- 0
  gamechecker[((gamechecker$Home.Win == 1 & gamechecker$Home.Pred == 1) 
             | (gamechecker$Visitor.Win == 1 & gamechecker$Visitor.Pred == 1)),]$correct <- 1
  
  return(-1*mean((gamechecker$Home.Win*log(gamechecker$Home.Prob))+((1-gamechecker$Home.Win)*log(1-gamechecker$Home.Prob))))
}

best <- optimize(f = fit.func, interval = c(1,5), maximum=F)


  rankings <- calculate.rankings()
  gamechecker <- merge(games, data.frame(Home.Team=rankings$Team, Home.Value = rankings$Home.Score, Neutral.Value = rankings$Score), by="Home.Team")
  gamechecker <- merge(gamechecker, data.frame(Visitor.Team=rankings$Team, Visitor.Value = rankings$Score), by="Visitor.Team")
  #gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Home.Value <- gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Neutral.Value
  gamechecker[gamechecker$Neutral %in% c("N","NE"),]$Home.Value <- gamechecker[gamechecker$Neutral %in% c("N","NE"),]$Neutral.Value  
  gamechecker$Home.Win <- 0
  gamechecker[gamechecker$Home.Score > gamechecker$Visitor.Score,]$Home.Win <- 1
  gamechecker$Visitor.Win <- 0
  gamechecker[gamechecker$Home.Score < gamechecker$Visitor.Score,]$Visitor.Win <- 1
  gamechecker$Home.Pred <- 0
  gamechecker$Visitor.Pred <- 0
  gamechecker[gamechecker$Home.Value > gamechecker$Visitor.Value,]$Home.Pred <- 1
  gamechecker[gamechecker$Home.Value < gamechecker$Visitor.Value,]$Visitor.Pred <- 1
  gamechecker$Winner.Prob <- predict(gamechecker$Home.Value,gamechecker$Visitor.Value)
  gamechecker$Home.Prob <- gamechecker$Winner.Prob 
  gamechecker[gamechecker$Visitor.Pred == 1,]$Winner.Prob <- predict(gamechecker[gamechecker$Visitor.Pred == 1,]$Visitor.Value,
                                                                   gamechecker[gamechecker$Visitor.Pred == 1,]$Home.Value)
  gamechecker$Predict.Diff <- 0

  gamechecker$correct <- 0
  gamechecker[((gamechecker$Home.Win == 1 & gamechecker$Home.Pred == 1) 
             | (gamechecker$Visitor.Win == 1 & gamechecker$Visitor.Pred == 1)),]$correct <- 1

  val <- (-1*mean((gamechecker$Home.Win*log(gamechecker$Home.Prob))+((1-gamechecker$Home.Win)*log(1-gamechecker$Home.Prob))))
 