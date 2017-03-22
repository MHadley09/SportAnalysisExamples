source("mle_ranking.R")

games <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


prediction <- data.frame(eloDiff=1:nrow(games), scoreDiff=1:nrow(games))

rankings <- calculate.rankings()

for(k in 1:nrow(games))
{
  realGame <- games[k,]
  homeElo <- rankings[rankings$Team == as.character(realGame$Home.Team),]$Score
  awayElo <- rankings[rankings$Team == as.character(realGame$Visitor.Team),]$Score
  if(!realGame$Neutral %in% c("N", "NE"))
  {
    winnerElo <- homeElo+70.82046
  }
  x <-   pmin(pmax(-21,  realGame$Home.Score - realGame$Visitor.Score),21)
  prediction[k,]$eloDiff <- 1/(1+10^((awayElo - homeElo)/400))
  prediction[k,]$scoreDiff <-   ((.5 - (.0000270209*x^3) + (0.0352489*x)))
}

plot(prediction)