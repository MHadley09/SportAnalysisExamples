source("scoreRank.R")

games <- read.csv(file = 'data\\pastweek.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

games$Visitor.Team <- trim(games$Visitor.Team)
games$Home.Team <- trim(games$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

games <- games[games$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games <- games[games$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

games$Spread <- 0
rankings <- calculate.rankings()

prediction <- data.frame(predicted.Diff=1:nrow(games), true.Diff=1:nrow(games), correct=1:nrow(games))

for(k in 1:nrow(games))
{
  realGame <- games[k,]
  homeScore <- rankings[rankings$Team == as.character(realGame$Home.Team),]$Score
  awayScore <- rankings[rankings$Team == as.character(realGame$Visitor.Team),]$Score
  
  if(!(realGame$Neutral %in% c("N","NE")))
  {
    homeScore <- homeScore +  2.16387
  }  
  prediction[k,]$predicted.Diff <- homeScore - awayScore
  prediction[k,]$true.Diff <- realGame$Home.Score - realGame$Visitor.Score
  prediction[k,]$correct <- 0
  if(((realGame$Home.Score  + realGame$Spread) > realGame$Visitor.Score) & ((homeScore + realGame$Spread)>awayScore))
  {
    prediction[k,]$correct <- 1
  }
  if(((realGame$Home.Score  + realGame$Spread) < realGame$Visitor.Score) & ((homeScore + realGame$Spread)<awayScore))
  {
    prediction[k,]$correct <- 1
  }
}
mean(prediction$correct)