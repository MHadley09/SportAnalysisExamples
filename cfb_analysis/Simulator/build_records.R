schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
conferences <- read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)

Teams <- unique(c(as.character(schedule$Visitor.Team),as.character(schedule$Home.Team)))

record <- data.frame(TEAM=Teams)

record$CONF.W <- 0
record$CONF.L <- 0
record$OVERALL.W <- 0
record$OVERALL.L <- 0
  
for(i in 1:nrow(schedule))
{
  game <- schedule[i,]
  winner <- as.character(game$Home.Team)
  loser <- as.character(game$Visitor.Team)
  if(game$Home.Score < game$Visitor.Score)
  {
    winner <- as.character(game$Visitor.Team)
    loser <- as.character(game$Home.Team)
  }
  record[record$TEAM == winner,]$OVERALL.W <- record[record$TEAM == winner,]$OVERALL.W + 1
  record[record$TEAM == loser,]$OVERALL.L <- record[record$TEAM == loser,]$OVERALL.L + 1
  confW <- paste0(as.character(conferences[conferences$TEAM == winner, ]$Conference)," ")
  confL <- paste0(as.character(conferences[conferences$TEAM == loser, ]$Conference)," ")
  if(confW == confL)
  {
    record[record$TEAM == winner,]$CONF.W <- record[record$TEAM == winner,]$CONF.W + 1
    record[record$TEAM == loser,]$CONF.L <- record[record$TEAM == loser,]$CONF.L + 1
  }
}

write.table(record,'data\\2016\\records.csv', row.names=FALSE, sep=",") 