Py.Wins <- function()
{
  schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)

  Teams <- unique(c(as.character(schedule$Visitor.Team),as.character(schedule$Home.Team)))

  py.record <- data.frame(TEAM=Teams)

  py.record$Points.For <- 0
  py.record$Points.Against <- 0
  py.record$Py.Wins <- 0
  py.record$Py.Losses <- 0
  for(i in 1:nrow(schedule))
  {
     game <- schedule[i,]
  
     teamA <- as.character(game$Home.Team)
     teamB <- as.character(game$Visitor.Team)
  
     py.record[py.record$TEAM == teamA,]$Points.For <- py.record[py.record$TEAM == teamA,]$Points.For + game$Home.Score
     py.record[py.record$TEAM == teamA,]$Points.Against <- py.record[py.record$TEAM == teamA,]$Points.Against + game$Visitor.Score
  
     py.record[py.record$TEAM == teamB,]$Points.For <- py.record[py.record$TEAM == teamB,]$Points.For + game$Visitor.Score
     py.record[py.record$TEAM == teamB,]$Points.Against <- py.record[py.record$TEAM == teamB,]$Points.Against + game$Home.Score
  
     py.record[py.record$TEAM == teamA,]$Py.Wins<-  py.record[py.record$TEAM == teamA,]$Py.Wins + ((game$Home.Score^2.37)/((game$Home.Score^2.37)+(game$Visitor.Score^2.37)))
     py.record[py.record$TEAM == teamA,]$Py.Losses <- py.record[py.record$TEAM == teamA,]$Py.Losses + ((game$Visitor.Score^2.37)/((game$Home.Score^2.37)+(game$Visitor.Score^2.37)))
 
     py.record[py.record$TEAM == teamB,]$Py.Losses<-  py.record[py.record$TEAM == teamB,]$Py.Losses + ((game$Home.Score^2.37)/((game$Home.Score^2.37)+(game$Visitor.Score^2.37)))
     py.record[py.record$TEAM == teamB,]$Py.Wins <- py.record[py.record$TEAM == teamB,]$Py.Wins + ((game$Visitor.Score^2.37)/((game$Home.Score^2.37)+(game$Visitor.Score^2.37)))
     
  }
   return(py.record)
}