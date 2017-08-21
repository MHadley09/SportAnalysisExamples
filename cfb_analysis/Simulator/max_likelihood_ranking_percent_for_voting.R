game.value <- function(scoreA, scoreB)
{
  return(scoreA/100)
}

# game.value <- function(scoreA, scoreB)
# {
#   return ((scoreA^2.37)/((scoreA^2.37)+(scoreB^2.37)))
# }
predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function(schedule)
{

#  divisions <-read.csv(file = 'data\\2016\\divisionBreakdown.csv', sep=',', header=T)
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                   Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)

  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                  Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  last.step.score <- rep(1600, length(teams))
  score <- rep(1600, length(teams))
  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  

  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.step.score,score)
  for(j in 1:3)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
        gameScores <- game.value(scores, oppScores)
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }

  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

calculate.wins <- function(schedule)
{
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score/100)
  
  data <- rbind(data, data.frame(Team = schedule$Visitor.Team, Score = schedule$Visitor.Score/100))
  
  schedule$Actual.Wins <- 1
  
  expected <- aggregate(Score~Team, data, sum)
  
  colnames(expected)[2]="Expected.Wins"
  actual <- aggregate(Actual.Wins~Winner, schedule, sum)

  combined <- merge(x = expected, y=actual, by.x="Team", by.y="Winner", all.x=T)
  combined[is.na(combined)] <- 0
  combined$Luck <- combined$Actual.Wins - combined$Expected.Wins
  
  return(combined)
}
calculate.FBS.Rankings <- function()
{
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  schedule <- read.csv(file = 'data\\votes.csv', sep=',', header=T)
  colnames(schedule)[1]="Visitor.Team"

  schedule$Visitor.Team <- trim(schedule$Visitor.Team)
  schedule$Home.Team <- trim(schedule$Home.Team)
  schedule$Winner <- trim(schedule$Winner)

  
  wins <- calculate.wins(schedule)
  rankings <- calculate.rankings(schedule)
  FBS <-  combined <- merge(x = rankings, y=wins, by="Team")

  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(FBS, fname, row.names=FALSE, sep=",") 
  
  return(FBS)
}
