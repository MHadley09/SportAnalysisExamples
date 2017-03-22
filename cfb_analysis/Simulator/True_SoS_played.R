expected.wins <- function(elo)
{
  return (1/(1+10^((elo-1600)/400))) 
}

All.SoS <- function(){
  source('max_likelihood_ranking.R')
  #records <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  schedules <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  rankings <- calculate.rankings()
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  schedules$Visitor.Team <- trim(schedules$Visitor.Team)
  schedules$Home.Team <- trim(schedules$Home.Team)
  
  
  conferences <- read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)
    
  records <- data.frame(Team=rankings$Team)
  records$Expected.Wins <-0
  records$Games <- 0
  
  
  for(i in 1:nrow(schedules))
  {
    game <- schedules[i,]
    teamA <- as.character(game$Home.Team)
    teamB <- as.character(game$Visitor.Team)
    
    records[records$Team == teamA,]$Expected.Wins <- (records[records$Team == teamA,]$Expected.Wins +
              expected.wins(rankings[rankings$Team == teamB,]$Score))
    records[records$Team == teamB,]$Expected.Wins <- (records[records$Team == teamB,]$Expected.Wins +
              expected.wins(rankings[rankings$Team == teamA,]$Score))
    
    records[records$Team == teamA,]$Games <- records[records$Team == teamA,]$Games+1
    records[records$Team == teamB,]$Games <- records[records$Team == teamB,]$Games+1
  }
  
  records$Average.Expected.Wins <-  records$Expected.Wins/ records$Games
  records$SoS.Rank <- rank(records$Average.Expected.Wins)
  return(records)
}
FBS.SoS <- function(){
  FBS.Team <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  
  Conferences <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)
  
  FBS.Team <- FBS.Team[FBS.Team$TEAM %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]
  
  SoS <- All.SoS()
  SoS <- SoS[SoS$Team %in% FBS.Team$TEAM ,]
  
  return(SoS)
}