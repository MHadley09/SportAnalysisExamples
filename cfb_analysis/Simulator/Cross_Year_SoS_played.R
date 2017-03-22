All.SoS <- function(year){
  #source('conferenceData.r')
  records <- read.csv(file = paste0('data\\', year, '\\records.csv'), sep=',', header=T)
  schedules <- read.csv(file = 'data\\gamesPlayed.csv', sep=',', header=T)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  schedules$Visitor <- trim(schedules$Visitor)
  schedules$Home.Team <- trim(schedules$Home.Team)
 # conferenceRecords <- Conference.Records()
  
  teams <- records
  
#  records <- conferenceRecords
  

  Adjusted.SoS <- rep(0,length(records$TEAM))
  
  First.Pass.SoS <- rep(0,length(records$TEAM))
  
  records <- cbind(records, First.Pass.SoS)
  records <- cbind(records, Adjusted.SoS)
  
  homeOffset <- 1
  awayOffset <- 1
  
  for(i in 1:length(teams$TEAM))
  {  
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOffset*(homeOppRecords$OVERALL.W / (homeOppRecords$OVERALL.W+homeOppRecords$OVERALL.L))
    awayScores <- awayOffset*(awayOppRecords$OVERALL.W / (awayOppRecords$OVERALL.W+awayOppRecords$OVERALL.L))
    
    homeScores <- homeScores[!is.na(homeScores)]
    awayScores <- awayScores[!is.na(awayScores)]
    
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
  }
  
  records[is.na(records$First.Pass.SoS),]$First.Pass.SoS <- 0.1

  for(i in 1:length(teams$TEAM))
  {  
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOffset*homeOppRecords$First.Pass.SoS
    awayScores <- awayOffset*awayOppRecords$First.Pass.SoS
    
    homeScores <- homeScores[!is.na(homeScores)]
    awayScores <- awayScores[!is.na(awayScores)]
    
    secondPass <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$Adjusted.SoS<- ((2*records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS)+secondPass)/3
  }
  
  records[is.na(records$Adjusted.SoS),]$Adjusted.SoS <- 0.1  
  maxSoS <- max(records$Adjusted.SoS)
  minSoS <- min(records[records$Adjusted.SoS!=0.385,]$Adjusted.SoS)
  
  SoS.Score <- (records$Adjusted.SoS - minSoS) / (maxSoS-minSoS)
  
  records <- cbind(records, SoS.Score)
  
  SoS.Ranking <- length(teams$TEAM) - ((length(teams$TEAM)-1)*records$SoS.Score)
  
  return(records <- cbind(records, SoS.Ranking))
}
FBS.SoS <- function(year){
 
  return(All.SoS(year))

}