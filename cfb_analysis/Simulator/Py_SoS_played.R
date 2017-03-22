All.SoS <- function(){
  source('conferenceData.r')
  #records <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  schedules <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  schedules$Visitor.Team <- trim(schedules$Visitor.Team)
  schedules$Home.Team <- trim(schedules$Home.Team)
  conferenceRecords <- Conference.Records()
  
  teams <- data.frame(TEAM=conferenceRecords$TEAM, ConferenceOffset = conferenceRecords$ConferenceOffset)
  
  schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  conferences <- read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)
  
  Teams <- unique(c(as.character(schedule$Visitor.Team),as.character(schedule$Home.Team)))
  
  records <- data.frame(TEAM=Teams)
  
  
  records$Points.For <- 0
  records$Points.Against <- 0
  records$Conf.Points.For <- 0
  records$Conf.Points.Against <- 0
  for(i in 1:nrow(schedule))
  {
    game <- schedule[i,]
    teamA <- as.character(game$Home.Team)
    teamB <- as.character(game$Visitor.Team)
    
    records[records$TEAM == teamA,]$Points.For <- records[records$TEAM == teamA,]$Points.For + game$Home.Score
    records[records$TEAM == teamA,]$Points.Against <- records[records$TEAM == teamA,]$Points.Against + game$Visitor.Score
    
    records[records$TEAM == teamB,]$Points.For <- records[records$TEAM == teamB,]$Points.For + game$Visitor.Score
    records[records$TEAM == teamB,]$Points.Against <- records[records$TEAM == teamB,]$Points.Against + game$Home.Score
    
    confA <- paste0(as.character(conferences[conferences$TEAM == teamA, ]$Conference)," ")
    confB <- paste0(as.character(conferences[conferences$TEAM == teamB, ]$Conference)," ")
    
    if(confA == confB)
    {
      records[records$TEAM == teamA,]$Conf.Points.For <- records[records$TEAM == teamA,]$Conf.Points.For + game$Home.Score
      records[records$TEAM == teamA,]$Conf.Points.Against <- records[records$TEAM == teamA,]$Conf.Points.Against + game$Visitor.Score
      
      records[records$TEAM == teamB,]$Conf.Points.For <- records[records$TEAM == teamB,]$Conf.Points.For + game$Visitor.Score
      records[records$TEAM == teamB,]$Conf.Points.Against <- records[records$TEAM == teamB,]$Conf.Points.Against + game$Home.Score
    }
  }
  
  records$CONF.W <- (records$Conf.Points.For^2.37)/((records$Conf.Points.For^2.37)+(records$Conf.Points.Against^2.37))
  records$CONF.L <- (records$Conf.Points.Against^2.37)/((records$Conf.Points.For^2.37)+(records$Conf.Points.Against^2.37))
  records$OVERALL.W <- (records$Points.For^2.37)/((records$Points.For^2.37)+(records$Points.Against^2.37))
  records$OVERALL.L <- (records$Points.Against^2.37)/((records$Points.For^2.37)+(records$Points.Against^2.37))
  
  
  records <- merge(records, teams, by="TEAM", all=T)
  
  records[is.na(records)] <- .75
  
  records$Adjusted.SoS <- 0
  
  records$First.Pass.SoS <- 0
  
  homeOffset <- 1
  awayOffset <- 1
  
  for(i in 1:length(teams$TEAM))
  {  
    
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor.Team == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor.Team, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOppRecords$ConferenceOffset*homeOffset*(homeOppRecords$OVERALL.W / (homeOppRecords$OVERALL.W+homeOppRecords$OVERALL.L))
    
    awayScores <- awayOppRecords$ConferenceOffset*awayOffset*(awayOppRecords$OVERALL.W / (awayOppRecords$OVERALL.W+awayOppRecords$OVERALL.L))
    
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
  }
  
  
  for(i in 1:length(teams$TEAM))
  {  
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor.Team == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor.Team, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOppRecords$ConferenceOffset*homeOffset*homeOppRecords$First.Pass.SoS
    awayScores <- awayOppRecords$ConferenceOffset*awayOffset*awayOppRecords$First.Pass.SoS
    
    homeScores <- homeScores[!is.na(homeScores)]
    awayScores <- awayScores[!is.na(awayScores)]
    
    secondPass <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$Adjusted.SoS<- ((2*records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS)+secondPass)/3
  }
  
  records[is.na(records )]<- .385
  
  maxSoS <- max(records$Adjusted.SoS)
  minSoS <- min(records[records$Adjusted.SoS!=0.385,]$Adjusted.SoS)
  
  SoS.Score <- (records$Adjusted.SoS - minSoS) / (maxSoS-minSoS)
  
  records <- cbind(records, SoS.Score)
  
  SoS.Ranking <- length(teams$TEAM) - ((length(teams$TEAM)-1)*records$SoS.Score)
  
  return(records <- cbind(records, SoS.Ranking))
}
FBS.SoS <- function(){
  
  FBS.Team <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  
  Conferences <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)
  
  FBS.Team <- FBS.Team[FBS.Team$TEAM %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]
  
  SoS <- All.SoS()
  SoS <- SoS[SoS$TEAM %in% FBS.Team$TEAM ,]
  SoS$SoS.Ranking <- NULL
  SoS$SoS.Score <- NULL
  maxSoS <- max(SoS$Adjusted.SoS)
  minSoS <- min(SoS[SoS$Adjusted.SoS!=0.385,]$Adjusted.SoS)
  SoS.Score <- (SoS$Adjusted.SoS - minSoS) / (maxSoS-minSoS)
  SoS <- cbind(SoS, SoS.Score)
  SoS.Ranking <- length(SoS$TEAM) - rank(SoS$SoS.Score) + 1
  SoS <- cbind(SoS, SoS.Ranking)
  
  return(SoS)
}