source('conferenceData.r')

schedules <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)


conferenceRecords <- Conference.Records()

teams <- conferenceRecords[conferenceRecords$Conference != "FCS",] 

records <- conferenceRecords

records$Value <- 0

records[records$Conference == "FCS",]$Value <- 0.4* (records[records$Conference == "FCS",]$OVERALL.W / (records[records$Conference == "FCS",]$OVERALL.W + records[records$Conference == "FCS",]$OVERALL.L))
  


scores <- data.frame(Team = recordsTemp$TEAM, Value = recordsTemp$RANK.SCORE)

scores <- rbind(scores, data.frame(Team = records[records$Conference == "FCS",]$TEAM, 
                                   Value = records[records$Conference == "FCS",]$Value))

scores$SoS <- 0

homeOffset <- 0.95
awayOffset <- 1.05



for(i in 1:length(teams$TEAM))
{  
  homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
  awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
  homeOppRecords <- records[match(homeGames$Visitor, records$TEAM),] 
  awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
  
  homeScores <- homeOffset*scores[match(homeOppRecords$TEAM,scores$Team),]$Value
  awayScores <- awayOffset*scores[match(awayOppRecords$TEAM,scores$Team),]$Value
  homeScores <- homeScores[!is.na(homeScores)]
  awayScores <- awayScores[!is.na(awayScores)]
  
  
  scores[scores$Team == as.character(teams[i,]$TEAM),]$SoS <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
}
scores <- scores[scores$SoS != 0,]

maxSoS <- max(scores$SoS)
minSoS <- min(scores$SoS)

SoS.Score <- (scores$SoS - minSoS) / (maxSoS-minSoS)

scores$SoS <- SoS.Score
scores$Ranking <- 1 + length(scores$SoS) - rank(scores$SoS)