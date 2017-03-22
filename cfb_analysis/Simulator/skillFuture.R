
played <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
remainingSchedule <- read.csv(file = 'data\\2016\\schedule.csv', sep=',', header=T)

names(remainingSchedule)[names(remainingSchedule)=="TeamA"] <- "Home.Team"
names(remainingSchedule)[names(remainingSchedule)=="TeamB"] <- "Visitor.Team"

remainingSchedule$Home.Score <- 0
remainingSchedule$Visitor.Score <- 0

count <- 250
source('play_by_play_predict_with_no_zero.R')


for(i in 1:(length(remainingSchedule$Home.Team)))
{
  hName <- as.character(remainingSchedule[i,]$Home.Team)
  aName <- as.character(remainingSchedule[i,]$Visitor.Team)
  
  gameResult <- neutral.predict(hName, aName, count)
  
  remainingSchedule[i,]$Home.Score <- gameResult[1,]$Score
  remainingSchedule[i,]$Visitor.Score <- gameResult[2,]$Score
}

schedule <- played

schedule <- rbind(schedule, remainingSchedule)

source('skill_likelihood_ranking_enhance.R')

save.rankings(schedule)
