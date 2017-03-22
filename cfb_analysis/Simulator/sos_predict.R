neutral.predict <- function(homeName, awayName, runCount){
  
  source('SoS_played.r')
  SoS <- All.SoS()
  offense <- read.csv(file = 'data\\offense.csv', sep=',', header=T)
  defense <- read.csv(file = 'data\\defense.csv', sep=',', header=T)
  records <- read.csv(file = 'data\\records.csv', sep=',', header=T)
  box.scores <- read.csv(file = 'data\\box_data.csv', sep=',', header=T)
  
  averageSoS <- sum(SoS$Adjusted.SoS)/length(SoS$Adjusted.SoS)
  
  combine <- merge(offense, defense, by="TEAM")
  combine <- merge(combine, records, by="TEAM")
  average.YDS.G <- sum(offense$YDS.G)/length(offense$YDS.G)
  deviation.YDS.G <- sd(offense$YDS.G)
  average.D.YDS.G <- sum(defense$D.YDS.G)/length(defense$D.YDS.G)
  deviation.D.YDS.G <- sd(defense$D.YDS.G)
  
  home <- combine[combine$TEAM == homeName,]
  away <- combine[combine$TEAM == awayName,]
  
  homeOpps <- box.scores[box.scores$TEAM == homeName,]
  awayOpps <- box.scores[box.scores$TEAM == awayName,]
  
  
  homePassOadj <- 0
  homePassDadj <- 0
  homeRunOadj <- 0
  homeRunDadj <- 0
  homePFadj <- 0
  homePAadj <- 0
  
  homeTurnoverForAve <- sum(homeOpps$TURNOVERS.FOR)/length(homeOpps$TURNOVERS.FOR)                      
  homeTurnoverForSd <- sd(homeOpps$TURNOVERS.FOR)
  
  homeTurnoverAgainstAve <- sum(homeOpps$TURNOVERS.AGAINST)/length(homeOpps$TURNOVERS.AGAINST)
  homeTurnoverAgainstSd <- sd(homeOpps$TURNOVERS.AGAINST)
  
  homePenaltyAgainstAve <- sum(homeOpps$PENALTY.YARDS.AGAINST)/length(homeOpps$PENALTY.YARDS.AGAINST)
  homePenaltyAgainstSd  <- sd(homeOpps$PENALTY.YARDS.AGAINST)
  
  awayPassOadj <- 0
  awayPassDadj <- 0
  awayRunOadj <- 0
  awayRunDadj <- 0
  awayPFadj <- 0
  awayPAadj <- 0
  
  awayTurnoverForAve <- sum(awayOpps$TURNOVERS.FOR)/length(awayOpps$TURNOVERS.FOR)                      
  awayTurnoverForSd <- sd(awayOpps$TURNOVERS.FOR)
  
  awayTurnoverAgainstAve <- sum(awayOpps$TURNOVERS.AGAINST)/length(awayOpps$TURNOVERS.AGAINST)
  awayTurnoverAgainstSd <- sd(awayOpps$TURNOVERS.AGAINST)
  
  awayPenaltyAgainstAve <- sum(awayOpps$PENALTY.YARDS.AGAINST)/length(awayOpps$PENALTY.YARDS.AGAINST)
  awayPenaltyAgainstSd  <- sd(awayOpps$PENALTY.YARDS.AGAINST)
  
  homePassOoff <- c(1:length(homeOpps$OPP))
  homePassDoff <- c(1:length(homeOpps$OPP))
  homeRunOoff <- c(1:length(homeOpps$OPP))
  homeRunDoff <- c(1:length(homeOpps$OPP))
  homePFoff <- c(1:length(homeOpps$OPP))
  homePAoff <- c(1:length(homeOpps$OPP))
  
  homePassOYards.Past <- c(1:length(homeOpps$OPP))
  homePassDYards.Past <- c(1:length(homeOpps$OPP))
  homeRunOYards.Past <- c(1:length(homeOpps$OPP))
  homeRunDYards.Past <- c(1:length(homeOpps$OPP))
  homePF.Past <- c(1:length(homeOpps$OPP))
  homePA.Past <- c(1:length(homeOpps$OPP))
  
  awayPassOoff <- c(1:length(awayOpps$OPP))
  awayPassDoff <- c(1:length(awayOpps$OPP))
  awayRunOoff <- c(1:length(awayOpps$OPP))
  awayRunDoff <- c(1:length(awayOpps$OPP))
  awayPFoff <- c(1:length(awayOpps$OPP))
  awayPAoff <- c(1:length(awayOpps$OPP))
  
  
  awayPassOYards.Past <- c(1:length(awayOpps$OPP))
  awayPassDYards.Past <- c(1:length(awayOpps$OPP))
  awayRunOYards.Past <- c(1:length(awayOpps$OPP))
  awayRunDYards.Past <- c(1:length(awayOpps$OPP))
  awayPF.Past <- c(1:length(awayOpps$OPP))
  awayPA.Past <- c(1:length(awayOpps$OPP))
  
  for(i in 1:length(homeOpps$OPP))
  {
    
    oppName <- as.character(homeOpps[i,]$OPP)
    
    OSoSoff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSoff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    opp <- combine[combine$TEAM == oppName,]
    homePassOoff[i] <- OSoSoff*((homeOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    homePassDoff[i] <- DSoSoff*(homeOpps[i,]$P.YARDS.AGAINST)/(opp$P.YDS.G)
    homeRunOoff[i] <- OSoSoff*(homeOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    homeRunDoff[i] <- DSoSoff*(homeOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
    homePFoff[i] <- OSoSoff*(homeOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    homePAoff[i] <- DSoSoff*(homeOpps[i,]$POINTS.AGAINST)/opp$PTS.G
    
    homePassOYards.Past[i] <-  OSoSoff*homeOpps[i,]$P.YARDS.FOR
    homePassDYards.Past[i] <- DSoSoff*homeOpps[i,]$P.YARDS.AGAINST
    homeRunOYards.Past[i] <- OSoSoff*homeOpps[i,]$R.YARDS.F
    homeRunDYards.Past[i] <- DSoSoff*homeOpps[i,]$R.YARDS.AGAINST
    homePF.Past[i] <-  OSoSoff*homeOpps[i,]$POINTS.FOR
    homePA.Past[i] <- DSoSoff*homeOpps[i,]$POINTS.AGAINST
    
    
  }
  
  
  for(i in 1:length(awayOpps$OPP))
  {
    oppName <- as.character(awayOpps[i,]$OPP)
    opp <- combine[combine$TEAM == oppName,]
    
    OSoSoff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSoff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    awayPassOoff[i] <-  OSoSoff*(awayOpps[i,]$P.YARDS.FOR)/opp$D.P.YDS.G
    awayPassDoff[i] <-  DSoSoff*(awayOpps[i,]$P.YARDS.AGAINST)/opp$P.YDS.G
    awayRunOoff[i] <-  OSoSoff*(awayOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    awayRunDoff[i] <-  DSoSoff*(awayOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
    awayPFoff[i] <-  OSoSoff*(awayOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    awayPAoff[i] <-  DSoSoff*(awayOpps[i,]$POINTS.AGAINST)/opp$PTS.G
    
    awayPassOYards.Past[i] <-  OSoSoff*awayOpps[i,]$P.YARDS.FOR
    awayPassDYards.Past[i] <- DSoSoff*awayOpps[i,]$P.YARDS.AGAINST
    awayRunOYards.Past[i] <-  OSoSoff*awayOpps[i,]$R.YARDS.F
    awayRunDYards.Past[i] <- DSoSoff*awayOpps[i,]$R.YARDS.AGAINST
    awayPF.Past[i] <-  OSoSoff*awayOpps[i,]$POINTS.FOR
    awayPA.Past[i] <- DSoSoff*awayOpps[i,]$POINTS.AGAINST
  }
  
  homePassOadj <- sum(homePassOoff)/length(homePassOoff)
  homePassDadj <-  sum(homePassDoff)/length(homePassDoff)
  homeRunOadj <-  sum(homeRunOoff)/length(homeRunOoff)
  homeRunDadj <-  sum(homeRunDoff)/length(homeRunDoff)
  homePFadj <-  sum(homePFoff)/length(homePFoff)
  homePAadj <-  sum(homePAoff)/length(homePAoff)
  
  homePassOsd <- sd(homePassOoff)
  homePassDsd <-  sd(homePassDoff)
  homeRunOsd <-  sd(homeRunOoff)
  homeRunDsd <-  sd(homeRunDoff)
  homePFsd <-  sd(homePFoff)
  homePAsd <-  sd(homePAoff)
  
  awayPassOadj <- sum(awayPassOoff)/length(awayPassOoff)
  awayPassDadj <-  sum(awayPassDoff)/length(awayPassDoff)
  awayRunOadj <-  sum(awayRunOoff)/length(awayRunOoff)
  awayRunDadj <-  sum(awayRunDoff)/length(awayRunDoff)
  awayPFadj <-  sum(awayPFoff)/length(awayPFoff)
  awayPAadj <-  sum(awayPAoff)/length(awayPAoff)
  
  
  awayPassOsd <- sd(awayPassOoff)
  awayPassDsd <-  sd(awayPassDoff)
  awayRunOsd <-  sd(awayRunOoff)
  awayRunDsd <-  sd(awayRunDoff)
  awayPFsd <-  sd(awayPFoff)
  awayPAsd <-  sd(awayPAoff)
  
  
  homeSoSoffset <- sqrt(SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS /  SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS)
  awaySoSoffset <- sqrt(SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS / SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS)
  
  
  homePassO <- rnorm(n=runCount, mean=homePassOadj, sd=homePassOsd)
  homePassD <-rnorm(n=runCount, mean=homePassDadj, sd=homePassDsd)
  awayPassO <-rnorm(n=runCount, mean=awayPassOadj, sd=awayPassOsd)
  awayPassD <-rnorm(n=runCount, mean=awayPassDadj, sd=awayPassDsd)
  
  homeRunO <- rnorm(n=runCount, mean=homeRunOadj, sd=homeRunOsd)
  homeRunD <-rnorm(n=runCount, mean=homeRunDadj, sd=homeRunDsd)
  awayRunO <-rnorm(n=runCount, mean=awayRunOadj, sd=awayRunOsd)
  awayRunD <-rnorm(n=runCount, mean=awayRunDadj, sd=awayRunDsd)
  
  homePassOYards<- rnorm(n=runCount, mean=mean(homePassOYards.Past), sd=sd(homePassOYards.Past))
  homePassDYards <- rnorm(n=runCount, mean=mean(homePassDYards.Past), sd=sd(homePassDYards.Past))
  awayPassOYards<- rnorm(n=runCount, mean=mean(awayPassOYards.Past), sd=sd(awayPassOYards.Past))
  awayPassDYards <- rnorm(n=runCount, mean=mean(awayPassDYards.Past), sd=sd(awayPassDYards.Past))
  
  homeRunOYards <- rnorm(n=runCount, mean=mean(homeRunOYards.Past), sd=sd(homeRunOYards.Past))
  homeRunDYards <- rnorm(n=runCount, mean=mean(homeRunDYards.Past), sd=sd(homeRunDYards.Past))
  awayRunOYards <- rnorm(n=runCount, mean=mean(awayRunOYards.Past), sd=sd(awayRunOYards.Past))
  awayRunDYards <- rnorm(n=runCount, mean=mean(awayRunDYards.Past), sd=sd(awayRunDYards.Past))
  
  homePointsForList <-  rnorm(n=runCount, mean=mean(homePF.Past), sd=sd(homePF.Past))
  homePointsAgainstList <- rnorm(n=runCount, mean=mean(homePA.Past), sd=sd(homePA.Past))
  
  awayPointsForList <-  rnorm(n=runCount, mean=mean(awayPF.Past), sd=sd(awayPF.Past))
  awayPointsAgainstList <- rnorm(n=runCount, mean=mean(awayPA.Past), sd=sd(awayPA.Past))
  
  homePenalty <- rnorm(n=runCount, mean=homePenaltyAgainstAve, sd=homePenaltyAgainstSd)
  awayPenalty <-rnorm(n=runCount, mean=awayPenaltyAgainstAve, sd=awayPenaltyAgainstSd)
  
  homeTurnoverAgainst <- rnorm(n=runCount, mean=homeTurnoverAgainstAve, sd=homeTurnoverAgainstSd)
  awayTurnoverAgainst <-rnorm(n=runCount, mean=awayTurnoverAgainstAve, sd=awayTurnoverAgainstSd)
  
  homeTurnoverFor <- rnorm(n=runCount, mean=homeTurnoverForAve, sd=homeTurnoverForSd)
  awayTurnoverFor <-rnorm(n=runCount, mean=awayTurnoverForAve, sd=awayTurnoverForSd)
  
  homePF <- rnorm(n=runCount, mean=homePFadj, sd=homePFsd)
  awayPF <- rnorm(n=runCount, mean=awayPFadj, sd=awayPFsd)
  
  homePA <- rnorm(n=runCount, mean=homePAadj, sd=homePAsd)
  awayPA <- rnorm(n=runCount, mean=awayPAadj, sd=awayPAsd)
  
  homePass <- c(1:runCount)
  awayPass <- c(1:runCount)
  
  homeRun <- c(1:runCount)
  awayRun <- c(1:runCount)
  
  homeScore <- c(1:runCount)
  awayScore <- c(1:runCount)
  
  homeTurnover <- c(1:runCount)
  awayTurnover <- c(1:runCount)
  
  for(j in 1:runCount){
    
    homeExpectedPass <- c(homeSoSoffset*homePassO[j]*awayPassDYards[j], homeSoSoffset*awayPassD[j]*homePassOYards[j])
    homeExpectedRun <- c(awaySoSoffset*homeRunO[j]*awayRunDYards[j], awaySoSoffset*awayRunD[j]*homeRunOYards[j])
    
    awayExpectedPass <- c(homeSoSoffset*homePassD[j]*awayPassOYards[j], homeSoSoffset*awayPassO[j]*homePassDYards[j])
    awayExpectedRun <- c(awaySoSoffset*homeRunD[j]*awayRunOYards[j], awaySoSoffset*awayRunO[j]*homeRunDYards[j])
    
    homePointsFor <- c(homeSoSoffset*homePF[j]*awayPointsAgainstList[j], homeSoSoffset*awayPA[j]*homePointsForList[j])
    awayPointsFor <- c(awaySoSoffset*homePA[j]*awayPointsForList[j], awaySoSoffset*awayPF[j]*homePointsAgainstList[j])
    
    homeExpectedPass[3] <- ((1.1*homeExpectedPass[1])+(0.9*homeExpectedPass[2]))/2 
    homeExpectedRun[3] <- ((1.1*homeExpectedRun[1])+(0.9*homeExpectedRun[2]))/2 
    
    awayExpectedPass[3] <- ((0.9*awayExpectedPass[1])+(1.1*awayExpectedPass[2]))/2 
    awayExpectedRun[3] <- ((0.9*awayExpectedRun[1])+(1.1*awayExpectedRun[2]))/2 
    
    homePointsFor[3] <- ((1.1*homePointsFor[1])+(0.9*homePointsFor[2]))/2 
    awayPointsFor[3] <- ((0.9*awayPointsFor[1])+(1.1*awayPointsFor[2]))/2 
    
    homeTurnovers <- (0.7*awayTurnoverFor[j]+1.3*homeTurnoverAgainst[j])/2
    awayTurnovers <- (0.7*homeTurnoverFor[j]+1.3*awayTurnoverAgainst[j])/2
    
    turnoverMargin <- homeTurnover[j] - awayTurnover[j]
    
    penaltyMargin <-  awayPenalty[j] - homePenalty[j] 
    
    yardageMargin <- (homeExpectedRun[3]+homeExpectedPass[3])-(awayExpectedPass[3]+awayExpectedRun[3])
    
    homePointsFor[4] <- (1.75*(yardageMargin/100.0))+(-1.75*turnoverMargin)+homePointsFor[3]
    awayPointsFor[4] <- (-1.75*(yardageMargin/100.0))+(1.75*turnoverMargin)+awayPointsFor[3]
    
    homePass[j] <- homeExpectedPass[3]
    awayPass[j] <- awayExpectedPass[3]
    
    homeRun[j] <- homeExpectedRun[3]
    awayRun[j] <- awayExpectedRun[3]
    
    
    homePenalty[j] <- 0.95*homePenalty[j]
    awayPenalty[j] <- 1.05*awayPenalty[j] 
    
    homeTurnover[j] <- homeTurnovers
    awayTurnover[j] <-awayTurnovers
    
    homeScore[j] <- homePointsFor[4]
    awayScore[j] <- awayPointsFor[4]
  }
  
  TEAM <- c(homeName, awayName)
  PASS <- c(mean(homePass), mean(awayPass))
  RUN <- c(mean(homeRun), mean(awayRun))
  PENALTY <- c(mean(homePenalty), mean(awayPenalty))
  TURNOVERS <- c(mean(homeTurnover), mean(awayTurnover))
  SCORE <- c(mean(homeScore), mean(awayScore))
  
  WINS <- c(length(homeScore[homeScore > awayScore]), length(homeScore[homeScore < awayScore]))
  return(Box.Score <- data.frame(TEAM, PASS, RUN, PENALTY, TURNOVERS, SCORE, WINS))
}

predict <- function(homeName, awayName, runCount){
  
  
  source('SoS_played.r')
  SoS <- All.SoS()
  offense <- read.csv(file = 'data\\offense.csv', sep=',', header=T)
  defense <- read.csv(file = 'data\\defense.csv', sep=',', header=T)
  records <- read.csv(file = 'data\\records.csv', sep=',', header=T)
  box.scores <- read.csv(file = 'data\\box_data.csv', sep=',', header=T)
  
  
  averageSoS <- sum(SoS$Adjusted.SoS)/length(SoS$Adjusted.SoS)
  
  combine <- merge(offense, defense, by="TEAM")
  combine <- merge(combine, records, by="TEAM")
  average.YDS.G <- sum(offense$YDS.G)/length(offense$YDS.G)
  deviation.YDS.G <- sd(offense$YDS.G)
  average.D.YDS.G <- sum(defense$D.YDS.G)/length(defense$D.YDS.G)
  deviation.D.YDS.G <- sd(defense$D.YDS.G)
  
  home <- combine[combine$TEAM == homeName,]
  away <- combine[combine$TEAM == awayName,]
  
  homeOpps <- box.scores[box.scores$TEAM == homeName,]
  awayOpps <- box.scores[box.scores$TEAM == awayName,]
  
  
  homePassOadj <- 0
  homePassDadj <- 0
  homeRunOadj <- 0
  homeRunDadj <- 0
  homePFadj <- 0
  homePAadj <- 0
  
  homeTurnoverForAve <- sum(homeOpps$TURNOVERS.FOR)/length(homeOpps$TURNOVERS.FOR)                      
  homeTurnoverForSd <- sd(homeOpps$TURNOVERS.FOR)
  
  homeTurnoverAgainstAve <- sum(homeOpps$TURNOVERS.AGAINST)/length(homeOpps$TURNOVERS.AGAINST)
  homeTurnoverAgainstSd <- sd(homeOpps$TURNOVERS.AGAINST)
  
  homePenaltyAgainstAve <- sum(homeOpps$PENALTY.YARDS.AGAINST)/length(homeOpps$PENALTY.YARDS.AGAINST)
  homePenaltyAgainstSd  <- sd(homeOpps$PENALTY.YARDS.AGAINST)
  
  awayPassOadj <- 0
  awayPassDadj <- 0
  awayRunOadj <- 0
  awayRunDadj <- 0
  awayPFadj <- 0
  awayPAadj <- 0
  
  awayTurnoverForAve <- sum(awayOpps$TURNOVERS.FOR)/length(awayOpps$TURNOVERS.FOR)                      
  awayTurnoverForSd <- sd(awayOpps$TURNOVERS.FOR)
  
  awayTurnoverAgainstAve <- sum(awayOpps$TURNOVERS.AGAINST)/length(awayOpps$TURNOVERS.AGAINST)
  awayTurnoverAgainstSd <- sd(awayOpps$TURNOVERS.AGAINST)
  
  awayPenaltyAgainstAve <- sum(awayOpps$PENALTY.YARDS.AGAINST)/length(awayOpps$PENALTY.YARDS.AGAINST)
  awayPenaltyAgainstSd  <- sd(awayOpps$PENALTY.YARDS.AGAINST)
  
  homePassOoff <- c(1:length(homeOpps$OPP))
  homePassDoff <- c(1:length(homeOpps$OPP))
  homeRunOoff <- c(1:length(homeOpps$OPP))
  homeRunDoff <- c(1:length(homeOpps$OPP))
  homePFoff <- c(1:length(homeOpps$OPP))
  homePAoff <- c(1:length(homeOpps$OPP))
  
  homePassOYards.Past <- c(1:length(homeOpps$OPP))
  homePassDYards.Past <- c(1:length(homeOpps$OPP))
  homeRunOYards.Past <- c(1:length(homeOpps$OPP))
  homeRunDYards.Past <- c(1:length(homeOpps$OPP))
  homePF.Past <- c(1:length(homeOpps$OPP))
  homePA.Past <- c(1:length(homeOpps$OPP))
  
  awayPassOoff <- c(1:length(awayOpps$OPP))
  awayPassDoff <- c(1:length(awayOpps$OPP))
  awayRunOoff <- c(1:length(awayOpps$OPP))
  awayRunDoff <- c(1:length(awayOpps$OPP))
  awayPFoff <- c(1:length(awayOpps$OPP))
  awayPAoff <- c(1:length(awayOpps$OPP))
  
  
  awayPassOYards.Past <- c(1:length(awayOpps$OPP))
  awayPassDYards.Past <- c(1:length(awayOpps$OPP))
  awayRunOYards.Past <- c(1:length(awayOpps$OPP))
  awayRunDYards.Past <- c(1:length(awayOpps$OPP))
  awayPF.Past <- c(1:length(awayOpps$OPP))
  awayPA.Past <- c(1:length(awayOpps$OPP))
  
  for(i in 1:length(homeOpps$OPP))
  {
    
    oppName <- as.character(homeOpps[i,]$OPP)
    
    OSoSoff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSoff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    opp <- combine[combine$TEAM == oppName,]
    homePassOoff[i] <- OSoSoff*((homeOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    homePassDoff[i] <- DSoSoff*(homeOpps[i,]$P.YARDS.AGAINST)/(opp$P.YDS.G)
    homeRunOoff[i] <- OSoSoff*(homeOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    homeRunDoff[i] <- DSoSoff*(homeOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
    homePFoff[i] <- OSoSoff*(homeOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    homePAoff[i] <- DSoSoff*(homeOpps[i,]$POINTS.AGAINST)/opp$PTS.G
    
    homePassOYards.Past[i] <-  OSoSoff*homeOpps[i,]$P.YARDS.FOR
    homePassDYards.Past[i] <- DSoSoff*homeOpps[i,]$P.YARDS.AGAINST
    homeRunOYards.Past[i] <-  OSoSoff*homeOpps[i,]$R.YARDS.F
    homeRunDYards.Past[i] <- DSoSoff*homeOpps[i,]$R.YARDS.AGAINST
    homePF.Past[i] <-  OSoSoff*homeOpps[i,]$POINTS.FOR
    homePA.Past[i] <- DSoSoff*homeOpps[i,]$POINTS.AGAINST
    
    
  }
  
  
  for(i in 1:length(awayOpps$OPP))
  {
    oppName <- as.character(awayOpps[i,]$OPP)
    opp <- combine[combine$TEAM == oppName,]
    
    OSoSoff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSoff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    awayPassOoff[i] <-  OSoSoff*(awayOpps[i,]$P.YARDS.FOR)/opp$D.P.YDS.G
    awayPassDoff[i] <-  DSoSoff*(awayOpps[i,]$P.YARDS.AGAINST)/opp$P.YDS.G
    awayRunOoff[i] <-  OSoSoff*(awayOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    awayRunDoff[i] <-  DSoSoff*(awayOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
    awayPFoff[i] <-  OSoSoff*(awayOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    awayPAoff[i] <-  DSoSoff*(awayOpps[i,]$POINTS.AGAINST)/opp$PTS.G
    
    awayPassOYards.Past[i] <- OSoSoff*awayOpps[i,]$P.YARDS.FOR
    awayPassDYards.Past[i] <- DSoSoff*awayOpps[i,]$P.YARDS.AGAINST
    awayRunOYards.Past[i] <- OSoSoff*awayOpps[i,]$R.YARDS.F
    awayRunDYards.Past[i] <- DSoSoff*awayOpps[i,]$R.YARDS.AGAINST
    awayPF.Past[i] <- OSoSoff*awayOpps[i,]$POINTS.FOR
    awayPA.Past[i] <- DSoSoff*awayOpps[i,]$POINTS.AGAINST
  }
  
  homePassOadj <- sum(homePassOoff)/length(homePassOoff)
  homePassDadj <-  sum(homePassDoff)/length(homePassDoff)
  homeRunOadj <-  sum(homeRunOoff)/length(homeRunOoff)
  homeRunDadj <-  sum(homeRunDoff)/length(homeRunDoff)
  homePFadj <-  sum(homePFoff)/length(homePFoff)
  homePAadj <-  sum(homePAoff)/length(homePAoff)
  
  homePassOsd <- sd(homePassOoff)
  homePassDsd <-  sd(homePassDoff)
  homeRunOsd <-  sd(homeRunOoff)
  homeRunDsd <-  sd(homeRunDoff)
  homePFsd <-  sd(homePFoff)
  homePAsd <-  sd(homePAoff)
  
  awayPassOadj <- sum(awayPassOoff)/length(awayPassOoff)
  awayPassDadj <-  sum(awayPassDoff)/length(awayPassDoff)
  awayRunOadj <-  sum(awayRunOoff)/length(awayRunOoff)
  awayRunDadj <-  sum(awayRunDoff)/length(awayRunDoff)
  awayPFadj <-  sum(awayPFoff)/length(awayPFoff)
  awayPAadj <-  sum(awayPAoff)/length(awayPAoff)
  
  
  awayPassOsd <- sd(awayPassOoff)
  awayPassDsd <-  sd(awayPassDoff)
  awayRunOsd <-  sd(awayRunOoff)
  awayRunDsd <-  sd(awayRunDoff)
  awayPFsd <-  sd(awayPFoff)
  awayPAsd <-  sd(awayPAoff)
  
  
  homeSoSoffset <- sqrt(SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS /  SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS)
  awaySoSoffset <- sqrt(SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS / SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS)
  
  
  homePassO <- rnorm(n=runCount, mean=homePassOadj, sd=homePassOsd)
  homePassD <-rnorm(n=runCount, mean=homePassDadj, sd=homePassDsd)
  awayPassO <-rnorm(n=runCount, mean=awayPassOadj, sd=awayPassOsd)
  awayPassD <-rnorm(n=runCount, mean=awayPassDadj, sd=awayPassDsd)
  
  homeRunO <- rnorm(n=runCount, mean=homeRunOadj, sd=homeRunOsd)
  homeRunD <-rnorm(n=runCount, mean=homeRunDadj, sd=homeRunDsd)
  awayRunO <-rnorm(n=runCount, mean=awayRunOadj, sd=awayRunOsd)
  awayRunD <-rnorm(n=runCount, mean=awayRunDadj, sd=awayRunDsd)
  
  homePassOYards<- rnorm(n=runCount, mean=mean(homePassOYards.Past), sd=sd(homePassOYards.Past))
  homePassDYards <- rnorm(n=runCount, mean=mean(homePassDYards.Past), sd=sd(homePassDYards.Past))
  awayPassOYards<- rnorm(n=runCount, mean=mean(awayPassOYards.Past), sd=sd(awayPassOYards.Past))
  awayPassDYards <- rnorm(n=runCount, mean=mean(awayPassDYards.Past), sd=sd(awayPassDYards.Past))
  
  homeRunOYards <- rnorm(n=runCount, mean=mean(homeRunOYards.Past), sd=sd(homeRunOYards.Past))
  homeRunDYards <- rnorm(n=runCount, mean=mean(homeRunDYards.Past), sd=sd(homeRunDYards.Past))
  awayRunOYards <- rnorm(n=runCount, mean=mean(awayRunOYards.Past), sd=sd(awayRunOYards.Past))
  awayRunDYards <- rnorm(n=runCount, mean=mean(awayRunDYards.Past), sd=sd(awayRunDYards.Past))
  
  homePointsForList <-  rnorm(n=runCount, mean=mean(homePF.Past), sd=sd(homePF.Past))
  homePointsAgainstList <- rnorm(n=runCount, mean=mean(homePA.Past), sd=sd(homePA.Past))
  
  awayPointsForList <-  rnorm(n=runCount, mean=mean(awayPF.Past), sd=sd(awayPF.Past))
  awayPointsAgainstList <- rnorm(n=runCount, mean=mean(awayPA.Past), sd=sd(awayPA.Past))
  
  homePenalty <- rnorm(n=runCount, mean=homePenaltyAgainstAve, sd=homePenaltyAgainstSd)
  awayPenalty <-rnorm(n=runCount, mean=awayPenaltyAgainstAve, sd=awayPenaltyAgainstSd)
  
  homeTurnoverAgainst <- rnorm(n=runCount, mean=homeTurnoverAgainstAve, sd=homeTurnoverAgainstSd)
  awayTurnoverAgainst <-rnorm(n=runCount, mean=awayTurnoverAgainstAve, sd=awayTurnoverAgainstSd)
  
  homeTurnoverFor <- rnorm(n=runCount, mean=homeTurnoverForAve, sd=homeTurnoverForSd)
  awayTurnoverFor <-rnorm(n=runCount, mean=awayTurnoverForAve, sd=awayTurnoverForSd)
  
  homePF <- rnorm(n=runCount, mean=homePFadj, sd=homePFsd)
  awayPF <- rnorm(n=runCount, mean=awayPFadj, sd=awayPFsd)
  
  homePA <- rnorm(n=runCount, mean=homePAadj, sd=homePAsd)
  awayPA <- rnorm(n=runCount, mean=awayPAadj, sd=awayPAsd)
  
  homePass <- c(1:runCount)
  awayPass <- c(1:runCount)
  
  homeRun <- c(1:runCount)
  awayRun <- c(1:runCount)
  
  homeScore <- c(1:runCount)
  awayScore <- c(1:runCount)
  
  homeTurnover <- c(1:runCount)
  awayTurnover <- c(1:runCount)
  
  for(j in 1:runCount){
    
    homeExpectedPass <- c(homeSoSoffset*homePassO[j]*awayPassDYards[j], homeSoSoffset*awayPassD[j]*homePassOYards[j])
    homeExpectedRun <- c(awaySoSoffset*homeRunO[j]*awayRunDYards[j], awaySoSoffset*awayRunD[j]*homeRunOYards[j])
    
    awayExpectedPass <- c(homeSoSoffset*homePassD[j]*awayPassOYards[j], homeSoSoffset*awayPassO[j]*homePassDYards[j])
    awayExpectedRun <- c(awaySoSoffset*homeRunD[j]*awayRunOYards[j], awaySoSoffset*awayRunO[j]*homeRunDYards[j])
    
    homePointsFor <- c(homeSoSoffset*homePF[j]*awayPointsAgainstList[j], homeSoSoffset*awayPA[j]*homePointsForList[j])
    awayPointsFor <- c(awaySoSoffset*homePA[j]*awayPointsForList[j], awaySoSoffset*awayPF[j]*homePointsAgainstList[j])
    
    homeExpectedPass[3] <- ((1.1*homeExpectedPass[1])+(0.9*homeExpectedPass[2]))/2 
    homeExpectedRun[3] <- ((1.1*homeExpectedRun[1])+(0.9*homeExpectedRun[2]))/2 
    
    awayExpectedPass[3] <- ((0.9*awayExpectedPass[1])+(1.1*awayExpectedPass[2]))/2 
    awayExpectedRun[3] <- ((0.9*awayExpectedRun[1])+(1.1*awayExpectedRun[2]))/2 
    
    homePointsFor[3] <- ((1.1*homePointsFor[1])+(0.9*homePointsFor[2]))/2 
    awayPointsFor[3] <- ((0.9*awayPointsFor[1])+(1.1*awayPointsFor[2]))/2 
    
    homeTurnovers <- (0.65*awayTurnoverFor[j]+1.35*homeTurnoverAgainst[j])/2
    awayTurnovers <- (0.75*homeTurnoverFor[j]+1.25*awayTurnoverAgainst[j])/2
    
    turnoverMargin <- homeTurnover[j] - awayTurnover[j]
    
    penaltyMargin <-  1.05* awayPenalty[j] - 0.95*homePenalty[j] 
    
    yardageMargin <- (homeExpectedRun[3]+homeExpectedPass[3])-(awayExpectedPass[3]+awayExpectedRun[3])
    
    homePointsFor[4] <- 3+(1.75*(yardageMargin/100.0))+(-1.75*turnoverMargin)+homePointsFor[3]
    awayPointsFor[4] <- (-1.75*(yardageMargin/100.0))+(1.75*turnoverMargin)+awayPointsFor[3]
    
    homePass[j] <- homeExpectedPass[3]
    awayPass[j] <- awayExpectedPass[3]
    
    homeRun[j] <- homeExpectedRun[3]
    awayRun[j] <- awayExpectedRun[3]
    
    
    homePenalty[j] <- 0.95*homePenalty[j]
    awayPenalty[j] <- 1.05*awayPenalty[j] 
    
    homeTurnover[j] <- homeTurnovers
    awayTurnover[j] <-awayTurnovers
    
    homeScore[j] <- homePointsFor[4]
    awayScore[j] <- awayPointsFor[4]
  }
  
  TEAM <- c(homeName, awayName)
  PASS <- c(mean(homePass), mean(awayPass))
  RUN <- c(mean(homeRun), mean(awayRun))
  PENALTY <- c(mean(homePenalty), mean(awayPenalty))
  TURNOVERS <- c(mean(homeTurnover), mean(awayTurnover))
  SCORE <- c(mean(homeScore), mean(awayScore))
  
  WINS <- c(length(homeScore[homeScore > awayScore]), length(homeScore[homeScore < awayScore]))
  return(Box.Score <- data.frame(TEAM, PASS, RUN, PENALTY, TURNOVERS, SCORE, WINS))
}


