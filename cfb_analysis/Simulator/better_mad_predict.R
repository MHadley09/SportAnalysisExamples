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
  deviation.YDS.G <- mad(offense$YDS.G)
  average.D.YDS.G <- sum(defense$D.YDS.G)/length(defense$D.YDS.G)
  deviation.D.YDS.G <- mad(defense$D.YDS.G)
  
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
  homeTurnoverForSd <- mad(homeOpps$TURNOVERS.FOR)
  
  homeTurnoverAgainstAve <- sum(homeOpps$TURNOVERS.AGAINST)/length(homeOpps$TURNOVERS.AGAINST)
  homeTurnoverAgainstSd <- mad(homeOpps$TURNOVERS.AGAINST)
  
  homePenaltyAgainstAve <- sum(homeOpps$PENALTY.YARDS.AGAINST)/length(homeOpps$PENALTY.YARDS.AGAINST)
  homePenaltyAgainstSd  <- mad(homeOpps$PENALTY.YARDS.AGAINST)
  
  awayPassOadj <- 0
  awayPassDadj <- 0
  awayRunOadj <- 0
  awayRunDadj <- 0
  awayPFadj <- 0
  awayPAadj <- 0
  
  awayTurnoverForAve <- sum(awayOpps$TURNOVERS.FOR)/length(awayOpps$TURNOVERS.FOR)                      
  awayTurnoverForSd <- mad(awayOpps$TURNOVERS.FOR)
  
  awayTurnoverAgainstAve <- sum(awayOpps$TURNOVERS.AGAINST)/length(awayOpps$TURNOVERS.AGAINST)
  awayTurnoverAgainstSd <- mad(awayOpps$TURNOVERS.AGAINST)
  
  awayPenaltyAgainstAve <- sum(awayOpps$PENALTY.YARDS.AGAINST)/length(awayOpps$PENALTY.YARDS.AGAINST)
  awayPenaltyAgainstSd  <- mad(awayOpps$PENALTY.YARDS.AGAINST)
  
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
    
    
    SoSOff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSOff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    opp <- combine[combine$TEAM == oppName,]
    
    homePassOoff[i] <- SoSOff*((homeOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    homePassDoff[i] <- max(0, 1 + ((homeOpps[i,]$P.YARDS.AGAINST - opp$P.YDS.G)/(SoSOff*opp$P.YDS.G)))
    homeRunOoff[i] <- SoSOff*(homeOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    homeRunDoff[i] <-  max(0, 1 + ((homeOpps[i,]$R.YARDS.AGAINST - opp$R.YDS.G)/(SoSOff*opp$R.YDS.G)))
    homePFoff[i] <- SoSOff*(homeOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    homePAoff[i] <-  max(0, 1 + ((homeOpps[i,]$POINTS.AGAINST - opp$PTS.G)/(SoSOff*opp$PTS.G)))
    
    homePassOYards.Past[i] <-  homeOpps[i,]$P.YARDS.FOR
    homePassDYards.Past[i] <- homeOpps[i,]$P.YARDS.AGAINST
    homeRunOYards.Past[i] <- homeOpps[i,]$R.YARDS.F
    homeRunDYards.Past[i] <- homeOpps[i,]$R.YARDS.AGAINST
    homePF.Past[i] <-  homeOpps[i,]$POINTS.FOR
    homePA.Past[i] <- homeOpps[i,]$POINTS.AGAINST
    
    
  }
  
  
  for(i in 1:length(awayOpps$OPP))
  {
    oppName <- as.character(awayOpps[i,]$OPP)
    opp <- combine[combine$TEAM == oppName,]
    
    SoSOff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSOff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    awayPassOoff[i] <- SoSOff*((awayOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    awayPassDoff[i] <- max(0, 1 + ((awayOpps[i,]$P.YARDS.AGAINST - opp$P.YDS.G)/(SoSOff*opp$P.YDS.G)))
    awayRunOoff[i] <- SoSOff*(awayOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    awayRunDoff[i] <-  max(0, 1 + ((awayOpps[i,]$R.YARDS.AGAINST - opp$R.YDS.G)/(SoSOff*opp$R.YDS.G)))
    awayPFoff[i] <- SoSOff*(awayOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    awayPAoff[i] <-  max(0, 1 + ((awayOpps[i,]$POINTS.AGAINST - opp$PTS.G)/(SoSOff*opp$PTS.G)))
    
    
    awayPassOYards.Past[i] <- awayOpps[i,]$P.YARDS.FOR
    awayPassDYards.Past[i] <- awayOpps[i,]$P.YARDS.AGAINST
    awayRunOYards.Past[i] <-  awayOpps[i,]$R.YARDS.F
    awayRunDYards.Past[i] <- awayOpps[i,]$R.YARDS.AGAINST
    awayPF.Past[i] <-  awayOpps[i,]$POINTS.FOR
    awayPA.Past[i] <- awayOpps[i,]$POINTS.AGAINST
  }
  
  homePassOadj <- sum(homePassOoff)/length(homePassOoff)
  homePassDadj <-  sum(homePassDoff)/length(homePassDoff)
  homeRunOadj <-  sum(homeRunOoff)/length(homeRunOoff)
  homeRunDadj <-  sum(homeRunDoff)/length(homeRunDoff)
  homePFadj <-  sum(homePFoff)/length(homePFoff)
  homePAadj <-  sum(homePAoff)/length(homePAoff)
  
  homePassOmad <- mad(homePassOoff)
  homePassDmad <-  mad(homePassDoff)
  homeRunOmad <-  mad(homeRunOoff)
  homeRunDmad <-  mad(homeRunDoff)
  homePFmad <-  mad(homePFoff)
  homePAmad <-  mad(homePAoff)
  
  awayPassOadj <- sum(awayPassOoff)/length(awayPassOoff)
  awayPassDadj <-  sum(awayPassDoff)/length(awayPassDoff)
  awayRunOadj <-  sum(awayRunOoff)/length(awayRunOoff)
  awayRunDadj <-  sum(awayRunDoff)/length(awayRunDoff)
  awayPFadj <-  sum(awayPFoff)/length(awayPFoff)
  awayPAadj <-  sum(awayPAoff)/length(awayPAoff)
  
  
  awayPassOmad <- mad(awayPassOoff)
  awayPassDmad <-  mad(awayPassDoff)
  awayRunOmad <-  mad(awayRunOoff)
  awayRunDmad <-  mad(awayRunDoff)
  awayPFmad <-  mad(awayPFoff)
  awayPAmad <-  mad(awayPAoff)
  
  
  homeSoSoffset <- sqrt(SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS /  SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS)
  awaySoSoffset <- sqrt(SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS / SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS)
  
  
  homePassO <- rnorm(n=runCount, mean=homePassOadj, sd=homePassOmad)
  homePassD <-rnorm(n=runCount, mean=homePassDadj, sd=homePassDmad)
  awayPassO <-rnorm(n=runCount, mean=awayPassOadj, sd=awayPassOmad)
  awayPassD <-rnorm(n=runCount, mean=awayPassDadj, sd=awayPassDmad)
  
  homeRunO <- rnorm(n=runCount, mean=homeRunOadj, sd=homeRunOmad)
  homeRunD <-rnorm(n=runCount, mean=homeRunDadj, sd=homeRunDmad)
  awayRunO <-rnorm(n=runCount, mean=awayRunOadj, sd=awayRunOmad)
  awayRunD <-rnorm(n=runCount, mean=awayRunDadj, sd=awayRunDmad)
  
  homePassOYards<- rnorm(n=runCount, mean=mean(homePassOYards.Past), sd=mad(homePassOYards.Past))
  homePassDYards <- rnorm(n=runCount, mean=mean(homePassDYards.Past), sd=mad(homePassDYards.Past))
  awayPassOYards<- rnorm(n=runCount, mean=mean(awayPassOYards.Past), sd=mad(awayPassOYards.Past))
  awayPassDYards <- rnorm(n=runCount, mean=mean(awayPassDYards.Past), sd=mad(awayPassDYards.Past))
  
  homeRunOYards <- rnorm(n=runCount, mean=mean(homeRunOYards.Past), sd=mad(homeRunOYards.Past))
  homeRunDYards <- rnorm(n=runCount, mean=mean(homeRunDYards.Past), sd=mad(homeRunDYards.Past))
  awayRunOYards <- rnorm(n=runCount, mean=mean(awayRunOYards.Past), sd=mad(awayRunOYards.Past))
  awayRunDYards <- rnorm(n=runCount, mean=mean(awayRunDYards.Past), sd=mad(awayRunDYards.Past))
  
  homePointsForList <-  rnorm(n=runCount, mean=mean(homePF.Past), sd=mad(homePF.Past))
  homePointsAgainstList <- rnorm(n=runCount, mean=mean(homePA.Past), sd=mad(homePA.Past))
  
  awayPointsForList <-  rnorm(n=runCount, mean=mean(awayPF.Past), sd=mad(awayPF.Past))
  awayPointsAgainstList <- rnorm(n=runCount, mean=mean(awayPA.Past), sd=mad(awayPA.Past))
  
  homePenalty <- rnorm(n=runCount, mean=homePenaltyAgainstAve, sd=homePenaltyAgainstSd)
  awayPenalty <-rnorm(n=runCount, mean=awayPenaltyAgainstAve, sd=awayPenaltyAgainstSd)
  
  homeTurnoverAgainst <- rnorm(n=runCount, mean=homeTurnoverAgainstAve, sd=homeTurnoverAgainstSd)
  awayTurnoverAgainst <-rnorm(n=runCount, mean=awayTurnoverAgainstAve, sd=awayTurnoverAgainstSd)
  
  homeTurnoverFor <- rnorm(n=runCount, mean=homeTurnoverForAve, sd=homeTurnoverForSd)
  awayTurnoverFor <-rnorm(n=runCount, mean=awayTurnoverForAve, sd=awayTurnoverForSd)
  
  homePF <- rnorm(n=runCount, mean=homePFadj, sd=homePFmad)
  awayPF <- rnorm(n=runCount, mean=awayPFadj, sd=awayPFmad)
  
  homePA <- rnorm(n=runCount, mean=homePAadj, sd=homePAmad)
  awayPA <- rnorm(n=runCount, mean=awayPAadj, sd=awayPAmad)
  
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

mad.predict <- function(homeName, awayName, runCount){
  
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
  deviation.YDS.G <- mad(offense$YDS.G)
  average.D.YDS.G <- sum(defense$D.YDS.G)/length(defense$D.YDS.G)
  deviation.D.YDS.G <- mad(defense$D.YDS.G)
  
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
  homeTurnoverForSd <- mad(homeOpps$TURNOVERS.FOR)
  
  homeTurnoverAgainstAve <- sum(homeOpps$TURNOVERS.AGAINST)/length(homeOpps$TURNOVERS.AGAINST)
  homeTurnoverAgainstSd <- mad(homeOpps$TURNOVERS.AGAINST)
  
  homePenaltyAgainstAve <- sum(homeOpps$PENALTY.YARDS.AGAINST)/length(homeOpps$PENALTY.YARDS.AGAINST)
  homePenaltyAgainstSd  <- mad(homeOpps$PENALTY.YARDS.AGAINST)
  
  awayPassOadj <- 0
  awayPassDadj <- 0
  awayRunOadj <- 0
  awayRunDadj <- 0
  awayPFadj <- 0
  awayPAadj <- 0
  
  awayTurnoverForAve <- sum(awayOpps$TURNOVERS.FOR)/length(awayOpps$TURNOVERS.FOR)                      
  awayTurnoverForSd <- mad(awayOpps$TURNOVERS.FOR)
  
  awayTurnoverAgainstAve <- sum(awayOpps$TURNOVERS.AGAINST)/length(awayOpps$TURNOVERS.AGAINST)
  awayTurnoverAgainstSd <- mad(awayOpps$TURNOVERS.AGAINST)
  
  awayPenaltyAgainstAve <- sum(awayOpps$PENALTY.YARDS.AGAINST)/length(awayOpps$PENALTY.YARDS.AGAINST)
  awayPenaltyAgainstSd  <- mad(awayOpps$PENALTY.YARDS.AGAINST)
  
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
    
    
    SoSOff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSOff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    opp <- combine[combine$TEAM == oppName,]
    
    homePassOoff[i] <- SoSOff*((homeOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    homePassDoff[i] <- max(0, 1 + ((homeOpps[i,]$P.YARDS.AGAINST - opp$P.YDS.G)/(SoSOff*opp$P.YDS.G)))
    homeRunOoff[i] <- SoSOff*(homeOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    homeRunDoff[i] <-  max(0, 1 + ((homeOpps[i,]$R.YARDS.AGAINST - opp$R.YDS.G)/(SoSOff*opp$R.YDS.G)))
    homePFoff[i] <- SoSOff*(homeOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    homePAoff[i] <-  max(0, 1 + ((homeOpps[i,]$POINTS.AGAINST - opp$PTS.G)/(SoSOff*opp$PTS.G)))
    
    homePassOYards.Past[i] <-  homeOpps[i,]$P.YARDS.FOR
    homePassDYards.Past[i] <- homeOpps[i,]$P.YARDS.AGAINST
    homeRunOYards.Past[i] <-  homeOpps[i,]$R.YARDS.F
    homeRunDYards.Past[i] <- homeOpps[i,]$R.YARDS.AGAINST
    homePF.Past[i] <-  homeOpps[i,]$POINTS.FOR
    homePA.Past[i] <- homeOpps[i,]$POINTS.AGAINST
    
    
  }
  
  
  for(i in 1:length(awayOpps$OPP))
  {
    oppName <- as.character(awayOpps[i,]$OPP)
    opp <- combine[combine$TEAM == oppName,]
    
    SoSOff <- sqrt(SoS[SoS$TEAM == oppName,]$Adjusted.SoS / averageSoS )
    DSoSOff <- sqrt(averageSoS / SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
    
    awayPassOoff[i] <- SoSOff*((awayOpps[i,]$P.YARDS.FOR)/(opp$D.P.YDS.G))
    awayPassDoff[i] <- max(0, 1 + ((awayOpps[i,]$P.YARDS.AGAINST - opp$P.YDS.G)/(SoSOff*opp$P.YDS.G)))
    awayRunOoff[i] <- SoSOff*(awayOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
    awayRunDoff[i] <-  max(0, 1 + ((awayOpps[i,]$R.YARDS.AGAINST - opp$R.YDS.G)/(SoSOff*opp$R.YDS.G)))
    awayPFoff[i] <- SoSOff*(awayOpps[i,]$POINTS.FOR)/opp$D.PTS.G
    awayPAoff[i] <-  max(0, 1 + ((awayOpps[i,]$POINTS.AGAINST - opp$PTS.G)/(SoSOff*opp$PTS.G)))
    
    
    awayPassOYards.Past[i] <- awayOpps[i,]$P.YARDS.FOR
    awayPassDYards.Past[i] <- awayOpps[i,]$P.YARDS.AGAINST
    awayRunOYards.Past[i] <- awayOpps[i,]$R.YARDS.F
    awayRunDYards.Past[i] <- awayOpps[i,]$R.YARDS.AGAINST
    awayPF.Past[i] <- awayOpps[i,]$POINTS.FOR
    awayPA.Past[i] <- awayOpps[i,]$POINTS.AGAINST
  }
  
  homePassOadj <- sum(homePassOoff)/length(homePassOoff)
  homePassDadj <-  sum(homePassDoff)/length(homePassDoff)
  homeRunOadj <-  sum(homeRunOoff)/length(homeRunOoff)
  homeRunDadj <-  sum(homeRunDoff)/length(homeRunDoff)
  homePFadj <-  sum(homePFoff)/length(homePFoff)
  homePAadj <-  sum(homePAoff)/length(homePAoff)
  
  homePassOmad <- mad(homePassOoff)
  homePassDmad <-  mad(homePassDoff)
  homeRunOmad <-  mad(homeRunOoff)
  homeRunDmad <-  mad(homeRunDoff)
  homePFmad <-  mad(homePFoff)
  homePAmad <-  mad(homePAoff)
  
  awayPassOadj <- sum(awayPassOoff)/length(awayPassOoff)
  awayPassDadj <-  sum(awayPassDoff)/length(awayPassDoff)
  awayRunOadj <-  sum(awayRunOoff)/length(awayRunOoff)
  awayRunDadj <-  sum(awayRunDoff)/length(awayRunDoff)
  awayPFadj <-  sum(awayPFoff)/length(awayPFoff)
  awayPAadj <-  sum(awayPAoff)/length(awayPAoff)
  
  
  awayPassOmad <- mad(awayPassOoff)
  awayPassDmad <-  mad(awayPassDoff)
  awayRunOmad <-  mad(awayRunOoff)
  awayRunDmad <-  mad(awayRunDoff)
  awayPFmad <-  mad(awayPFoff)
  awayPAmad <-  mad(awayPAoff)
  
  
  homeSoSoffset <- sqrt(SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS /  SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS)
  awaySoSoffset <- sqrt(SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS / SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS)
  
  
  homePassO <- rnorm(n=runCount, mean=homePassOadj, sd=homePassOmad)
  homePassD <-rnorm(n=runCount, mean=homePassDadj, sd=homePassDmad)
  awayPassO <-rnorm(n=runCount, mean=awayPassOadj, sd=awayPassOmad)
  awayPassD <-rnorm(n=runCount, mean=awayPassDadj, sd=awayPassDmad)
  
  homeRunO <- rnorm(n=runCount, mean=homeRunOadj, sd=homeRunOmad)
  homeRunD <-rnorm(n=runCount, mean=homeRunDadj, sd=homeRunDmad)
  awayRunO <-rnorm(n=runCount, mean=awayRunOadj, sd=awayRunOmad)
  awayRunD <-rnorm(n=runCount, mean=awayRunDadj, sd=awayRunDmad)
  
  homePassOYards<- rnorm(n=runCount, mean=mean(homePassOYards.Past), sd=mad(homePassOYards.Past))
  homePassDYards <- rnorm(n=runCount, mean=mean(homePassDYards.Past), sd=mad(homePassDYards.Past))
  awayPassOYards<- rnorm(n=runCount, mean=mean(awayPassOYards.Past), sd=mad(awayPassOYards.Past))
  awayPassDYards <- rnorm(n=runCount, mean=mean(awayPassDYards.Past), sd=mad(awayPassDYards.Past))
  
  homeRunOYards <- rnorm(n=runCount, mean=mean(homeRunOYards.Past), sd=mad(homeRunOYards.Past))
  homeRunDYards <- rnorm(n=runCount, mean=mean(homeRunDYards.Past), sd=mad(homeRunDYards.Past))
  awayRunOYards <- rnorm(n=runCount, mean=mean(awayRunOYards.Past), sd=mad(awayRunOYards.Past))
  awayRunDYards <- rnorm(n=runCount, mean=mean(awayRunDYards.Past), sd=mad(awayRunDYards.Past))
  
  homePointsForList <-  rnorm(n=runCount, mean=mean(homePF.Past), sd=mad(homePF.Past))
  homePointsAgainstList <- rnorm(n=runCount, mean=mean(homePA.Past), sd=mad(homePA.Past))
  
  awayPointsForList <-  rnorm(n=runCount, mean=mean(awayPF.Past), sd=mad(awayPF.Past))
  awayPointsAgainstList <- rnorm(n=runCount, mean=mean(awayPA.Past), sd=mad(awayPA.Past))
  
  homePenalty <- rnorm(n=runCount, mean=homePenaltyAgainstAve, sd=homePenaltyAgainstSd)
  awayPenalty <-rnorm(n=runCount, mean=awayPenaltyAgainstAve, sd=awayPenaltyAgainstSd)
  
  homeTurnoverAgainst <- rnorm(n=runCount, mean=homeTurnoverAgainstAve, sd=homeTurnoverAgainstSd)
  awayTurnoverAgainst <-rnorm(n=runCount, mean=awayTurnoverAgainstAve, sd=awayTurnoverAgainstSd)
  
  homeTurnoverFor <- rnorm(n=runCount, mean=homeTurnoverForAve, sd=homeTurnoverForSd)
  awayTurnoverFor <-rnorm(n=runCount, mean=awayTurnoverForAve, sd=awayTurnoverForSd)
  
  homePF <- rnorm(n=runCount, mean=homePFadj, sd=homePFmad)
  awayPF <- rnorm(n=runCount, mean=awayPFadj, sd=awayPFmad)
  
  homePA <- rnorm(n=runCount, mean=homePAadj, sd=homePAmad)
  awayPA <- rnorm(n=runCount, mean=awayPAadj, sd=awayPAmad)
  
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

