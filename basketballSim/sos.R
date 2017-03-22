All.SoS <- function()
{
  schedules <- read.csv(file='data\\allscores.csv', sep=',',header=T)
  sos <- data.frame(TEAM = unique(schedules$Visitor.Team), row.names=NULL)
  sos$Wins <- 0
  sos$Losses <- 0
  sos$Win.Percent <- 0
  for(i in 1:length(sos$TEAM))
  {
    homeGames <- schedules[schedules$Home.Team == as.character(sos[i,]$TEAM),] 
    awayGames <- schedules[schedules$Visitor.Team == as.character(sos[i,]$TEAM),] 
    
    homeWins <- length(homeGames[homeGames$Home.Score > homeGames$Visitor.Score,]$Home.Score)
    homeLosses <- length(homeGames[homeGames$Home.Score < homeGames$Visitor.Score,]$Home.Score)
    awayWins  <- length(awayGames[awayGames$Home.Score < awayGames$Visitor.Score,]$Home.Score)
    awayLosses  <- length(awayGames[awayGames$Home.Score > awayGames$Visitor.Score,]$Home.Score)
    
    sos[i,]$Wins <- homeWins + awayWins
    sos[i,]$Losses <- homeLosses + awayLosses
    sos[i,]$Win.Percent <- (homeWins+awayWins)/(homeWins + awayWins+ homeLosses + awayLosses)
  }

  sos$First.Pass.SoS.Count <- 0
  sos$First.Pass.SoS.Sum <- 0
    
  for(i in 1:length(schedules$Visitor.Team))
  {
    homeMulti <- ifelse(schedules[i,]$Neutral %in% c("N", "NE"), 1, .95)
    awayMulti <- ifelse(schedules[i,]$Neutral %in% c("N", "NE"), 1, 1.05)
    weighting <- ifelse(schedules[i,]$Neutral %in% c("E", "NE"), .5, 1)
    
    visitor <- sos[sos$TEAM == as.character(schedules[i,]$Visitor.Team),] 
    home <- sos[sos$TEAM == as.character(schedules[i,]$Home.Team),] 
    
    if(nrow(home) != 0 && nrow(visitor == 0))
    {     
      sos[sos$TEAM == as.character(schedules[i,]$Visitor.Team),]$First.Pass.SoS.Count <- (
        visitor$First.Pass.SoS.Count + weighting )
      sos[sos$TEAM == as.character(schedules[i,]$Visitor.Team),]$First.Pass.SoS.Sum <- (
        visitor$First.Pass.SoS.Sum +  (awayMulti*weighting*home$Win.Percent))
    
      sos[sos$TEAM == as.character(schedules[i,]$Home.Team),]$First.Pass.SoS.Count <- (
        home$First.Pass.SoS.Count + weighting )
      sos[sos$TEAM == as.character(schedules[i,]$Home.Team),]$First.Pass.SoS.Sum <- (
        home$First.Pass.SoS.Sum +  (awayMulti*weighting*visitor$Win.Percent))
    }
  }
  sos$First.Pass.SoS <-   sos$First.Pass.SoS.Sum/sos$First.Pass.SoS.Count 
  sos$SoS.Sum <- 0
  sos$First.Pass.SoS.Sum <- NULL
  for(i in 1:length(schedules$Visitor.Team))
  {
    homeMulti <- ifelse(schedules[i,]$Neutral %in% c("N", "NE"), 1, .95)
    awayMulti <- ifelse(schedules[i,]$Neutral %in% c("N", "NE"), 1, 1.05)
    weighting <- ifelse(schedules[i,]$Neutral %in% c("E", "NE"), .5, 1)
    
    visitor <- sos[sos$TEAM == as.character(schedules[i,]$Visitor.Team),] 
    home <- sos[sos$TEAM == as.character(schedules[i,]$Home.Team),] 
    
    if(nrow(home) != 0 && nrow(visitor == 0))
    {     
      sos[sos$TEAM == as.character(schedules[i,]$Visitor.Team),]$SoS.Sum <- (
        visitor$SoS.Sum + (awayMulti*weighting*home$First.Pass.SoS))
      

      sos[sos$TEAM == as.character(schedules[i,]$Home.Team),]$SoS.Sum <- (
        home$SoS.Sum +  (awayMulti*weighting*visitor$First.Pass.SoS))
    }
  }
  sos$SoS <- ((2*sos$First.Pass.SoS) + (sos$SoS.Sum/sos$First.Pass.SoS.Count))/3
  sos$First.Pass.SoS.Count <- NULL
  sos$SoS.Sum <- NULL
  return(sos)
}

SoS.Best <- function()
{
  source("ranking.R")
}