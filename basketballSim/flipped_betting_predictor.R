data <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

data$Visitor.Team <- trim(data$Visitor.Team)
data$Home.Team <- trim(data$Home.Team)

leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

data <- data[data$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

data <- data[data$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


#data[!(data$Neutral %in% c("N", "NE")),]$Home.Score <- data[!(data$Neutral %in% c("N", "NE")),]$Home.Score - 3.5 
#data[!(data$Neutral %in% c("N", "NE")),]$Visitor.Score <- data[!(data$Neutral %in% c("N", "NE")),]$Visitor.Score + 3.5 

Team <- c(data$Home.Team, data$Visitor.Team)
Score <- c(data$Home.Score, data$Visitor.Score)
Opp <- c(data$Visitor.Team, data$Home.Team)
Opp.Score <- c(data$Visitor.Score,data$Home.Score)
Date <- c(data$Date, data$Date)

#Weighting <- ifelse(data$Neutral %in% c("E", "NE"), .75, 1)
#Weighting <- c(Weighting, Weighting)

combined <- data.frame(Team=Team, Score=Score, Opp=Opp, Opp.Score=Opp.Score, Date=Date)

combined$Wins <- ifelse(combined$Score > combined$Opp.Score, 1, 0)
combined$Losses <- ifelse(combined$Score < combined$Opp.Score, 1, 0)
#list <- as.vector(by(combined[c("Score","Weighting")],
#             list(combined$Team),
#             function(x) {
#               do.call(weighted.mean, unname(x))
#             }
#))

record <- aggregate(cbind(Wins, Losses)~Team, data=combined, FUN=sum)
stat.mean <- aggregate(cbind(Score, Opp.Score)~Team , data=combined, FUN=mean)
stat.std <- aggregate(cbind(Score, Opp.Score)~Team , data=combined, FUN=sd)





neutral.predict <- function(homeName, awayName, spread, overunder, runCount)
{
  homeOpps <- combined[combined$Team == homeName,]
  awayOpps <- combined[combined$Team == awayName,]
  
  homeAdjOff <- 1:nrow(homeOpps)
  homeAdjDef <- 1:nrow(homeOpps)
  
  awayAdjOff <- 1:nrow(awayOpps) 
  awayAdjDef <- 1:nrow(awayOpps)
  
  for(i in 1:length(homeOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(homeOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(homeOpps[i,]$Opp),]
    homeAdjOff[i] <- mean(homeOpps[i,]$Score / oppMean$Opp.Score), na.rm = T)
    homeAdjDef[i] <- mean(homeOpps[i,]$Opp.Score / oppMean$Score), na.rm = T)
  }
  for(i in 1:length(awayOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(awayOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(awayOpps[i,]$Opp),]
    awayAdjOff[i] <- mean((homeOpps[i,]$Score / oppMean$Opp.Score), na.rm = T)
    awayAdjDef[i] <- mean((homeOpps[i,]$Opp.Score / oppMean$Score), na.rm = T)
  }
  
  home.Mean <- stat.mean[stat.mean$Team == homeName,]
  away.Mean <- stat.mean[stat.mean$Team == awayName,]
  
  home.Sd <- stat.std[stat.std$Team == homeName,]
  away.Sd <- stat.std[stat.std$Team == awayName,]
  
  homeOffense <- rnorm(n=runCount,mean=home.Mean$Score,sd=home.Sd$Score, na.rm = T)
  awayOffense <- rnorm(n=runCount,mean=away.Mean$Score,sd=away.Sd$Score, na.rm = T)
  homeDefense <- rnorm(n=runCount,mean=home.Mean$Opp.Score,sd=home.Sd$Opp.Score, na.rm = T)
  awayDefense <- rnorm(n=runCount,mean=away.Mean$Opp.Score,sd=away.Sd$Opp.Score, na.rm = T)
  

  
  home.score <- ((1.1*(homeAdjOff*awayDefense)) 
                 + (.9*(awayAdjDef*homeOffense)))/2
  away.score <- ((1.1*(awayAdjOff*homeDefense)) 
                 + (.9*(homeAdjDef*awayOffense)))/2
  
  home.Wins <- sum(home.score>away.score)
  away.Wins <- sum(home.score<away.score)
  home.spread.Wins <- sum(home.score+spread > (away.score))
  away.spread.Wins <- sum(home.score+spread < (away.score))
  
  over <-  sum(home.score+away.score > overunder)
  under <-  sum(home.score+away.score < overunder)
  
  box.score <- data.frame(Team = c(homeName,awayName), 
                          Score=c(mean(home.score),mean(away.score)),
                          Wins=c(home.Wins,away.Wins), 
                          Spread.Wins=c(home.spread.Wins,away.spread.Wins),
                          Over.Under=c(over,under))
  
  return(box.score)
}
predict <- function(homeName, awayName, spread, overunder, runCount)
{
  homeOpps <- combined[combined$Team == homeName,]
  awayOpps <- combined[combined$Team == awayName,]
  
  homeAdjOff <- 1:nrow(homeOpps)
  homeAdjDef <- 1:nrow(homeOpps)
  
  awayAdjOff <- 1:nrow(awayOpps) 
  awayAdjDef <- 1:nrow(awayOpps)
  
  for(i in 1:length(homeOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(homeOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(homeOpps[i,]$Opp),]
    homeAdjOff[i] <- mean(homeOpps[i,]$Score / oppMean$Opp.Score), na.rm = T)
    homeAdjDef[i] <- mean(homeOpps[i,]$Opp.Score / oppMean$Score), na.rm = T)
  }
  for(i in 1:length(awayOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(awayOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(awayOpps[i,]$Opp),]
    awayAdjOff[i] <- mean((homeOpps[i,]$Score / oppMean$Opp.Score), na.rm = T)
    awayAdjDef[i] <- mean((homeOpps[i,]$Opp.Score / oppMean$Score), na.rm = T)
  }

  home.Mean <- stat.mean[stat.mean$Team == homeName,]
  away.Mean <- stat.mean[stat.mean$Team == awayName,]

  home.Sd <- stat.std[stat.std$Team == homeName,]
  away.Sd <- stat.std[stat.std$Team == awayName,]

  homeOffense <- rnorm(n=runCount,mean=home.Mean$Score,sd=home.Sd$Score, na.rm = T)
  awayOffense <- rnorm(n=runCount,mean=away.Mean$Score,sd=away.Sd$Score, na.rm = T)
  homeDefense <- rnorm(n=runCount,mean=home.Mean$Opp.Score,sd=home.Sd$Opp.Score, na.rm = T)
  awayDefense <- rnorm(n=runCount,mean=away.Mean$Opp.Score,sd=away.Sd$Opp.Score, na.rm = T)

  home.score <- 2.968459+(((1.1*(home.Mean$Score*awayDefense)) 
                    + (.9*(away.Mean$Opp.Score*homeOffense)))/2)
  away.score <- (((1.1*(away.Mean$Score*homeDefense)) 
                     + (.9*(home.Mean$Opp.Score*awayOffense)))/2)
  
  home.Wins <- sum(home.score>away.score)
  away.Wins <- sum(home.score<away.score)
  home.spread.Wins <- sum(home.score+spread > (away.score))
  away.spread.Wins <- sum(home.score+spread < (away.score))
  
  over <-  sum(home.score+away.score > overunder)
  under <-  sum(home.score+away.score < overunder)
  box.score <- data.frame(Team = c(homeName,awayName), 
                          Score=c(mean(home.score),mean(away.score)),
                          Wins=c(home.Wins,away.Wins), 
                          Spread.Wins=c(home.spread.Wins,away.spread.Wins),
                          Over.Under=c(over,under))
  
 
    
  return(box.score)
}