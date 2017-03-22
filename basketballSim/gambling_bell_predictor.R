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





neutral.predict <- function(homeName, awayName, spread, runCount)
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
    homeAdjOff[i] <- (homeOpps[i,]$Score / oppMean$Opp.Score)
    homeAdjDef[i] <- (homeOpps[i,]$Opp.Score / oppMean$Score)
  }
  for(i in 1:length(awayOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(awayOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(awayOpps[i,]$Opp),]
    awayAdjOff[i] <- (homeOpps[i,]$Score / oppMean$Opp.Score)
    awayAdjDef[i] <- (homeOpps[i,]$Opp.Score / oppMean$Score)
  }
  
  homeOffense <- rnorm(n=runCount,mean=mean(homeAdjOff, na.rm = T),sd=sd(homeAdjOff, na.rm = T))
  awayOffense <- rnorm(n=runCount,mean=mean(awayAdjOff, na.rm = T),sd=sd(awayAdjOff, na.rm = T))
  homeDefense <- rnorm(n=runCount,mean=mean(homeAdjDef, na.rm = T),sd=sd(homeAdjDef, na.rm = T))
  awayDefense <- rnorm(n=runCount,mean=mean(awayAdjDef, na.rm = T),sd=sd(awayAdjDef, na.rm = T))
  
  home.Mean <- stat.mean[stat.mean$Team == homeName,]
  away.Mean <- stat.mean[stat.mean$Team == awayName,]
  
  home.Sd <- stat.std[stat.std$Team == homeName,]
  away.Sd <- stat.std[stat.std$Team == awayName,]
  
  home.score <- (((1.199967)*(home.Mean$Score*awayDefense)) 
                 + (( 0.800033)*(away.Mean$Opp.Score*homeOffense)))/2
  away.score <- (((1.199967)*(away.Mean$Score*homeDefense)) 
                 + (( 0.800033)*(home.Mean$Opp.Score*awayOffense)))/2
  
  
  home.Wins <- sum((home.score+spread)>away.score)
  away.Wins <- sum((home.score+spread)<away.score)
  
  box.score <- data.frame(Team = c(homeName,awayName), Score=c(mean(home.score),mean(away.score)),Wins=c(home.Wins,away.Wins))
  
  return(box.score)
}
predict <- function(homeName, awayName, spread, runCount, weight)
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
    homeAdjOff[i] <- (homeOpps[i,]$Score / oppMean$Opp.Score)
    homeAdjDef[i] <- (homeOpps[i,]$Opp.Score / oppMean$Score)
  }
  for(i in 1:length(awayOpps$Opp))
  {
    oppMean <- stat.mean[stat.mean$Team == as.character(awayOpps[i,]$Opp),]
    oppStd <- stat.std[stat.std$Team == as.character(awayOpps[i,]$Opp),]
    awayAdjOff[i] <- (homeOpps[i,]$Score / oppMean$Opp.Score)
    awayAdjDef[i] <- (homeOpps[i,]$Opp.Score / oppMean$Score)
  }
  
  homeOffense <- rnorm(n=runCount,mean=mean(homeAdjOff, na.rm = T),sd=sd(homeAdjOff, na.rm = T))
  awayOffense <- rnorm(n=runCount,mean=mean(awayAdjOff, na.rm = T),sd=sd(awayAdjOff, na.rm = T))
  homeDefense <- rnorm(n=runCount,mean=mean(homeAdjDef, na.rm = T),sd=sd(homeAdjDef, na.rm = T))
  awayDefense <- rnorm(n=runCount,mean=mean(awayAdjDef, na.rm = T),sd=sd(awayAdjDef, na.rm = T))
  
  home.Mean <- stat.mean[stat.mean$Team == homeName,]
  away.Mean <- stat.mean[stat.mean$Team == awayName,]
  
  home.Sd <- stat.std[stat.std$Team == homeName,]
  away.Sd <- stat.std[stat.std$Team == awayName,]
  
  home.score <- ((((1.199967)*(home.Mean$Score*awayDefense)) 
                 + (( 0.800033)*(away.Mean$Opp.Score*homeOffense)))/2)+(2.968459)
  away.score <- ((((1.199967)*(away.Mean$Score*homeDefense)) 
                 + (( 0.800033)*(home.Mean$Opp.Score*awayOffense)))/2)
  
  home.Wins <- sum((home.score+spread)>away.score)
  away.Wins <- sum((home.score+spread)<away.score)
  
  box.score <- data.frame(Team = c(homeName,awayName), Score=c(mean(home.score),mean(away.score)),Wins=c(home.Wins,away.Wins))
  
  return(box.score)
}