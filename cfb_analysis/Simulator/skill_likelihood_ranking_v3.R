predict.score <- function(offense, defense)
{
  return(offense-defense)
}

log.likelihood.offense <- function(x, defense, score, deviation)
{
  return((dnorm(predict.score(x, defense), score, deviation, log=T)))
}
log.likelihood.defense <- function(x, offense, score, deviation)
{
  return((dnorm(predict.score(offense, x), score, deviation, log=T)))
}

calculate.rankings <- function()
{
  schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)

#  divisions <-read.csv(file = 'data\\2005\\divisionBreakdown.csv', sep=',', header=T)
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                   Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)

  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                  Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  last.step.offense <- rep(28, length(teams))
  offense <- rep(28, length(teams))
  last.step.defense <- rep(0, length(teams))
  defense <- rep(0, length(teams))
  
  rm(schedule)
  

  
  rankings <- data.frame(Team=teams, Last.Step.Offense = last.step.offense, Offense = offense,
                         Last.Step.Defense = last.step.defense, Defense = defense)
  

  divisions <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)
  FBS <- data.frame(Team=divisions[divisions$Conference != "FCS",]$TEAM)
  FBS <- merge(FBS, rankings, by="Team")
  
  rankings <- FBS

  data <- data[data$Team %in% as.character(rankings$Team),]
  data <- data[data$Opp %in% as.character(rankings$Team),]

  deviation <- sd(c(data$Score, data$Opp.Score))

  rm(teams,last.step.defense, last.step.offense)
  for(j in 1:50)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Score=scores, Opp.Score = oppScores), 
                     data.frame(Opp=oppScore$Team, Opp.Offense=oppScore$Last.Step.Offense,
                                Opp.Defense=oppScore$Last.Step.Defense), by="Opp")
      
      func1 <- function(x)
      {
        return(mean(log.likelihood.offense(x, games$Opp.Defense, games$Score, deviation)))
      }
      rankings[i,]$Offense <- optimize(f=func1, interval=c(-100,100), maximum=T)$maximum
      
      func2 <- function(x)
      {
        return(mean(log.likelihood.defense(x, games$Opp.Offense, games$Opp.Score, deviation)))
      }
      rankings[i,]$Defense <- optimize(f=func2, interval=c(-100,100), maximum=T)$maximum
    }
    rankings$Last.Step.Offense <- rankings$Offense
    rankings$Last.Step.Defense <- rankings$Defense
  }

  divisions <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

  FBS <- data.frame(Team=divisions[divisions$Conference != "FCS",]$TEAM)
  FBS <- merge(FBS, rankings, by="Team")

  records <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  names(records)[names(records)=="TEAM"] <- "Team"

  records$Win.Pct <- records$OVERALL.W / (records$OVERALL.W + records$OVERALL.L)
  records$Record.Rank <- 1 + length(records$Win.Pct) - rank(records$Win.Pct)
  source('Py_SoS_played.r')
  SoS <- FBS.SoS()
  names(SoS)[names(SoS)=="TEAM"] <- "Team"
  FBS <- merge(FBS, SoS, by="Team")
  FBS <- merge(FBS, records, by="Team")

  FBS$Offense.Rank <- (1 + length(FBS$Offense) - rank(FBS$Offense))
  FBS$Defense.Rank <- (1 + length(FBS$Defense) - rank(FBS$Defense))


  FBS$Offense.Rank <- rank(FBS$Offense.Rank)
  FBS$Defense.Rank <- rank(FBS$Defense.Rank)
  FBS$Overall.Rank <- rank(((2*FBS$Offense.Rank)+(2*FBS$Defense.Rank)+FBS$Record.Rank+(.5*FBS$SoS.Ranking))/5.5)
  return(FBS)
}

calculate.FBS.Rankings <- function()
{
  rankings <- calculate.rankings()

  rankings$Offense.Score <- rankings$Offense - min(rankings$Offense)
  rankings$Defense.Score <- rankings$Defense - min(rankings$Defense)

  rankings$SoS.Score <- 0.75 + ((rankings$SoS.Score - min(rankings$SoS.Score))/(4*max(rankings$SoS.Score)))
  rankings$Overall.Score <-   rankings$SoS.Score*((rankings$Offense.Score+rankings$Defense.Score))/2
  rankings$Overall.Rank <- rank(rankings$Overall.Rank)
  
  rankings$Offense.Rank <- (1 + length(rankings$Offense.Score) - rank(rankings$Offense.Score))
  rankings$Defense.Rank <- (1 + length(rankings$Defense.Score) - rank(rankings$Defense.Score))
  
  rankings$Overall.Rank <- (1 + length(rankings$Overall.Score) - rank(rankings$Overall.Score))
  
  rankings <- data.frame(Team=rankings$Team, Offense <- rankings$Offense.Score, Defense <- rankings$Defense.Score,
                         Overall.Score = rankings$Overall.Score, Overall = rankings$Overall.Rank)
  
  return(rankings)
}
save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
}
