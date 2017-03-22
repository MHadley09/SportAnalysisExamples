predict.score <- function(offense, defense, offense.adjust, defense.adjust)
{
  return(((offense*defense.adjust)+(offense.adjust*defense))/2)
}


log.likelihood <- function(offense, defense, offense.adjust, defense.adjust, score, deviation)
{
  return((dnorm(predict.score(offense, defense, offense.adjust, defense.adjust), score, deviation, log=T)))
}

# log.likelihood <- function(x, defense, offense.adjust, defense.adjust, score, deviation)
# {
#   return((dnorm(predict.score(x, defense, offense.adjust, defense.adjust), score, deviation, log=T)))
# }
# log.likelihood.defense <- function(x, offense, offense.adjust, defense.adjust, score, deviation)
# {
#   return((dnorm(predict.score(offense, x, offense.adjust, defense.adjust), score, deviation, log=T)))
# }
# log.likelihood.offense.adjust <- function(x, offense, defense, defense.adjust, score, deviation)
# {
#   return((dnorm(predict.score(offense, defense, x, defense.adjust), score, deviation, log=T)))
# }
# log.likelihood.defense.adjust <- function(x, defense, offense.adjust, defense.adjust, score, deviation)
# {
#   return((dnorm(predict.score(offense, defense, offense.adjust, x), score, deviation, log=T)))
# }

calculate.rankings <- function()
{
  schedule <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)

#  divisions <-read.csv(file = 'data\\2005\\divisionBreakdown.csv', sep=',', header=T)
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                   Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)

  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                  Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  rankings <- data.frame(Team=teams)
  rankings$Last.Step.Offense <- 28
  rankings$Offense <- 28
  rankings$Last.Step.Defense <- 28
  rankings$Defense <- 28
  rankings$Last.Step.Offense.Adjust <-1
  rankings$Offense.Adjust <- 1
  rankings$Last.Step.Defense.Adjust <-1
  rankings$Defense.Adjust <- 1
  
  rm(schedule)
  

  divisions <- read.csv(file = 'data\\2015\\ConferenceWithFCS.csv', sep=',', header=T)
  FBS <- data.frame(Team=divisions[divisions$Conference != "FCS",]$TEAM)
  FBS <- merge(FBS, rankings, by="Team")
  
  rankings <- FBS

  data <- data[data$Team %in% as.character(rankings$Team),]
  data <- data[data$Opp %in% as.character(rankings$Team),]

  deviation <- sd(c(data$Score, data$Opp.Score))

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
                                Opp.Defense=oppScore$Last.Step.Defense, 
                                Opp.Offense.Adjust=oppScore$Last.Step.Offense.Adjust,
                                Opp.Defense.Adjust=oppScore$Last.Step.Defense.Adjust), by="Opp")
      
      funcOff <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Defense,  rankings[i,]$Last.Step.Offense.Adjust,
                                           games$Opp.Defense.Adjust, games$Score, deviation)))
      }
      rankings[i,]$Offense <- optimize(f=funcOff, interval=c(0,100), maximum=T)$maximum
      
      funcDef <- function(x)
      {
        return(mean(log.likelihood(games$Opp.Offense, x, games$Opp.Offense.Adjust,
                                   rankings[i,]$Last.Step.Defense.Adjust, games$Opp.Score, deviation)))
      }
      rankings[i,]$Defense <- optimize(f=funcDef, interval=c(0,100), maximum=T)$maximum
      
      funcOffAdjust <- function(x)
      {
        return(mean(log.likelihood(rankings[i,]$Last.Step.Offense, games$Opp.Defense, x,
                                           games$Opp.Defense.Adjust, games$Score, deviation)))
      }
      rankings[i,]$Offense.Adjust <- optimize(f=funcOffAdjust, interval=c(0,3), maximum=T)$maximum
      
      funcDefAdjust <- function(x)
      {
        return(mean(log.likelihood(games$Opp.Offense, rankings[i,]$Last.Step.Defense, 
                                           games$Opp.Offense.Adjust, x, games$Opp.Score, deviation)))
      }
      rankings[i,]$Defense.Adjust <- optimize(f=funcDefAdjust, interval=c(0,3), maximum=T)$maximum
      
    }
    rankings$Last.Step.Offense <- rankings$Offense
    rankings$Last.Step.Defense <- rankings$Defense
    rankings$Last.Step.Offense.Adjust <- rankings$Offense.Adjust
    rankings$Last.Step.Defense.Adjust <- rankings$Defense.Adjust
  }

  rankings$Offense.Rank <- (1 + length(rankings$Offense) - rank(rankings$Offense))
  rankings$Offense.Adjust.Rank <- (1 + length(rankings$Offense.Adjust) - rank(rankings$Offense.Adjust))
  rankings$Defense.Rank <- (1 + length(rankings$Defense) - rank(rankings$Defense))
  rankings$Defense.Adjust.Rank <- (1 + length(rankings$Defense.Adjust) - rank(rankings$Defense.Adjust))
  rankings$Overall.Rank <- rank(m
  return(rankings)
}

calculate.FBS.Rankings <- function()
{
  rankings <- calculate.rankings()
  divisions <- read.csv(file = 'data\\2015\\ConferenceWithFCS.csv', sep=',', header=T)
  FBS <- data.frame(Team=divisions[divisions$Conference != "FCS",]$TEAM)
  FBS <- merge(FBS, rankings, by="Team")
  FBS$Offense.Rank <- rank(FBS$Offense.Rank)
  FBS$Defense.Rank <- rank(FBS$Defense.Rank)
  FBS$Overall.Rank <- rank(FBS$Overall.Rank)
  
  return(FBS)
}
save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
}
