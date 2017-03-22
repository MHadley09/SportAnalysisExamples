game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(.5, scoreA^3.3 / scoreB^3.3),2)
  return(.5 + (.692494*log(x)))
}

predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function()
{
  schedule <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  schedule$Visitor.Team <- trim(schedule$Visitor.Team)
  schedule$Home.Team <- trim(schedule$Home.Team)
  leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)
  
  schedule <- schedule[schedule$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
  
  schedule <- schedule[schedule$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
  
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                     Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)
  
  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                 Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  last.step.score <- rep(1500, length(teams))
  score <- rep(1500, length(teams))
  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  
  deviation <- sd(game.value(data$Score, data$Opp.Score))
  
  rm(teams,last.step.score,score)
  for(j in 1:10)
  {
    for(i in 1:length(rankings$Team))
    {
      opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
      scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
      oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
      gameScores <- game.value(scores, oppScores)
      oppScore <- rankings[rankings$Team %in% opps,]
      games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                     data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
      
      func <- function(x)
      {
        return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
      }
      rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
    }
    rankings$Previous.Score <- rankings$Score
  }
  
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                     Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)
  
  rankings$Home.Score <- rankings$Score
  for(i in 1:length(rankings$Team))
  {
    opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
    scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
    oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
    gameScores <- game.value(scores, oppScores)
    oppScore <- rankings[rankings$Team %in% opps,]
    games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
                   data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
    
    func <- function(x)
    {
      return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
    }
    rankings[i,]$Home.Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
  }
    
  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  rankings$Home.Adv <- rankings$Home.Score - rankings$Score
  rankings$Home.Adv.Rank <-  (1 + length(rankings$Home.Adv) - rank(rankings$Home.Adv))
  return(rankings)
}

save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
 }
 
# calculate.rankings <- function()
# {
#   schedule <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
#   trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#   
#   schedule$Visitor.Team <- paste(trim(schedule$Visitor.Team), "Home", sep = " - ")
#   schedule$Home.Team <- paste(trim(schedule$Home.Team), "Visitor", sep = " - ")
#   leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)
#   leaguesA <- leagues 
#   leagues$Team <- paste(trim(leagues$Team), "Home", sep = " - ")
#   leaguesA$Team <- paste(trim(leaguesA$Team), "Visitor", sep = " - ")
#   leagues <- rbind(leagues, leaguesA)
#   rm(leaguesA)
#   schedule <- schedule[schedule$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
#   
#   schedule <- schedule[schedule$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
#   
#   
#   data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
#                      Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)
#   
#   data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
#                                  Opp = data$Team, Opp.Score = data$Score))
#   
#   teams <- unique(data$Team)
#   last.step.score <- rep(1600, length(teams))
#   score <- rep(1600, length(teams))
#   
#   rm(schedule)
#   
#   
#   
#   rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
#   
#   deviation <- sd(game.value(data$Score, data$Opp.Score))
#   
#   rm(teams,last.step.score,score)
#   for(j in 1:10)
#   {
#     for(i in 1:length(rankings$Team))
#     {
#       opps <- data[data$Team==as.character(rankings[i,]$Team),]$Opp
#       scores <- data[data$Team==as.character(rankings[i,]$Team),]$Score
#       oppScores <-  data[data$Team==as.character(rankings[i,]$Team),]$Opp.Score
#       gameScores <- game.value(scores, oppScores)
#       oppScore <- rankings[rankings$Team %in% opps,]
#       games <- merge(data.frame(Opp=opps, Game.Value=gameScores), 
#                      data.frame(Opp=oppScore$Team, Opp.Value=oppScore$Previous.Score), by="Opp")
#       
#       func <- function(x)
#       {
#         return(mean(log.likelihood(x, games$Opp.Value, games$Game.Value, deviation)))
#       }
#       rankings[i,]$Score <- optimize(f=func, interval=c(0,3500), maximum=T)$maximum
#     }
#     rankings$Previous.Score <- rankings$Score
#   }
#   temp <- rankings
#   temp$Home.Score <- 0
#   temp$Visitor.Score <- 0
#   home <- grepl("Home",temp$Team,)
#   temp[home,]$Home.Score <- temp[home,]$Score
#   temp[!home,]$Visitor.Score <- temp[!home,]$Score 
#   temp$Team <- sub(" - Home","", temp$Team)
#   temp$Team <- sub(" - Visitor","", temp$Team)
#   rankings <- aggregate(cbind(Score, Home.Score, Visitor.Score)~Team, data=temp, FUN=sum)
#   rankings$Score <- rankings$Score/2
#   rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
#   return(rankings)
# }
# 
# save.rankings <- function()
# {
#   fname <- file.choose()
#   print("Running rankings.  Please wait.")
#   write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
# }
