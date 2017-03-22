game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return (.5 - (.0000270209*x^3) + (0.0352489*x))
}

predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

log.likelihood <- function(x, oppElo, game.value, deviation)
{
  return((dnorm(predict(x, oppElo), game.value, deviation, log=T)))
}

calculate.rankings <- function(year)
{
  schedule <- read.csv(file = paste0('data\\',year,'\\perplaydata.csv'), sep=',', header=T)
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  schedule$Visitor.Team <- trim(schedule$Visitor.Team)
  schedule$Home.Team <- trim(schedule$Home.Team)
  
  schedule <- schedule[!schedule$Home.Team == "",]
  data <- data.frame(Team = schedule$Home.Team, Score = schedule$Home.Score, 
                   Opp = schedule$Visitor.Team, Opp.Score = schedule$Visitor.Score)

  data <- rbind(data, data.frame(Team = data$Opp, Score = data$Opp.Score, 
                                  Opp = data$Team, Opp.Score = data$Score))
  
  teams <- unique(data$Team)
  last.step.score <- rep(1600, length(teams))
  score <- rep(1600, length(teams))
  
  rm(schedule)
  

  
  rankings <- data.frame(Team=teams, Previous.Score = last.step.score, Score = score)
  
#   rankings[rankings$Team %in% divisions[divisions$Division=="FBS P5",]$Team,]$Previous.Score <- 1800
#   rankings[rankings$Team %in% divisions[divisions$Division=="FBS G5",]$Team,]$Previous.Score <- 1600
#   rankings[rankings$Team %in% divisions[divisions$Division=="FCS",]$Team,]$Previous.Score <- 1400
#   rankings[rankings$Team %in% divisions[divisions$Division=="DivII",]$Team,]$Previous.Score <- 1000
#   rankings[rankings$Team %in% divisions[divisions$Division=="DivIII",]$Team,]$Previous.Score <- 600
#   
#   rankings[rankings$Team %in% divisions[divisions$Division=="FBS P5",]$Team,]$Score <- 1800
#   rankings[rankings$Team %in% divisions[divisions$Division=="FBS G5",]$Team,]$Score <- 1600
#   rankings[rankings$Team %in% divisions[divisions$Division=="FCS",]$Team,]$Score <- 1400
#   rankings[rankings$Team %in% divisions[divisions$Division=="DivII",]$Team,]$Score <- 1000
#   rankings[rankings$Team %in% divisions[divisions$Division=="DivIII",]$Team,]$Score <- 600
  
  deviation <- sd(game.value(data$Score, data$Opp.Score), na.rm=T)
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

  rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
  return(rankings)
}

 calculate.FBS.Rankings <- function(year)
 {
   rankings <- calculate.rankings(year)
   divisions <- read.csv(file = paste0('data\\',year,'\\conferenceWithFCS.csv'), sep=',', header=T)
   temp <- rankings[!(rankings$Team %in% divisions[divisions$Division=="FCS",]$Team),]
   rankings <- rbind(temp, rankings[rankings$Team %in% divisions[divisions$Division=="FBS G5",]$Team,])
   rankings$Rank <- (1 + length(rankings$Score) - rank(rankings$Score))
   return(rankings)
 }
save.rankings <- function()
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.FBS.Rankings(), fname, row.names=FALSE, sep=",") 
}
