source("comp_mle_split_home_ranking.R")
source("determine_tourny_off.R")

calculate.Value <- function(team, season, rank)
{
  score <- 1:length(team)
  delta <- 1:length(team)
  mult <- 1:length(team)
  deltaMulti <-  0.2303881
  firstMulti <-   0.006261896#0.07508172
  secondMulti <-   0.07295033#0.08010252 
  thirdMulti <-   4.102259e-05#6.610696e-05
  for(r in 1:length(team))
  {
    row <- rank[rank$Team == team[r],]
    deltas <- as.list(row[,grepl("Delta*",colnames(row))& colnames(row) != paste0("Delta",season[r])])
    delta[r] <- ifelse(length(unlist(deltas[deltas != 0]))>=2,mean(unlist(deltas[deltas != 0])),0)
    #delta[r] <- sign(delta[r])*(sqrt(abs(delta[r])))
    score[r] <- (row[, colnames(row)==paste0("Score",season[r])])
    mult[r] <- 1
    if(row[, colnames(row)==paste0("WithTournyScore",season[r]-1)] != 0)
    {
      score[r] <- (firstMulti*row[, colnames(row)==paste0("WithTournyScore",season[r]-1)]) + score[r]
      mult[r] <- mult[r] + firstMulti
    }
    if(row[, colnames(row)==paste0("WithTournyScore",season[r]-2)] != 0)
    {
      score[r] <- (secondMulti*row[, colnames(row)==paste0("WithTournyScore",season[r]-2)]) + score[r]
      mult[r] <- mult[r] + secondMulti
      
    }
    
    if(row[, colnames(row)==paste0("WithTournyScore",season[r]-3)] != 0)
    {
      score[r] <- thirdMulti*row[, colnames(row)==paste0("WithTournyScore",season[r]-3)] + score[r]
      mult[r] <- mult[r] + thirdMulti
    }
    
#     if(row[, colnames(row)==paste0("WithTournyScore",season[r]-4)] != 0)
#     {
#       score[r] <- fourthMulti*row[, colnames(row)==paste0("WithTournyScore",season[r]-4)] + score[r]
#       mult[r] <- mult[r] + fourthMulti
#     }
#     if(row[, colnames(row)==paste0("WithTournyScore",season[r]-5)] != 0)
#     {
#       score[r] <- fifthMulti*row[, colnames(row)==paste0("WithTournyScore",season[r]-5)] + score[r]
#       mult[r] <- mult[r] + fifthMulti
#     }
    score[r] <- score[r]/mult[r] + (deltaMulti*delta[r])
  }
  return(score)
}
grab.value <- function(team, season, adjustments, id)
{
  adjustments <- adjusted.list
  merged.frame <- data.frame(wteam=team,season=season)
  adjust <- adjustments[[1]]
  frame <- merge(merged.frame[merged.frame$season == min(merged.frame$season),], 
                 adjust[merged.frame[merged.frame$season == min(merged.frame$season),]$team == adjust$team,], by="wteam")
  
  for(i in 2:length(unique(merged.frame$season)))
  {
    index <- (unique(merged.frame$season)[i]-2003)+1
    adjust <- adjustments[[i]]
    frame <- rbind(frame, merge(merged.frame[merged.frame$season == min(merged.frame$season)+(i-1),], adjust, by="wteam"))
  }
  return(frame)
}

merge.frames <- function(team, season, mean.frame, id)
{

  merged.frame <- data.frame(wteam=team,season=season)
  m.frame <- mean.frame[[1]]
  frame <- merge(merged.frame[merged.frame$season == min(merged.frame$season),], 
                 m.frame[merged.frame[merged.frame$season == min(merged.frame$season),]$team == m.frame$team,], by="wteam")
  
  for(i in 2:length(unique(merged.frame$season)))
  {
    index <- (unique(merged.frame$season)[i]-2003)+1
    m.frame <- mean.frame[[i]]
    frame <- rbind(frame, merge(merged.frame[merged.frame$season == min(merged.frame$season)+(i-1),], m.frame, by="wteam"))
  }
  return(frame)
}

slots <- read.csv(file = 'data\\competition\\tourney_slots.csv', sep=',', header=T)
seeds <- read.csv(file = 'data\\competition\\tourney_seeds.csv', sep=',', header=T)
teams <- read.csv(file = 'data\\competition\\team_spellings.csv', sep=',', header=T)

schedule <- read.csv(file = 'data\\competition\\regular_season_compact_results.csv', sep=',', header=T)
tournament <- read.csv(file = 'data\\competition\\tourney_compact_results.csv', sep=',', header=T)

ranks <- tourny.offset()

games <- tournament[tournament$season >= 2003,]
#games <- schedule[schedule$season >= 2007,]

#games <- tournament[tournament$season >= 201 & tournament$season <= 2014,]


#gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Home.Value <- gamechecker[gamechecker$Home.Score < gamechecker$Neutral.Value,]$Neutral.Value
gamechecker <- games
gamechecker$Winner.Pred <- 0
gamechecker <- unique(merge(gamechecker, unique(grab.value(gamechecker$wteam, gamechecker$season,  adjusted.list, "W")), by=c("wteam", "season")))
loser.values <- unique(grab.value(gamechecker$lteam, gamechecker$season,  adjusted.list, "L"))
colnames(loser.values)[colnames(loser.values) == "wteam" ] <- "lteam"
gamechecker <- unique(merge(gamechecker, loser.values, by=c("lteam", "season")))
 gamechecker$wscore.x <- gamechecker$wscore.y
 gamechecker$lscore.x <- gamechecker$lscore.y
 gamechecker$wscore.y <- gamechecker$wscore
 gamechecker$lscore.y <- gamechecker$lscore
gamechecker <- unique(merge(gamechecker, unique(merge.frames(gamechecker$wteam, gamechecker$season,  mean.list)), by=c("wteam", "season")))

loser.values <- unique(merge.frames(gamechecker$lteam, gamechecker$season,  mean.list))
colnames(loser.values)[colnames(loser.values) == "wteam" ] <- "lteam"

gamechecker <- unique(merge(gamechecker, loser.values, by=c("lteam", "season")))


fit.func <- function(x)
{

  adjust <-1.09199
  gamechecker$Winner.Pred <- 0
  
  gamechecker$Winner.Value <- calculate.Value(gamechecker$wteam, gamechecker$season, ranks)
  gamechecker$Loser.Value <- calculate.Value(gamechecker$lteam, gamechecker$season, ranks) # gamechecker$wscore.y +
  gamechecker$Winner.Value.A <- ((gamechecker$wscore.x*gamechecker$Mean.lscore.y)
                                 +(gamechecker$lscore.y*gamechecker$Mean.wscore.x))/2 
  gamechecker$Loser.Value.A <- ((gamechecker$wscore.y*gamechecker$Mean.lscore.x)
                                +(gamechecker$lscore.x*gamechecker$Mean.wscore.y))/2 
  
   firstBalance <-  1
   secondBalance <- 1
#    thirdBalance <- .2942689 #0
     gamechecker$Winner.Prob <- (firstBalance)*predict(gamechecker$Winner.Value, gamechecker$Loser.Value)
     gamechecker$Winner.Prob <-   ((secondBalance*pmax(.00005, pmin(.99995, ifelse(gamechecker$Winner.Value.A > gamechecker$Loser.Value.A, 
                                           pmax(.5, 0.3912+0.1801*log(pmax(0.00001,gamechecker$Winner.Value.A-gamechecker$Loser.Value.A))),
                                          pmin(.5, 1-(0.3912+0.1801*log(pmax(0.0001,gamechecker$Loser.Value.A-gamechecker$Winner.Value.A))))))))
                                    +gamechecker$Winner.Prob)

#   gamechecker$Winner.Prob <-   ((thirdBalance*ifelse(gamechecker$Winner.Value.T > gamechecker$Loser.Value.T, 
#                                                         .5329+1.2880*(gamechecker$Winner.Value.T-gamechecker$Loser.Value.T),
#                                                           -1*(.5329+1.2880*(gamechecker$Loser.Value.T-gamechecker$Winner.Value.T))))
#                                                      +gamechecker$Winner.Prob)
#   
   gamechecker$Winner.Prob <- pmax(.00005, pmin(.99995, gamechecker$Winner.Prob / (firstBalance+secondBalance)))

    gamechecker$Winner.Prob <- pmax(0.00005,pmin(.99995,
                                       ifelse(gamechecker$Winner.Prob > .5, 
                                              adjust*gamechecker$Winner.Prob,
                                              1-(adjust*(1-gamechecker$Winner.Prob)))))

  gamechecker[gamechecker$Winner.Prob > .5,]$Winner.Pred <- 1

  gamechecker$correct <- 0
  gamechecker[gamechecker$Winner.Pred == 1,]$correct <- 1
  mean(gamechecker$correct)
  mean(gamechecker$Winner.Prob)
  return( -1*mean(log(gamechecker$Winner.Prob)))
}

best <- optimize(f = fit.func, interval = c(0.01,2.5))

test <- data.frame(correct=gamechecker$correct, prob=gamechecker$Winner.Prob)

agg <- aggregate(cbind(correct)~prob, data = test, FUN=mean)

breakdown <- agg[1:10,]
for(j in 1:10)
{
  per <- .07*j#+2
  breakdown[j,]$prob<-mean(test[test$prob >= per-.07 & test$prob < per,]$prob)
  breakdown[j,]$correct<-mean(test[test$prob >= per-0.07 & test$prob < per,]$correct)
}
breakdown$prob <- log(breakdown$prob)

plot(breakdown)  
line <- lm(correct~prob,data=breakdown)
abline(line,col="red")
summary(line,data=breakdown)

for(i in 1:length(mean.list))
{
  colnames(mean.list[[i]])[colnames(mean.list[[i]]) == "Mean.wteam"] <- "wteam"
  colnames(mean.list[[i]])[colnames(mean.list[[i]]) == "Mean.season"] <- "season"
  
}
