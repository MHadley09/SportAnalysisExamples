calculate.rankings.typed <- function(predictType, count)
{
source('Py_SoS_played.r')
source(predictType)
box.data <- read.csv(file = 'data\\2016\\perplaydata.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
box.data$Home.Team <- trim(box.data$Home.Team)
FBS.Record.List <- FBS.SoS()
FBS.Record.List <- FBS.Record.List[FBS.Record.List$TEAM %in% as.character(box.data$Home.Team),]
TEAM <- FBS.Record.List$TEAM
WIN.PERC <- FBS.Record.List$OVERALL.W/(FBS.Record.List$OVERALL.W+FBS.Record.List$OVERALL.L)
SoS.MULT <- 0.75 + (FBS.Record.List$SoS.Score/4)
WIN.MULT <- 0.5 + (WIN.PERC/2)
SIM.WINS <-  rep(0, length(FBS.Record.List$TEAM))
SIM.LOSSES <- rep(0, length(FBS.Record.List$TEAM))
SIM.PERC <-  rep(0, length(FBS.Record.List$TEAM))
RANK.SCORE <-  rep(0, length(FBS.Record.List$TEAM))
RANK <-  rep(0, length(FBS.Record.List$TEAM))

rankings <- data.frame(TEAM, WIN.PERC, WIN.MULT, SoS.MULT, SIM.WINS, SIM.LOSSES, SIM.PERC, RANK.SCORE)
rm(WIN.PERC,SoS.MULT,SIM.WINS, SIM.LOSSES, SIM.PERC,RANK.SCORE,RANK)
rankings <- rankings[!is.na(rankings$TEAM),]



for(l in 1:(length(rankings$TEAM)-1))
{
  for(m in (l+1):length(rankings$TEAM))
  {
    hName <- as.character(rankings[l,]$TEAM)
    aName <- as.character(rankings[m,]$TEAM)
    
    ranking.temp <- neutral.predict(hName, aName, count)
    
    rankings[l,]$SIM.WINS <- rankings[l,]$SIM.WINS + ranking.temp[1,]$WINS
    rankings[l,]$SIM.LOSSES <- rankings[l,]$SIM.LOSSES + ranking.temp[2,]$WINS
    
    rankings[m,]$SIM.WINS <- rankings[m,]$SIM.WINS + ranking.temp[2,]$WINS                                
    rankings[m,]$SIM.LOSSES <- rankings[m,]$SIM.LOSSES + ranking.temp[1,]$WINS
  }
  rm(m)
}
rm(l)


rankings$SIM.PERC <- rankings$SIM.WINS / (rankings$SIM.WINS + rankings$SIM.LOSSES)
rankings$RANK.SCORE <- (rankings$SIM.PERC*(rankings$WIN.MULT*rankings$SoS.MULT))
rankings$RANK <-  (length(rankings$TEAM)+1) - rank(rankings$RANK.SCORE)
rankings$SIMURANK <-  (length(rankings$TEAM)+1) - rank(rankings$SIM.PERC)
rankings$ALT.RANK.SCORE <- (rankings$SIM.PERC+((2*rankings$WIN.PERC)*rankings$SoS.MULT))/3
rankings$ALT.RANK <-  (length(rankings$TEAM)+1) - rank(rankings$ALT.RANK.SCORE)

return(rankings)
}

calculate.rankings <- function(count)
{
  return(rankings<-calculate.rankings.typed('play_by_play_predict_with_no_zero.R', count))
}

save.rankings <- function(count)
{
  fname <- file.choose()
  print("Running rankings.  Please wait.")
  write.table(calculate.rankings(count), fname, row.names=FALSE, sep=",") 
}