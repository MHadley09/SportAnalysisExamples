source('SoSplayed.r')
source('predictor_function.r')
box.data <- read.csv(file = 'data\\box_data.csv', sep=',', header=T)

Bball.Records <- sos_calc()
TEAM <- Bball.Records$TEAM
WIN.PERC <- Bball.Records$OVERALL.W/(Bball.Records$OVERALL.W+Bball.Records$OVERALL.L)
SoS.MULT <- 0.75 + (Bball.Records$SoS.Score/4)
SIM.WINS <-  rep(0, length(Bball.Records$TEAM))
SIM.LOSSES <- rep(0, length(Bball.Records$TEAM))
SIM.PERC <-  rep(0, length(Bball.Records$TEAM))
RANK.SCORE <-  rep(0, length(Bball.Records$TEAM))
RANK <-  rep(0, length(Bball.Records$TEAM))

rankings <- data.frame(TEAM, WIN.PERC, SoS.MULT, SIM.WINS, SIM.LOSSES, SIM.PERC, RANK.SCORE)
madRankings  <- data.frame(TEAM, WIN.PERC, SoS.MULT, SIM.WINS, SIM.LOSSES, SIM.PERC, RANK.SCORE)
rm(WIN.PERC,SoS.MULT,SIM.WINS, SIM.LOSSES, SIM.PERC,RANK.SCORE,RANK)
rankings <- rankings[!is.na(rankings$TEAM),]

count <- 20

for(l in 1:(length(rankings$TEAM)-1))
{
  for(m in (l+1):length(rankings$TEAM))
  {
      hName <- as.character(rankings[l,]$TEAM)
      aName <- as.character(rankings[m,]$TEAM)
      
      ranking.temp <- neutral.complex.predict(hName, aName, count)
      
      rankings[l,]$SIM.WINS <- rankings[l,]$SIM.WINS + ranking.temp[1,]$WINS
      rankings[l,]$SIM.LOSSES <- rankings[l,]$SIM.LOSSES + ranking.temp[2,]$WINS
      
      rankings[m,]$SIM.WINS <- rankings[m,]$SIM.WINS + ranking.temp[2,]$WINS                                
      rankings[m,]$SIM.LOSSES <- rankings[m,]$SIM.LOSSES + ranking.temp[1,]$WINS
  }
  rm(m)
}
rm(l)


rankings$SIM.PERC <- rankings$SIM.WINS / (rankings$SIM.WINS + rankings$SIM.LOSSES)
rankings$RANK.SCORE <- (rankings$SIM.PERC+((2*rankings$WIN.PERC)*rankings$SoS.MULT))/3
rankings$RANK.SCORE.WITHOUT.SOS <-  (rankings$SIM.PERC+(2*rankings$WIN.PERC))/3
rankings$RANK <-  (length(rankings$TEAM)+1) - rank(rankings$RANK.SCORE)
rankings$RANK.WITHOUT.SOS <- (length(rankings$TEAM)+1) - rank(rankings$RANK.SCORE.WITHOUT.SOS)
rankings$RANK.DIFFERENCE <- rankings$RANK - rankings$RANK.WITHOUT.SOS
rankings$SIMURANK <-  (length(rankings$TEAM)+1) - rank(rankings$SIM.PERC)


temp <- data.frame(FBS.Records$TEAM, FBS.Records$Conference)
names(temp)[names(temp)=="FBS.Records.TEAM"] <- "TEAM"
names(temp)[names(temp)=="FBS.Records.Conference"] <- "Conference"

rankings <- merge(rankings, temp, by="TEAM")

