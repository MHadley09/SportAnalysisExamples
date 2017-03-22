composite.ranking.typed <- function(predictType, count)
{
  source('SoS_played.r')
  box.data <- read.csv(file = 'data\\box_data.csv', sep=',', header=T)
  source('ranking.r')
  calc.ranking <- calculate.rankings.typed(predictType, count)
  source('elo.r')
  elo.ranking <- elo.ranking()
  
  offense <- read.csv(file = 'data\\offense.csv', sep=',', header=T)
  defense <- read.csv(file = 'data\\defense.csv', sep=',', header=T)

  FBS.RECORDs <- FBS.SoS()
  
  combine <- merge(offense, defense, by="TEAM")
  combine <- merge(combine, elo.ranking, by="TEAM")
  combine <- merge(combine, calc.ranking, by="TEAM")
  combine <- merge(combine, FBS.RECORDs, by="TEAM")
  
  combine$ELO.SCORE <- (combine$ELO - min(combine$ELO))/(max(combine$ELO)-min(combine$ELO))
  combine$OFF.YARDS.SCORE <- (combine$YDS.G - min(combine$YDS.G))/(max(combine$YDS.G)-min(combine$YDS.G))
  combine$DEF.YARDS.SCORE <- 1-(combine$D.YDS.G - min(combine$D.YDS.G))/(max(combine$D.YDS.G)-min(combine$D.YDS.G))
  combine$OFF.POINTS.SCORE <- (combine$PTS.G - min(combine$PTS.G))/(max(combine$PTS.G)-min(combine$PTS.G))
  combine$DEF.POINTS.SCORE <- 1-(combine$D.PTS.G - min(combine$D.PTS.G))/(max(combine$D.PTS.G)-min(combine$D.PTS.G))
  combine$RECORD <- combine$OVERALL.W/(combine$OVERALL.W + combine$OVERALL.L)
  combine$SOS.SCORE <- combine$SoS.Score
  combine$RECORD.SCORE <- (combine$RECORD - min(combine$RECORD))/(max(combine$RECORD)-min(combine$RECORD))
  combine$POWER.SCORE <- (combine$SIM.PERC - min(combine$SIM.PERC))/(max(combine$SIM.PERC)-min(combine$SIM.PERC))
  combine$ADJUSTED.RECORD <- combine$SoS.MULT*combine$WIN.PERC
  combine$ADJUSTED.RECORD.SCORE <- (combine$ADJUSTED.RECORD - min(combine$ADJUSTED.RECORD))/(max(combine$ADJUSTED.RECORD)-min(combine$ADJUSTED.RECORD))
  combine$COMBINE.SCORE <- ((0.15*combine$ELO.SCORE)+(0.025*combine$OFF.YARDS.SCORE)+(0.025*combine$DEF.YARDS.SCORE)+
                              (0.05*combine$OFF.POINTS.SCORE)+(0.05*combine$DEF.POINTS.SCORE)+
                              (0.075*combine$RECORD.SCORE)+(0.05*combine$SOS.SCORE)+
                              (0.2*combine$POWER.SCORE)+(0.4*combine$ADJUSTED.RECORD.SCORE))
                              
  combine$COMBINE.SCORE <-  combine$COMBINE.SCORE
  combine$COMBINE.RANKING <- (length(combine$COMBINE.SCORE)+1) - rank(combine$COMBINE.SCORE)
  
  return(combine[,names(combine) %in% c("TEAM", "ELO.SCORE","OFF.YARDS.SCORE","DEF.YARDS.SCORE",
                                        "OFF.POINTS.SCORE", "DEF.POINTS.SCORE", "RECORD.SCORE",
                                        "SOS.SCORE","POWER.SCORE", "ADJUSTED.RECORD.SCORE",
                                        "COMBINE.SCORE", "COMBINE.RANKING")])
}
composite.rankings <- function(count)
{
  return(rankings<-composite.ranking.typed('better_complex_predict.r', count))
}