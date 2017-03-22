adjust <- function(row)
{ 
  row[["wscore"]] <-  as.numeric(row[["wscore"]]) /agg[agg$wteam==as.character(row[["lteam"]]),]$lscore
  row[["lscore"]] <-as.numeric( row[["lscore"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wscore
  row[["wfgm"]] <- as.numeric(row[["wfgm"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfgm
  row[["wfga"]] <- as.numeric(row[["wfga"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfga
  row[["wfgP"]] <- as.numeric(row[["wfgP"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfgP
  row[["wfgm3"]] <- as.numeric(row[["wfgm3"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfgm3
  row[["wfga3"]] <- as.numeric(row[["wfga3"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfga3
  row[["wfg3P"]] <- as.numeric(row[["wfg3P"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfg3P
  row[["wftm"]] <- as.numeric(row[["wftm"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lftm
  row[["wfta"]] <- as.numeric(row[["wfta"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lfta
  row[["wftP"]] <- as.numeric(row[["wftP"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lftP
  row[["wor"]] <- as.numeric(row[["wor"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lor
  row[["wdr"]] <- as.numeric(row[["wdr"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$ldr
  row[["wstl"]] <- as.numeric(row[["wstl"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lstl
  row[["wblk"]] <- as.numeric(row[["wblk"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lblk
  row[["wpf"]] <- as.numeric(row[["wpf"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lpf
  row[["wast"]] <- as.numeric(row[["wast"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$last
  row[["wto"]] <- as.numeric(row[["wto"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$lto
  row[["lfgm"]] <- as.numeric(row[["lfgm"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfgm
  row[["lfga"]] <- as.numeric(row[["lfga"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfga
  row[["lfgP"]] <- as.numeric(row[["lfgP"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfgP
  row[["lfgm3"]] <- as.numeric(row[["lfgm3"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfgm3
  row[["lfga3"]] <- as.numeric(row[["lfga3"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfga3
  row[["lfg3P"]] <- as.numeric(row[["lfg3P"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfg3P
  row[["lftm"]] <- as.numeric(row[["lftm"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wftm
  row[["lfta"]] <- as.numeric(row[["lfta"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wfta
  row[["lftP"]] <- as.numeric(row[["lftP"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wftP
  row[["lor"]] <- as.numeric(row[["lor"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wor
  row[["ldr"]] <- as.numeric(row[["ldr"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wdr
  row[["lstl"]] <- as.numeric(row[["lstl"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wstl
  row[["lblk"]] <- as.numeric(row[["lblk"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wblk
  row[["lpf"]] <- as.numeric(row[["lpf"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wpf
  row[["last"]] <- as.numeric(row[["last"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wast
  row[["lto"]] <- as.numeric(row[["lto"]])/agg[agg$wteam==as.character(row[["lteam"]]),]$wto
  
  return (row)
}

adjusted.stats.calculation <- function()
{
  schedule <- read.csv(file = 'data\\competition\\regular_season_detailed_results.csv', sep=',', header=T)
  schedule$wloc <- NULL
  adjusted.list <- list()
  for(y in min(schedule$season): max(schedule$season))
  {
    index <- 1 + (y-min(schedule$season))
    season <- schedule[schedule$season == y,]
    
    flipped <- data.frame(season = season$season, daynum = season$daynum, wteam=season$lteam, wscore=season$lscore,
                          lteam=season$wteam, lscore = season$wscore, numot=season$numot, wfgm = season$lfgm,
                          wfga = season$lfga, wfgm3 = season$lfgm3, wfga3 = season$lfga3, wftm = season$lftm,
                          wfta = season$lfta, wor = season$lor, wdr = season$ldr,  wstl=season$lstl, wast=season$last,
                          wto=season$lto, wblk=season$lblk, wpf=season$lpf,lfgm = season$wfga, lfga = season$wfga, 
                          lfgm3 = season$wfgm3, lfga3 = season$wfga3, lftm = season$wftm,
                          lfta = season$wfta, lor = season$wor, ldr = season$wdr,  lstl=season$wstl, 
                          lblk=season$wblk, lpf=season$wpf, last=season$wast, lto=season$wto)
    
    season <- rbind(season, flipped)
    rm(flipped)
    
    season$wfgP <- season$wfgm/season$wfga
    season$lfgP <- season$lfgm/season$lfga
    season$wfg3P <- season$wfgm3/season$wfga3
    season$lfg3P <- season$lfgm3/season$lfga3
    
    season$wftP <- season$wftm/season$wfta
    season$lftP <- season$lftm/season$lfta
    
    agg <- aggregate( cbind(wscore, lscore, wfgm, wfga, wfgm3, wfga3 , wftm, wfta, wor, wdr,  wstl, wast,
                                 wto, wblk, wpf,lfgm, lfga, lfgm3, lfga3, lftm,lfta, lor, ldr ,  lstl, 
                                 lblk, lpf, last, lto, wfgP, lfgP, wfg3P, lfg3P, wftP, lftP)~wteam, data=season, FUN=mean)
    
    adjustments <- apply(FUN=adjust, MARGIN=1, season)
    
    adjustments <- as.data.frame(t(adjustments))
    adjustments <- aggregate( cbind(wscore, lscore, wfgm, wfga, wfgm3, wfga3 , wftm, wfta, wor, wdr,  wstl, wast,
                            wto, wblk, wpf,lfgm, lfga, lfgm3, lfga3, lftm,lfta, lor, ldr ,  lstl, 
                            lblk, lpf, last, lto, wfgP, lfgP, wfg3P, lfg3P, wftP, lftP)~wteam, data=adjustments, FUN=mean)
    adjusted.list[index] <- list(adjustments)
  }
  
  return(adjusted.list)
}

mean.stats.calculation <- function()
{
  schedule <- read.csv(file = 'data\\competition\\regular_season_detailed_results.csv', sep=',', header=T)
  schedule$wloc <- NULL
  mean.list <- list()
  for(y in min(schedule$season): max(schedule$season))
  {
    index <- 1 + (y-min(schedule$season))
    season <- schedule[schedule$season == y,]
    
    flipped <- data.frame(season = season$season, daynum = season$daynum, wteam=season$lteam, wscore=season$lscore,
                          lteam=season$wteam, lscore = season$wscore, numot=season$numot, wfgm = season$lfgm,
                          wfga = season$lfga, wfgm3 = season$lfgm3, wfga3 = season$lfga3, wftm = season$lftm,
                          wfta = season$lfta, wor = season$lor, wdr = season$ldr,  wstl=season$lstl, wast=season$last,
                          wto=season$lto, wblk=season$lblk, wpf=season$lpf,lfgm = season$wfga, lfga = season$wfga, 
                          lfgm3 = season$wfgm3, lfga3 = season$wfga3, lftm = season$wftm,
                          lfta = season$wfta, lor = season$wor, ldr = season$wdr,  lstl=season$wstl, 
                          lblk=season$wblk, lpf=season$wpf, last=season$wast, lto=season$wto)
    
    season <- rbind(season, flipped)
    rm(flipped)
    
    season$wfgP <- season$wfgm/season$wfga
    season$lfgP <- season$lfgm/season$lfga
    season$wfg3P <- season$wfgm3/season$wfga3
    season$lfg3P <- season$lfgm3/season$lfga3
    
    season$wftP <- season$wftm/season$wfta
    season$lftP <- season$lftm/season$lfta
    
    agg <- aggregate( cbind(wscore, lscore, wfgm, wfga, wfgm3, wfga3 , wftm, wfta, wor, wdr,  wstl, wast,
                            wto, wblk, wpf,lfgm, lfga, lfgm3, lfga3, lftm,lfta, lor, ldr ,  lstl, 
                            lblk, lpf, last, lto, wfgP, lfgP, wfg3P, lfg3P, wftP, lftP)~wteam, data=season, FUN=mean)
    
      mean.list[index] <- list(agg)
  }
  
  return(mean.list)
}