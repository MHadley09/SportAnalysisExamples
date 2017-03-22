source("comp_mle_split_home_ranking.R")
source("determine_tourny_off.R")

determine.score<- function(team, season, rank)
{
  score <- 0
  delta <-0
  mult <- 0
  deltaMulti <-  .0.17#0.2303881
  firstMulti <-   0.07508172 #0.006261896
  secondMulti <-   0.08010252 #0.07295033
  thirdMulti <-   6.610696e-05 #4.102259e-05
  row <- rank[rank$Team == team,]
  deltas <- as.list(row[,grepl("Delta*",colnames(row)) & colnames(row) != paste0("Delta",season)])
  delta <- ifelse(length(unlist(deltas[deltas != 0]))>=2,mean(unlist(deltas[deltas != 0])),0)
  score <- (row[, colnames(row)==paste0("Score",season)])
  mult <- 1
  if(row[, colnames(row)==paste0("WithTournyScore",season-1)] != 0)
  {
    score <- (firstMulti*row[, colnames(row)==paste0("WithTournyScore",season-1)]) + score
    mult <- mult + firstMulti
  }
  if(row[, colnames(row)==paste0("WithTournyScore",season-2)] != 0)
  {
    score <- (secondMulti*row[, colnames(row)==paste0("WithTournyScore",season-2)]) + score
    mult <- mult + secondMulti
    
  }
  
  if(row[, colnames(row)==paste0("WithTournyScore",season-3)] != 0)
  {
    score <- thirdMulti*row[, colnames(row)==paste0("WithTournyScore",season-3)] + score
    mult <- mult + thirdMulti
  }
  
  
  score <- score/mult + (deltaMulti*delta)
  return(score)
}

evaluate.slot <- function(slotNum, rank, slots,seeds, year)
{
  rank$Score <- rank[,colnames(rank) == paste0("Score",year)]
  slot <- slots[slotNum,]
  sub.slots.a <- slots[slots$slot == as.character(slot$strongseed),]
  sub.slots.b <- slots[slots$slot == as.character(slot$weakseed),]
  teamsA <- c(as.character(sub.slots.a$strongseed), as.character(sub.slots.a$weakseed))
  teamsB <- c(as.character(sub.slots.b$strongseed), as.character(sub.slots.b$weakseed))
  teamsAPrime <- list()
  teamsBPrime <- list()
  
  if(nrow(sub.slots.a)==0)
  {
    teamsAPrime[[1]] <- as.character(slot$strongseed)
  }
  if(nrow(sub.slots.b)==0)
  {
    teamsBPrime[[1]] <- as.character(slot$weakseed)
  }
  while(nrow(sub.slots.a) > 0)
  {
    teamsAPrime[[length(teamsAPrime)+1]] <- list(teamsA)
    sub.slots.a <- slots[slots$slot %in% teamsA,]
    teamsA <- c()
    if(nrow(sub.slots.a) == 0)
    {
      break
    }
    for(j in 1:nrow(sub.slots.a))
    {
      teamsA[length(teamsA)+1] <- as.character(sub.slots.a[j,]$strongseed)
      teamsA[length(teamsA)+1] <- as.character(sub.slots.a[j,]$weakseed)   
    }
  }
  while(nrow(sub.slots.b) > 0)
  {
    teamsBPrime[[length(teamsBPrime)+1]] <- list(teamsB)
    sub.slots.b <- slots[slots$slot %in% teamsB,]
    teamsB <- c()
    if(nrow(sub.slots.b) == 0)
    {
      break
    }
    for(j in 1:nrow(sub.slots.b))
    {
      teamsB[length(teamsB)+1] <- as.character(sub.slots.b[j,]$strongseed)
      teamsB[length(teamsB)+1] <- as.character(sub.slots.b[j,]$weakseed)   
    }
  }
  teamsA <- unlist(teamsAPrime)
  teamsB <- unlist(teamsBPrime)
  game_ids <- c("remove")
  game_pred <- c(0)
  idsA <- seeds[seeds$seed %in% as.character(teamsA),]$team
  idsB <- seeds[seeds$seed %in% as.character(teamsB),]$team
  idsA <- idsA[!is.na(idsA)]
  idsB <- idsB[!is.na(idsB)]
  for(k in 1:length(idsA))
  {
    idA <- idsA[k]
    
    scoreA <- determine.score(idA, year, rank)
    

    for(l in 1:length(idsB))
    {
      idB <- idsB[l]
      
      scoreB <-  determine.score(idB, year, rank)
 
      if(idA < idB)
      {
        
        row <- length(game_ids)+1
        game_ids[row] <- paste0(year, "_", idA, "_", idB)[1]
        game_pred[row] <- (predict(scoreA, scoreB))
      }else if(idB < idA)
      {
        row <- length(game_ids)+1
        game_ids[row] <- paste0(year, "_", idB, "_", idA)[1]
        game_pred[row] <- ((predict(scoreB, scoreA)))
      }      
    }
  }
  data <- data.frame(id=game_ids,pred=game_pred)
  adjust <-1.05
  data$pred <- pmax(0.00005,pmin(.99995,
                      ifelse( data$pred> .5, 
                              adjust* data$pred,
                              1-(adjust*(1- data$pred)))))
  data$pred <- pmax(0.000005, pmin(.999995, data$pred))
   
  return(data[data$id != "remove",])
}

evaluate.tournament <- function(rank, slots,seeds, year)
{
  
  all.data <- evaluate.slot(1, rank, slots, seeds, year)
  
  for(i in 2:nrow(slots))
  {
    all.data <- rbind(all.data, evaluate.slot(i, rank, slots, seeds, year))
  }
  
  return(all.data)
}

slots <- read.csv(file = 'data\\competition\\tourney_slots.csv', sep=',', header=T)
seeds <- read.csv(file = 'data\\competition\\tourney_seeds.csv', sep=',', header=T)
teams <- read.csv(file = 'data/competition/team_spellings.csv', sep=',', header=T)

ranks <- tourny.offset()

#rank2011 <- data.frame(Team = ranks$Team, Score= ranks$Score2011, adjustment = ranks$adjustment)
slots2011 <- slots[slots$season == 2011,]
seeds2011 <- seeds[seeds$season == 2011,]
#rank2012 <- data.frame(Team = ranks$Team, Score= ranks$Score2012, adjustment = ranks$adjustment)
slots2012 <- slots[slots$season == 2012,]
seeds2012 <- seeds[seeds$season == 2012,]
#rank2013 <- data.frame(Team = ranks$Team, Score= ranks$Score2013, adjustment = ranks$adjustment)
slots2013 <- slots[slots$season == 2013,]
seeds2013 <- seeds[seeds$season == 2013,]

#rank2014 <- data.frame(Team = ranks$Team, Score= ranks$Score2014, adjustment = ranks$adjustment)
slots2014 <- slots[slots$season == 2014,]
seeds2014 <- seeds[seeds$season == 2014,]


slots2015 <- slots[slots$season == 2015,]
seeds2015 <- seeds[seeds$season == 2015,]

results <- evaluate.tournament(ranks, slots2015, seeds2015, 2015)

fname <- file.choose()
write.table(results, fname, row.names=FALSE, sep=",") 
