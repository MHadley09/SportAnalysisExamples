source('betting_predictor_v2.R')

lines <- read.csv(file = 'data\\2016\\betting.csv', sep=',', header=T)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
lines$TeamA <- trim(lines$TeamA)
lines$TeamB<- trim(lines$TeamB)
runCount <- 100000


lines$A.Win.Percent <- 0
lines$B.Win.Percent <- 0
lines$A.Covers.Percent <- 0
lines$B.Covers.Percent <- 0
lines$Over.Percent <- 0
lines$Under.Percent <- 0
lines$A.Score <- 0
lines$B.Score <- 0

results <- data.frame(Winner = c(), Winner.Score = c(),Loser = c(), Loser.Score = c(), Winner.Percent = c(), Spread.Winner = c(), 
                      Cover.Percent = c(), OverUnder = c(), OverUnder.Percent = c())

for(i in 1:nrow(lines))
{
  curGame <- lines[i,]
  box <- predict(curGame$TeamA, curGame$TeamB, curGame$TeamA.Spread, curGame$OverUnder, runCount)
  curGame$A.Win.Percent <- 100*box[1,]$WINS / runCount
  curGame$B.Win.Percent <- 100*box[2,]$WINS / runCount
  curGame$A.Covers.Percent <- 100*box[1,"Spread Wins"] / runCount
  curGame$B.Covers.Percent <- 100* box[2,"Spread Wins"] / runCount
  curGame$Over.Percent <- 100* box[1,"Over/Under"] / runCount
  curGame$Under.Percent <- 100* box[2,"Over/Under"] / runCount
  curGame$A.Score <- box[1,"Score"] 
  curGame$B.Score <- box[2,"Score"] 
  lines[i,] <- curGame
}
results <- lines

results$Winner <- ifelse(results$A.Win.Percent >  results$B.Win.Percent, 
                         results$TeamA, results$TeamB)
results$Loser <- ifelse(results$A.Win.Percent <  results$B.Win.Percent, 
                        results$TeamA, results$TeamB)
results$Winner.Score <- ifelse(results$A.Win.Percent >  results$B.Win.Percent, 
                         results$A.Score, results$B.Score)
results$Loser.Score <- ifelse(results$A.Win.Percent <  results$B.Win.Percent, 
                        results$A.Score, results$B.Score)
results$Winner.Percent <- ifelse(results$A.Win.Percent >  results$B.Win.Percent, 
                                 results$A.Win.Percent, results$B.Win.Percent)
results$Spread.Winner <- ifelse(results$A.Covers.Percent >  results$B.Covers.Percent, 
                                results$TeamA, results$TeamB)
results$Cover.Percent <- ifelse(results$A.Covers.Percent >  results$B.Covers.Percent, 
                                results$A.Covers.Percent , results$B.Covers.Percent )
results$OverUnder <- ifelse(results$Over.Percent >  results$Under.Percent, 
                                "Over", "Under" )
results$OverUnder.Percent <- ifelse(results$Over.Percent >  results$Under.Percent, 
                                    results$Over.Percent ,  results$Under.Percent )

results <- data.frame(Winner=results$Winner, Loser=results$Loser, Winner.Percent=results$Winner.Percent,
                      Spread.Winner = results$Spread.Winner, Cover.Percent=results$Cover.Percent,
                      OverUnder = results$OverUnder, OverUnder.Percent=results$OverUnder.Percent)


write.table(results,'data\\2016\\bettingresults_v2.csv', row.names=FALSE, sep=",") 
write.table(lines,'data\\2016\\lines.csv', row.names=FALSE, sep=",") 