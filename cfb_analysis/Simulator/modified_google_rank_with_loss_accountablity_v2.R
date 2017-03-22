#This is the optimal version
py.wins <- function(a, b)
{
  return((a^2.37)/((a^2.37)+(b^2.37)))
}
#This is the optimal version


page.rank <- function(H, b, alpha )
{
  #conferences <-  read.csv(file = 'data\\divisionBreakdown.csv', sep=',', header=T)
  
  n <- dim(H)[1]
  S <- H
  rs <- H %*% rep(1,n)
  for (i in 1:n) {
    if (rs[i] == 0)
    {
      S[i,] <- b
      S[i,i] <- 3*(sum(b)-b[1])
      S[i,] <- S[i,]/sum(S[i,])
    }
    else 
    {
      S[i,] <- S[i,] / rs[i]    
    }
  }
  tm <- rep(1, n) %*% t(b)
  G <-  (alpha * S) + (1-alpha)*tm
  eig <- eigen(t(G))
  pi <- Re(eig$vectors[,1])
  return(pi)
}

schedule <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

schedule$Visitor.Team <- trim(schedule$Visitor.Team)
schedule$Home.Team <- trim(schedule$Home.Team)
Team <- unique(c(schedule$Visitor.Team,schedule$Home.Team))
#divisions <- read.csv(file = 'data\\divisionBreakdown.csv', sep=',', header=T)
divisions <- data.frame(Team = Team, id=1:length(Team))

schedule$Home.Id <- 0
schedule$Visitor.Id <- 0
for(i in 1:length(divisions$id))
{
  if(nrow(schedule[schedule$Home.Team == as.character(divisions[i,]$Team),]) > 0)
  {
    schedule[schedule$Home.Team == as.character(divisions[i,]$Team),]$Home.Id <- divisions[i,]$id
  }
  if(nrow(schedule[schedule$Visitor.Team == as.character(divisions[i,]$Team),]) > 0)
  {
    schedule[schedule$Visitor.Team == as.character(divisions[i,]$Team),]$Visitor.Id <-  divisions[i,]$id
  }
}

schedule <- schedule[schedule$Home.Id %in% divisions$id,]
schedule <- schedule[schedule$Visitor.Id %in% divisions$id,]

victories.matrix <- matrix(0, length(divisions$id), length(divisions$id))
mov.matrix <-  matrix(0, length(divisions$id), length(divisions$id))

for(j in 1:length(schedule$Home.Id))
{
  game <- schedule[j,]
  if(game$Home.Score > game$Visitor.Score){
    victories.matrix[game$Visitor.Id, game$Home.Id] <-  + victories.matrix[game$Visitor.Id, game$Home.Id]
    mov.matrix[game$Visitor.Id, game$Home.Id] <- 1 + mov.matrix[game$Visitor.Id, game$Home.Id] 
    
  }
  if(game$Home.Score < game$Visitor.Score){
    victories.matrix[game$Home.Id, game$Visitor.Id] <- 1+  victories.matrix[game$Home.Id, game$Visitor.Id] 
    mov.matrix[game$Home.Id, game$Visitor.Id] <-  1  + mov.matrix[game$Home.Id, game$Visitor.Id]
  }
}


losses.matrix <- t(victories.matrix)
mol.matrix <-  t(mov.matrix)

for(run in 1:100)
{
  for(k in 1:dim(mov.matrix)[1])
  {
    losses <- sum(victories.matrix[k,])
    lossMoV <- sum(mov.matrix[k,])
  }
  
  book <- rep(min(1, mean(mov.matrix))/(dim(mov.matrix)[1]),
              dim(mov.matrix)[1])
  
  t<- page.rank(mov.matrix, book, .85)
  
  divisions$score <- abs(1000*t)
  
  divisions$rank <- 1 + length(divisions$score) - rank(divisions$score)
  
  for(k in 1:length(divisions$id))
  {
    lossScores <- divisions[victories.matrix[k,] >= 0,]$score * mov.matrix[victories.matrix[k,] >= 0, k]
    mov.matrix[victories.matrix[k,] >= 0, k] <- lossScores
  }
}

resultsWins <- divisions 

for(run in 1:100)
{
  for(k in 1:dim(mol.matrix)[1])
  {
    wins <- sum(losses.matrix[k,])
    winMov <- sum(mol.matrix[k,])
  }
  
  book <- rep(min(1, mean(mol.matrix))/(dim(mol.matrix)[1]),
              dim(mol.matrix)[1])
  
  t<- page.rank(mol.matrix, book, .85)
  
  divisions$score <- abs(1000*t)
  
  divisions$rank <- 1 + length(divisions$score) - rank(divisions$score)
  
  for(k in 1:length(divisions$id))
  {
    winScores <- divisions[losses.matrix[k,] >= 0,]$score * mol.matrix[losses.matrix[k,] >= 0, k]
    mol.matrix[losses.matrix[k,] >= 0, k] <- winScores
  }
}

resultsLosses <- divisions

Conferences <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

resultsWins <- resultsWins[resultsWins$Team %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]
resultsLosses <- resultsLosses[resultsLosses$Team %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]

results <- data.frame(Team=resultsWins$Team, Score=(resultsWins$score-(3*resultsLosses$score)))
results$Rank <- 1 + length(results$Score) - rank(results$Score)

fname <- file.choose()
print("Running rankings.  Please wait.")
write.table(results, fname, row.names=FALSE, sep=",") 
