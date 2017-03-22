page.rank <- function(H, b, alpha )
{
  conferences <-  read.csv(file = 'data\\divisionBreakdown.csv', sep=',', header=T)
  
  n <- dim(H)[1]
  S <- H
  rs <- H %*% rep(1,n)
  for (i in 1:n) {
    if (rs[i] == 0)
    {
      S[i,] <- b
    }
    else 
    {
      S[i,] <- S[i,] / rs[i]    
      if(conferences[i,]$Division == "FBS P5")
      {
        S[i,] <- S[i,]*6
      }
      else if(conferences[i,]$Division == "FBS G5")
      {
        S[i,] <- S[i,]*5
      }
      else if(conferences[i,]$Division == "FCS")
      {
        S[i,] <- S[i,]*4
      }
      else if(conferences[i,]$Division == "DivII")
      {
        S[i,] <- S[i,]*3
      }
      else if(conferences[i,]$Division == "DivIII")
      {
        S[i,] <- S[i,]*2
      }
      
      else if(conferences[i,]$Division == "NAIA")
      {
        S[i,] <- S[i,]*1.5
      }
    }
    
  }
  tm <- rep(1, n) %*% t(b)
  G <-  (alpha * S) + (1-alpha)*tm
  eig <- eigen(t(G))
  pi <- Re(eig$vectors[,1])
  return(pi)
}

schedule <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)

divisions <- read.csv(file = 'data\\divisionBreakdown.csv', sep=',', header=T)

divisions$id <- 1:length(divisions$Team)

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
    victories.matrix[game$Visitor.Id, game$Home.Id] <- 1 + victories.matrix[game$Visitor.Id, game$Home.Id]
    mov.matrix[game$Visitor.Id, game$Home.Id] <- (game$Home.Score - game$Visitor.Score) + mov.matrix[game$Visitor.Id, game$Home.Id] 
    
  }
  if(game$Home.Score < game$Visitor.Score){
    victories.matrix[game$Home.Id, game$Visitor.Id] <- 1+  victories.matrix[game$Home.Id, game$Visitor.Id] 
    mov.matrix[game$Home.Id, game$Visitor.Id] <-  (game$Visitor.Score - game$Home.Score)  + mov.matrix[game$Home.Id, game$Visitor.Id]
    
  }
}

for(run in 1:25)
{
  for(k in 1:dim(mov.matrix)[1])
  {
    losses <- sum(victories.matrix[k,])
    lossMoV <- sum(mov.matrix[k,])
    mov.matrix[k,k] <- ifelse(losses==0, 0, lossMoV/(losses))
  }
  
  book <- rep(1/(dim(mov.matrix)[1]),
              dim(mov.matrix)[1])
  
  t<- page.rank(mov.matrix, book, .95)
  
  divisions$score <- -10000*t
  
  divisions$rank <- 1 + length(divisions$score) - rank(divisions$score)
  
  for(k in 1:length(divisions$id))
  {
    lossScores <- divisions[victories.matrix[k,] >= 1,]$score * mov.matrix[victories.matrix[k,] >= 1, k]
    mov.matrix[victories.matrix[k,] >= 1, k] <- lossScores
  }
}
results <- divisions[(divisions$Division == "FBS P5"),]
results <- rbind(results, divisions[(divisions$Division == "FBS G5"),])
results$rank <- 1 + length(results$score) - rank(results$score)
