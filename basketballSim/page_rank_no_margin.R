page.rank <- function(H, b, alpha )
{
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

schedule <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

schedule$Visitor.Team <- trim(schedule$Visitor.Team)
schedule$Home.Team <- trim(schedule$Home.Team)
leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)

schedule <- schedule[schedule$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]

schedule <- schedule[schedule$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]


divisions <- data.frame(Team = unique(schedule$Home.Team), id = 1:length(unique(schedule$Home.Team)),
                        row.names = NULL)

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

for(j in 1:length(schedule$Home.Id))
{
  game <- schedule[j,]
  if(game$Home.Score > game$Visitor.Score){
    victories.matrix[game$Visitor.Id, game$Home.Id] <- 1 + victories.matrix[game$Visitor.Id, game$Home.Id]
    
  }
  if(game$Home.Score < game$Visitor.Score){
    victories.matrix[game$Home.Id, game$Visitor.Id] <- 1+  victories.matrix[game$Home.Id, game$Visitor.Id] 
    
  }
}

  for(k in 1:dim(victories.matrix)[1])
  {
    losses <- sum(victories.matrix[k,])
    victories.matrix[k,k] <- ifelse(losses==0, 0, 1)
  }
  
  book <- rep(1 / (dim(victories.matrix)[1]),
              dim(victories.matrix)[1])
  
  t<- page.rank(victories.matrix, book, .85)
  
  divisions$score <- abs(1000*t)
  
  divisions$rank <- 1 + length(divisions$score) - rank(divisions$score)
  
  for(k in 1:dim(victories.matrix)[1])
  {  
    victories.matrix[k,k] <- 0
  }
  
results <- divisions
