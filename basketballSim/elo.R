elo.ranking <-function()
{
  box.scores <- read.csv(file = 'data\\allscores.csv', sep=',', header=T)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  box.scores$Visitor.Team <- trim(box.scores$Visitor.Team)
  box.scores$Home.Team <- trim(box.scores$Home.Team)
  leagues <- read.csv(file= 'data\\leagues.csv', sep=',', header=T)
  
  box.scores <- box.scores[box.scores$Home.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
  
  box.scores <- box.scores[box.scores$Visitor.Team %in% as.character(leagues[leagues$League == "NCAA" & leagues$Division == "I",]$Team),]
  
  
  startElo <- 1600
  TEAM <- unique(box.scores$Visitor.Team)
  ELO <- rep(startElo,  length(TEAM))
  GAMES <- rep(0,  length(TEAM))
  RANK <-  rep(0, length(TEAM))
  
  rankings <- data.frame(TEAM, ELO, GAMES, RANK)
  
  
  for(i in 1:(length(box.scores$Home.Team)))
  {
    box <- box.scores[i,]
    
    if(!(nrow(box) == 0))
    {
      home <- rankings[rankings$TEAM == as.character(box$Home.Team),]
      away <- rankings[rankings$TEAM == as.character(box$Visitor.Team),]
      Rh <- 10^(home$ELO/400)
      Ra <- 10^(away$ELO/400)
      Eh <- Rh/(Rh+Ra)
      Ea <- Ra/(Rh+Ra)
      Sh <- 1
      Sa <- 0
      if(box$Home.Score < box$Visitor.Score)
      {
        Sh <- 0
        Sa <- 1
      }
      else if(box$Home.Score == box$Visitor.Score)
      {      
        Sh <- 0.5
        Sa <- 0.5
      }
      home$ELO <- home$ELO + ((800/(15+home$GAMES))*(Sh-Eh)) 
      away$ELO <- away$ELO + ((800/(15+away$GAMES))*(Sa-Ea)) 
      home$GAMES <- home$GAMES + 1
      away$GAMES <- away$GAMES + 1
      rankings[rankings$TEAM == as.character(box$Home.Team),] <- home
      rankings[rankings$TEAM == as.character(box$OPP),] <- away
    }
  }
  
  second.pass.rankings <- rankings
#  second.pass.rankings$GAMES <- 0
#   for(i in (length(box.scores$Home.Team)):1)
#   {
#     box <- box.scores[i,]
#     
#     if(!(nrow(box) == 0))
#     {
#       home <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Home.Team),]
#       away <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Visitor.Team),]
#       Rh <- 10^(home$ELO/400)
#       Ra <- 10^(away$ELO/400)
#       Eh <- Rh/(Rh+Ra)
#       Ea <- Ra/(Rh+Ra)
#       Sh <- 1
#       Sa <- 0
#       if(box$Home.Score < box$Visitor.Score)
#       {
#         Sh <- 0
#         Sa <- 1
#       }
#       else if(box$Home.Score == box$Visitor.Score)
#       {      
#         Sh <- 0.5
#         Sa <- 0.5
#       }
#       home$ELO <- home$ELO + ((800/(20+home$GAMES))*(Sh-Eh)) 
#       away$ELO <- away$ELO + ((800/(20+away$GAMES))*(Sa-Ea)) 
#       home$GAMES <- home$GAMES + 1
#       away$GAMES <- away$GAMES + 1
#       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Home.Team),] <- home
#       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Visitor.Team),] <- away
#     }
#   }
    
  rankings$RANK <- 1 + length(rankings$ELO) - rank(rankings$ELO)
  second.pass.rankings$RANK <- 1 + length(second.pass.rankings$ELO) - rank(second.pass.rankings$ELO)
  
  return(second.pass.rankings)
}

predict<-function(teamA, teamB)
{
  elos <- elo.ranking()
  eloA <- elos[elos$TEAM == teamA,]$ELO
  eloB <- elos[elos$TEAM == teamB,]$ELO
  return (1/(1+10^((eloB-eloA)/400))) 
}