game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return (.5 - (.0000270209*x^3) + (0.0352489*x))
}


elo.ranking <-function()
{
  box.scores <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  
  FBS.Records <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  
  Conferences <- read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)
     
   FBS.Record.List <- FBS.Records[FBS.Records$TEAM %in% 
                                    as.character(Conferences[,]$TEAM),]
  
  TEAM <- FBS.Record.List$TEAM
  
  startElo <- 1400
  ELO <- rep(startElo,  length(FBS.Record.List$TEAM))
  GAMES <- rep(0,  length(FBS.Record.List$TEAM))
  RANK <-  rep(0, length(FBS.Record.List$TEAM))
  
  rankings <- data.frame(TEAM, ELO, GAMES, RANK)
  
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "FCS",]$TEAM,]$ELO <- startElo/2
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "ACC",]$TEAM,]$ELO <- startElo+100
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "SEC",]$TEAM,]$ELO <- startElo+100
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "Big Ten",]$TEAM,]$ELO <- startElo+100
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "Big 12",]$TEAM,]$ELO <- startElo+100
  rankings[rankings$TEAM %in% Conferences[Conferences$Conference == "Pac-12",]$TEAM,]$ELO <- startElo+100
  box.scores <- box.scores[box.scores$Home.Team %in% as.character(FBS.Record.List$TEAM),]
  box.scores <- box.scores[box.scores$Visitor.Team %in% as.character(FBS.Record.List$TEAM),]
  
  for(i in 1:(length(box.scores$Home.Team)))
  {
    box <- box.scores[i,]
        
    if(!(nrow(box) == 0))
    {
      home <- rankings[rankings$TEAM == as.character(box$Home.Team),]
      if(nrow(home) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      away <- rankings[rankings$TEAM == as.character(box$Visitor.Team),]
      if(nrow(away) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      Rh <- 10^(home$ELO/400)
      Ra <- 10^(away$ELO/400)
      Eh <- Rh/(Rh+Ra)
      Ea <- Ra/(Rh+Ra)
      Sh <- (1+game.value(box$Home.Score, box$Visitor.Score))/2
      Sa <-  game.value(box$Visitor.Score, box$Home.Score)/2
      
      if(box$Home.Score < box$Visitor.Score)
      {
        Sh <- (game.value(box$Home.Score, box$Visitor.Score))/2
        Sa <-  (1+game.value(box$Visitor.Score, box$Home.Score))/2
      }
      else if(box$Home.Score == box$Visitor.Score)
      {      
        Sh <- 0.5
        Sa <- 0.5
      }
      home$ELO <- home$ELO + 2*((1600/(5+home$GAMES))*(Sh-Eh)) 
      away$ELO <- away$ELO + 2*((1600/(5+away$GAMES))*(Sa-Ea)) 
      home$GAMES <- home$GAMES + 1
      away$GAMES <- away$GAMES + 1
      rankings[rankings$TEAM == as.character(box$Home.Team),] <- home
      rankings[rankings$TEAM == as.character(box$Visitor.Team),] <- away
    }
  }
  
   second.pass.rankings <- rankings
  second.pass.rankings$GAMES <- 0
   for(i in (length(box.scores$Home.Team)):1)
   {
     box <- box.scores[i,]
     
     if(!(nrow(box) == 0))
     {
       home <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Home.Team),]
       away <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Visitor.Team),]
       Rh <- 10^(home$ELO/400)
       Ra <- 10^(away$ELO/400)
       Eh <- Rh/(Rh+Ra)
       Ea <- Ra/(Rh+Ra)     
       Sh <- (1+game.value(box$Home.Score, box$Visitor.Score))/2
       Sa <-  game.value(box$Visitor.Score, box$Home.Score)/2
       if(box$Home.Score < box$Visitor.Score)
       {
         Sh <- (game.value(box$Home.Score, box$Visitor.Score))/2
         Sa <-  (1+game.value(box$Visitor.Score, box$Home.Score))/2
       }
       else if(box$Home.Score == box$Visitor.Score)
       {      
         Sh <- 0.5
         Sa <- 0.5
       }
       home$ELO <- home$ELO + 2*((1600/(5+home$GAMES))*(Sh-Eh)) 
       away$ELO <- away$ELO + 2*((1600/(5+away$GAMES))*(Sa-Ea)) 
       home$GAMES <- home$GAMES + 1
       away$GAMES <- away$GAMES + 1
       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Home.Team),] <- home
       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$Visitor.Team),] <- away
     }
   }
   
  rankings <- rankings[rankings$TEAM %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]
  second.pass.rankings <- second.pass.rankings[second.pass.rankings$TEAM %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]

  rankings$RANK <- 1 + length(rankings$ELO) - rank(rankings$ELO)
  second.pass.rankings$RANK <- 1 + length(second.pass.rankings$ELO) - rank(second.pass.rankings$ELO)
  
  return(second.pass.rankings)
}

elos <- elo.ranking()
fname <- file.choose()
print("Running rankings.  Please wait.")
write.table(elos, fname, row.names=FALSE, sep=",") 

elo.predict<-function(teamA, teamB)
{
  eloA <- elos[elos$TEAM == teamA,]$ELO
  eloB <- elos[elos$TEAM == teamB,]$ELO
  return (1/(1+10^((eloB-eloA)/400))) 
}