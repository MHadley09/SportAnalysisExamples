game.value <- function(scoreA, scoreB)
{
  x <- pmin(pmax(-21, scoreA - scoreB),21)
  return (.5 - (.0000270209*x^3) + (0.0352489*x))
}

elo.ranking <-function()
{
  box.scores <- read.csv(file = 'data\\2015\\gamesPlayed.csv', sep=',', header=T)
  
  ##FBS.Records <- read.csv(file = 'data\\2015\\records.csv', sep=',', header=T)
  
  Conferences <- read.csv(file = 'data\\2015\\conferenceWithFCS.csv', sep=',', header=T)
  
  names(Conferences)[names(Conferences) == "TEAM"] <- "Team"
  
  Starting.Elo <- read.csv(file= 'data\\2015\\preseason\\preseason.csv', sep=',', header=T)
  
  rankings <- merge(Conferences, Starting.Elo, by="Team", all=T)
  
  rankings[is.na(rankings)] <- 1200
  
  rankings$Rank <- 0
  
  rankings$Games <- 0
  
  box.scores <- box.scores[box.scores$Home.Team %in% as.character(rankings$Team),]
  box.scores <- box.scores[box.scores$Visitor.Team %in% as.character(rankings$Team),]
  
  for(i in 1:(length(box.scores$Home.Team)))
  {
    box <- box.scores[i,]
    
    if(!(nrow(box) == 0))
    {
      home <- rankings[rankings$Team == as.character(box$Home.Team),]
      if(nrow(home) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      away <- rankings[rankings$Team == as.character(box$Visitor.Team),]
      if(nrow(away) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      Rh <- 10^(home$Score/400)
      Ra <- 10^(away$Score/400)
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
      home$Score <- home$Score + ((400/(5+home$Games))*(Sh-Eh)) 
      away$Score <- away$Score + ((400/(5+away$Games))*(Sa-Ea)) 
      home$Games <- home$Games + 1
      away$Games <- away$Games + 1
      rankings[rankings$Team == as.character(box$Home.Team),] <- home
      rankings[rankings$Team == as.character(box$Visitor.Team),] <- away
    }
  }
  
  second.pass.rankings <- rankings
  second.pass.rankings$Games <- 0
  for(i in 1:(length(box.scores$Home.Team)))
  {
    box <- box.scores[i,]
    
    if(!(nrow(box) == 0))
    {
      home <- second.pass.rankings[second.pass.rankings$Team == as.character(box$Home.Team),]
      if(nrow(home) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      away <- second.pass.rankings[second.pass.rankings$Team == as.character(box$Visitor.Team),]
      if(nrow(away) != 1)
      {
        print(box$Home.Team)
        print(box$Visitor.Team)
      }
      Rh <- 10^(home$Score/400)
      Ra <- 10^(away$Score/400)
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
      home$Score <- home$Score + ((400/(5+home$Games))*(Sh-Eh)) 
      away$Score <- away$Score + ((400/(5+away$Games))*(Sa-Ea)) 
      home$Games <- home$Games + 1
      away$Games <- away$Games + 1
      second.pass.rankings[second.pass.rankings$Team == as.character(box$Home.Team),] <- home
      second.pass.rankings[second.pass.rankings$Team == as.character(box$Visitor.Team),] <- away
    }
  }
  rankings <- rankings[rankings$Team %in% Conferences[Conferences$Conference != "FCS",]$Team,]
  second.pass.rankings <- second.pass.rankings[second.pass.rankings$Team %in% Conferences[Conferences$Conference != "FCS",]$Team,]
  
  rankings$Rank <- 1 + length(rankings$Score) - rank(rankings$Score)
  second.pass.rankings$Rank <- 1 + length(second.pass.rankings$Score) - rank(second.pass.rankings$Score)
  
  return(second.pass.rankings)
}

elos2 <- elo.ranking()
elo.predict<-function(teamA, teamB)
{
  eloA <- elos[elos$TEAM == teamA,]$ELO
  eloB <- elos[elos$TEAM == teamB,]$ELO
  return (1/(1+10^((eloB-eloA)/400))) 
}