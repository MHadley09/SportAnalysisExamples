game.value <- function(scoreA, scoreB)
{
  if(scoreA>scoreB) 
    return(1) 
  else
    return(0)
}

elo.ranking <-function()
{
  box.scores <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)
  
  Teams <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
  
  ##Conferences <- read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)
  
  
  names(Teams)[names(Teams) == "TEAM"] <- "Team"
  
  
  
  
  Starting.Elo <- read.csv(file= 'data\\2016\\preseason\\preseason.csv', sep=',', header=T)
  
  rankings <- merge(Teams, Starting.Elo, by="Team", all=T)
  
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
      Sh <- game.value(box$Home.Score, box$Visitor.Score)
      Sa <- game.value(box$Visitor.Score, box$Home.Score)
      if(box$Home.Score == box$Visitor.Score)
      {      
        Sh <- 0.5
        Sa <- 0.5
      }
      home$Score <- home$Score + 2*((1600/(5+home$Games))*(Sh-Eh)) 
      away$Score <- away$Score + 2*((1600/(5+away$Games))*(Sa-Ea)) 
      home$Games <- home$Games + 1
      away$Games <- away$Games + 1
      rankings[rankings$Team == as.character(box$Home.Team),] <- home
      rankings[rankings$Team == as.character(box$Visitor.Team),] <- away
    }
  }
  
  second.pass.rankings <- rankings
  second.pass.rankings$Games <- 0
  for(i in (length(box.scores$Home.Team)):1)
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
      Sh <- game.value(box$Home.Score, box$Visitor.Score)
      Sa <- game.value(box$Visitor.Score, box$Home.Score)
      if(box$Home.Score == box$Visitor.Score)
      {      
        Sh <- 0.5
        Sa <- 0.5
      }
      home$Score <- home$Score + 2*((1600/(5+home$Games))*(Sh-Eh)) 
      away$Score <- away$Score + 2*((1600/(5+away$Games))*(Sa-Ea)) 
      home$Games <- home$Games + 1
      away$Games <- away$Games + 1
      second.pass.rankings[second.pass.rankings$Team == as.character(box$Home.Team),] <- home
      second.pass.rankings[second.pass.rankings$Team == as.character(box$Visitor.Team),] <- away
    }
  }
#  rankings <- rankings[rankings$Team %in% Conferences[Conferences$Conference != "FCS",]$Team,]
 # second.pass.rankings <- second.pass.rankings[second.pass.rankings$Team %in% Conferences[Conferences$Conference != "FCS",]$Team,]
  
  rankings$Rank <- 1 + length(rankings$Score) - rank(rankings$Score)
  second.pass.rankings$Rank <- 1 + length(second.pass.rankings$Score) - rank(second.pass.rankings$Score)
  
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