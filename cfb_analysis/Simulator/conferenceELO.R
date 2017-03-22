elo.ranking <-function()
{
  box.scores <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)
  
  FBS.Records <- read.csv(file = 'data\\2015\\records.csv', sep=',', header=T)
  
  divisions <- read.csv(file = 'data\\2015\\ConferenceWithFCS.csv', sep=',', header=T)
  
  homeDivision <- divisions
  awayDivision <- divisions
  
  names(homeDivision)[ names(homeDivision) == "TEAM"] <- "Home.Team"
  names(homeDivision)[ names(homeDivision) == "Conference"] <- "Home.Conference"
  names(awayDivision)[ names(awayDivision) == "TEAM"] <- "Visitor.Team"
  names(awayDivision)[ names(awayDivision) == "Conference"] <- "Visitor.Conference"
  
  box.scores <- merge(box.scores, homeDivision)
  box.scores <- merge(box.scores, awayDivision)
  
  box.scores$Home.Team <- box.scores$Home.Conference
  box.scores$Visitor.Team <- box.scores$Visitor.Conference
  
  TEAM <- unique(box.scores$Home.Team)
  
  startElo <- 1600
  ELO <- rep(startElo,  length(TEAMS))
  GAMES <- rep(0,  length(TEAMS))
  RANK <-  rep(0, length(TEAMS))
  
  rankings <- data.frame(TEAM, ELO, GAMES, RANK)

  box.scores <- box.scores[box.scores$Home.Team %in% as.character(TEAMS),]
  box.scores <- box.scores[box.scores$Visitor.Team %in% as.character(TEAMS),]
  
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
      home$ELO <- home$ELO + ((800/(20+home$GAMES))*(Sh-Eh)) 
      away$ELO <- away$ELO + ((800/(20+away$GAMES))*(Sa-Ea)) 
      home$GAMES <- home$GAMES + 1
      away$GAMES <- away$GAMES + 1
      rankings[rankings$TEAM == as.character(box$Home.Team),] <- home
      rankings[rankings$TEAM == as.character(box$OPP),] <- away
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
       home$ELO <- home$ELO + ((800/(20+home$GAMES))*(Sh-Eh)) 
       away$ELO <- away$ELO + ((800/(20+away$GAMES))*(Sa-Ea)) 
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
elo.predict<-function(teamA, teamB)
{
  eloA <- elos[elos$TEAM == teamA,]$ELO
  eloB <- elos[elos$TEAM == teamB,]$ELO
  return (1/(1+10^((eloB-eloA)/400))) 
}