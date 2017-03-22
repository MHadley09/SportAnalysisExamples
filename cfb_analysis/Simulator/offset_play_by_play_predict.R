read.double <- function(text)
{ 
  n <- readline(prompt=text)
  if(!grepl("[-+]?[0-9]*\\.?[0-9]*",n))
  {
    t <- text
    return(read.double(t))
  }
  
  return(as.numeric(n))
}


source('SoS_played.r')

SoS <- FBS.SoS()
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

data <- read.csv(file = 'data\\2015\\perplaydata.csv', sep=',', header=T)

data$Visitor.Team <- trim(data$Visitor.Team)
data$Home.Team <- trim(data$Home.Team)

FBS.Team <- read.csv(file = 'data\\2015\\records.csv', sep=',', header=T)
Conferences <- read.csv(file = 'data\\2015\\conferenceWithFCS.csv', sep=',', header=T)

FBS.Team <- FBS.Team[FBS.Team$TEAM %in% Conferences[Conferences$Conference != "FCS",]$TEAM,]

data <- data[data$Visitor.Team %in% FBS.Team$TEAM ,]

data <- data[data$Home.Team %in% FBS.Team$TEAM ,]



TEAM <- c(data$Home.Team, data$Visitor.Team)
SCORE <- c(data$Home.Score, data$Visitor.Score)
TOP <- c(data$Home.Time.of.Possession, data$Visitor.Time.of.Possession)
RUSH.YARDS <- c(data$Home.Rushing.Yards, data$Visitor.Rushing.Yards)
RUSH.ATTEMPTS <- c(data$Home.Rushing.Attempts, data$Visitor.Rushing.Attempts)
PASS.YARDS <- c(data$Home.Passing.Yards, data$Visitor.Passing.Yards)
PASS.ATTEMPTS <- c(data$Home.Passing.Attempts, data$Visitor.Passing.Attempts)
FUMBLES <- c(data$Home.Fumbles.Lost, data$Visitor.Fumbles.Lost)
INT <- c(data$Home.Interceptions.Thrown, data$Visitor.Interceptions.Thrown)
PENALTIES <-  c(data$Home.Penalties, data$Visitor.Penalties)
PENALTY.YARDS <-  c(data$Home.Penalty.Yards, data$Visitor.Penalty.Yards)

OPP <-  c(data$Visitor.Team, data$Home.Team)
OPP.SCORE <- c(data$Visitor.Score, data$Home.Score )
OPP.TOP <- c(data$Visitor.Time.of.Possession, data$Home.Time.of.Possession)
OPP.RUSH.YARDS <- c(data$Visitor.Rushing.Yards, data$Home.Rushing.Yards)
OPP.RUSH.ATTEMPTS <- c(data$Visitor.Rushing.Attempts, data$Home.Rushing.Attempts )
OPP.PASS.YARDS <- c(data$Visitor.Passing.Yards , data$Home.Passing.Yards )
OPP.PASS.ATTEMPTS <- c(data$Visitor.Passing.Attempts, data$Home.Passing.Attempts )
OPP.FUMBLES <- c(data$Visitor.Fumbles.Lost, data$Home.Fumbles.Lost)
OPP.INT <- c(data$Visitor.Interceptions.Thrown, data$Home.Interceptions.Thrown )
OPP.PENALTIES <-  c(data$Visitor.Penalties, data$Home.Penalties )
OPP.PENALTY.YARDS <-  c(data$Visitor.Penalty.Yards, data$Home.Penalty.Yards)

working <- data.frame(TEAM, SCORE, TOP, RUSH.YARDS, RUSH.ATTEMPTS, PASS.YARDS, PASS.ATTEMPTS, FUMBLES,
                      INT, PENALTIES, PENALTY.YARDS, OPP, OPP.SCORE, OPP.TOP, OPP.RUSH.YARDS, OPP.RUSH.ATTEMPTS,
                      OPP.PASS.YARDS, OPP.PASS.ATTEMPTS, OPP.FUMBLES, OPP.INT, OPP.PENALTIES, OPP.PENALTY.YARDS)

rm(TEAM, SCORE, TOP, RUSH.YARDS, RUSH.ATTEMPTS, PASS.YARDS, PASS.ATTEMPTS, FUMBLES,
   INT, PENALTIES, PENALTY.YARDS, OPP, OPP.SCORE, OPP.TOP, OPP.RUSH.YARDS, OPP.RUSH.ATTEMPTS,
   OPP.PASS.YARDS, OPP.PASS.ATTEMPTS, OPP.FUMBLES, OPP.INT, OPP.PENALTIES, OPP.PENALTY.YARDS)

working$Points.Per.Play <- working$SCORE/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Yards.Per.Carry <- working$RUSH.YARDS/working$RUSH.ATTEMPTS
working$Yards.Per.Pass <- working$PASS.YARDS/working$PASS.ATTEMPTS
working$Rush.Percent <-working$RUSH.ATTEMPTS/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Pass.Percent <-working$PASS.ATTEMPTS/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Time.Per.Play <- working$TOP/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Penalties.Per.Play <- working$PENALTIES/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Yards.Per.Penalty <- working$PENALTY.YARDS/working$PENALTIES
working$Fumbles.Per.Play <- working$FUMBLES/(working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Int.Per.Pass <- (working$INT)/(working$PASS.ATTEMPTS)
working$Possession.Percent <- working$TOP / (working$TOP + working$OPP.TOP)

working <- merge(x= working, y=SoS, by.x = "TEAM", by.y = "TEAM")
colnames(working)[colnames(working) == "Adjusted.SoS"] <- "SOS" 

working <- working[,!(names(working) %in% c("Conference", "CONF.W","CONF.L", "OVERALL.W", "OVERALL.L",
                                            "ConferenceOffset", "First.Pass.SoS", "SoS.Score", 
                                            "SoS.Ranking"))]

working$Opp.Points.Per.Play <- working$OPP.SCORE/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Yards.Per.Carry <- working$OPP.RUSH.YARDS/working$OPP.RUSH.ATTEMPTS
working$Opp.Yards.Per.Pass  <- working$OPP.PASS.YARDS/working$OPP.PASS.ATTEMPTS 
working$Opp.Rush.Percent <-working$OPP.RUSH.ATTEMPTS/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Pass.Percent <-working$OPP.PASS.ATTEMPTS/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Time.Per.Play <- working$OPP.TOP/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Yards.Per.Penalty <- working$OPP.PENALTY.YARDS/working$OPP.PENALTIES
working$Opp.Penalties.Per.Play <- working$OPP.PENALTIES/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS + working$RUSH.ATTEMPTS + working$PASS.ATTEMPTS)
working$Opp.Fumbles.Per.Play <- working$OPP.FUMBLES/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Int.Per.Pass <- working$OPP.INT/(working$OPP.RUSH.ATTEMPTS + working$OPP.PASS.ATTEMPTS)
working$Opp.Possession.Percent <- working$OPP.TOP / (working$TOP + working$OPP.TOP)

working <- merge(x= working, y=SoS, by.x = "OPP", by.y = "TEAM")
colnames(working)[colnames(working) == "Adjusted.SoS"] <- "OPP.SOS" 

working <- working[,!(names(working) %in% c("Conference", "CONF.W","CONF.L", "OVERALL.W", "OVERALL.L",
                                            "ConferenceOffset", "First.Pass.SoS", "SoS.Score", 
                                            "SoS.Ranking"))]

stat.Averages <- aggregate( cbind(SCORE, TOP, RUSH.YARDS, RUSH.ATTEMPTS, PASS.YARDS, PASS.ATTEMPTS,
                                  FUMBLES, INT, PENALTIES, PENALTY.YARDS, OPP.SCORE, OPP.TOP,
                                  OPP.RUSH.YARDS, OPP.RUSH.ATTEMPTS, OPP.PASS.YARDS, OPP.PASS.ATTEMPTS,
                                  OPP.FUMBLES, OPP.INT, OPP.PENALTIES, OPP.PENALTY.YARDS, Points.Per.Play,
                                  Yards.Per.Carry, Yards.Per.Pass, Rush.Percent, Pass.Percent,
                                  Time.Per.Play, Penalties.Per.Play, Yards.Per.Penalty, Fumbles.Per.Play,
                                  Int.Per.Pass, Possession.Percent, SOS, Opp.Points.Per.Play, Opp.Yards.Per.Carry,
                                  Opp.Yards.Per.Pass, Opp.Rush.Percent, Opp.Pass.Percent, Opp.Time.Per.Play,
                                  Opp.Yards.Per.Penalty, Opp.Penalties.Per.Play, Opp.Int.Per.Pass,
                                  Opp.Fumbles.Per.Play, Opp.Possession.Percent, OPP.SOS) ~ TEAM, data= working, FUN=mean)

stat.Std.Dev <- aggregate( cbind(SCORE, TOP, RUSH.YARDS, RUSH.ATTEMPTS, PASS.YARDS, PASS.ATTEMPTS,
                                 FUMBLES, INT, PENALTIES, PENALTY.YARDS, OPP.SCORE, OPP.TOP,
                                 OPP.RUSH.YARDS, OPP.RUSH.ATTEMPTS, OPP.PASS.YARDS, OPP.PASS.ATTEMPTS,
                                 OPP.FUMBLES, OPP.INT, OPP.PENALTIES, OPP.PENALTY.YARDS, Points.Per.Play,
                                 Yards.Per.Carry, Yards.Per.Pass, Rush.Percent, Pass.Percent,
                                 Time.Per.Play, Penalties.Per.Play, Yards.Per.Penalty, Fumbles.Per.Play,
                                 Int.Per.Pass, Possession.Percent, SOS, Opp.Points.Per.Play, Opp.Yards.Per.Carry,
                                 Opp.Yards.Per.Pass, Opp.Rush.Percent, Opp.Pass.Percent, Opp.Time.Per.Play,
                                 Opp.Yards.Per.Penalty, Opp.Penalties.Per.Play, Opp.Int.Per.Pass,
                                 Opp.Fumbles.Per.Play, Opp.Possession.Percent, OPP.SOS) ~ TEAM, data= working, FUN=sd)


Offensive.SoS.Adjust <- function(awaySoS)
{
  meanSoS <- mean(working$SOS)
  return(sqrt(awaySoS / meanSoS ))
}
Defensive.SoS.Adjust <- function(awaySoS)
{
  meanSoS <- mean(working$SOS)
  return(sqrt(meanSoS / awaySoS ))
}

Offensive.SoS.Compare <- function(homeSoS, awaySoS)
{
  return(sqrt(homeSoS / awaySoS ))
}

Defensive.SoS.Compare <- function(homeSoS, awaySoS)
{
  return(sqrt(awaySoS / homeSoS ))        
}


Adjustments <- function(row)
{ 
  row[["Yards.Per.Carry"]] <- (Offensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                 (as.numeric(row[["Yards.Per.Carry"]])/as.numeric(
                                   stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Yards.Per.Carry)))
  row[["Yards.Per.Pass"]] <- (Offensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                (as.numeric(row[["Yards.Per.Pass"]])/as.numeric(
                                  stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Yards.Per.Pass)))
  row[["Points.Per.Play"]] <- (Offensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                 (as.numeric(row[["Points.Per.Play"]])/as.numeric(
                                   stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Points.Per.Play)))
  row[["Penalties.Per.Play"]] <- (as.numeric(row[["Penalties.Per.Play"]])/as.numeric(
    stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Penalties.Per.Play))
  row[["Int.Per.Pass"]] <- (Defensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                              (as.numeric(row[["Int.Per.Pass"]])/as.numeric(
                                stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Int.Per.Pass)))
  row[["Possession.Percent"]] <-  (as.numeric(row[["Possession.Percent"]])/as.numeric(
    stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Opp.Possession.Percent))
  row[["Opp.Yards.Per.Carry"]] <- (Defensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                     (as.numeric(row[["Opp.Yards.Per.Carry"]])/as.numeric(
                                       stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Yards.Per.Carry)))
  row[["Opp.Yards.Per.Pass"]] <- (Defensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                    (as.numeric(row[["Opp.Yards.Per.Pass"]])/as.numeric(
                                      stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Yards.Per.Pass)))
  row[["Opp.Points.Per.Play"]] <- (Defensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                     (as.numeric(row[["Opp.Points.Per.Play"]])/as.numeric(
                                       stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Points.Per.Play)))
  row[["Opp.Penalties.Per.Play"]] <- (as.numeric(row[["Opp.Penalties.Per.Play"]])/as.numeric(
    stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Penalties.Per.Play))
  row[["Opp.Int.Per.Pass"]] <- (Offensive.SoS.Adjust(as.numeric(row[["OPP.SOS"]]))*
                                  (as.numeric(row[["Opp.Int.Per.Pass"]])/as.numeric(
                                    stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Int.Per.Pass)))
  row[["Opp.Possession.Percent"]] <-  (as.numeric(row[["Opp.Possession.Percent"]])/as.numeric(
    stat.Averages[stat.Averages$TEAM == as.character(row[["OPP"]]),]$Possession.Percent))
  
  return (row)
}


setup.predict <- function(teamA, teamB, runCount)
{
  
  
  homeTeamGames <- working[working$TEAM == teamA,]
  awayTeamGames <- working[working$TEAM == teamB,]
  
  adjustments <- apply(FUN=Adjustments, MARGIN=1, homeTeamGames)
  
  adjustments <- cbind(adjustments, apply(FUN=Adjustments, MARGIN=1, awayTeamGames))
  
  adjustments <- data.frame( TEAM = adjustments["TEAM", ], 
                             Yards.Per.Carry = as.numeric(adjustments["Yards.Per.Carry", ]), 
                             Yards.Per.Pass = as.numeric(adjustments["Yards.Per.Pass", ]),
                             Points.Per.Play = as.numeric(adjustments["Points.Per.Play", ]), 
                             Penalties.Per.Play = as.numeric(adjustments[ "Penalties.Per.Play",]),
                             Int.Per.Pass = as.numeric(adjustments["Int.Per.Pass", ]), 
                             Possession.Percent = as.numeric(adjustments["Possession.Percent", ]),
                             Opp.Yards.Per.Carry = as.numeric(adjustments["Opp.Yards.Per.Carry", ]), 
                             Opp.Yards.Per.Pass = as.numeric(adjustments["Opp.Yards.Per.Pass", ]),
                             Opp.Points.Per.Play = as.numeric(adjustments["Opp.Points.Per.Play", ]), 
                             Opp.Penalties.Per.Play = as.numeric(adjustments["Opp.Penalties.Per.Play", ]),
                             Opp.Int.Per.Pass = as.numeric(adjustments["Opp.Int.Per.Pass", ]), 
                             Opp.Possession.Percent = as.numeric(adjustments["Opp.Possession.Percent", ]),
                             row.names = NULL)
  
  adjustment.Averages <- aggregate( cbind(Points.Per.Play, Yards.Per.Carry, Yards.Per.Pass, 
                                          Penalties.Per.Play, Int.Per.Pass, Possession.Percent,
                                          Opp.Points.Per.Play, Opp.Yards.Per.Carry,Opp.Yards.Per.Pass,
                                          Opp.Penalties.Per.Play, Opp.Int.Per.Pass, Opp.Possession.Percent) 
                                    ~ TEAM, data= adjustments, FUN=mean)  
  adjustment.Std.Dev <- aggregate( cbind(Points.Per.Play, Yards.Per.Carry, Yards.Per.Pass, 
                                         Penalties.Per.Play, Int.Per.Pass, Possession.Percent,
                                         Opp.Points.Per.Play, Opp.Yards.Per.Carry,Opp.Yards.Per.Pass,
                                         Opp.Penalties.Per.Play, Opp.Int.Per.Pass, Opp.Possession.Percent) 
                                   ~ TEAM, data= adjustments, FUN=sd) 
  
  home.sos <- stat.Averages[stat.Averages$TEAM == teamA,]$SOS
  away.sos <- stat.Averages[stat.Averages$TEAM == teamB,]$SOS
  
  home.passing.off <- read.double("Input manual offset for home team's passing yards:\t")
  home.rushing.off <-  read.double("Input manual offset for home team's rushing yards:\t")
  home.scoring.off <- read.double("Input manual offset for home team's scoring:\t")
  home.play.calling.off <- read.double("Input manual offset for home team's play calling
                                   (1.0 is normal above 1 increases passing below decrease): \t")
  away.passing.off <- read.double("Input manual offset for away team's passing yards:\t")
  away.rushing.off <-  read.double("Input manual offset for away team's rushing yards:\t")
  away.scoring.off <- read.double("Input manual offset for away team's scoring:\t")
  away.play.calling.off <- read.double("Input manual offset for away team's play calling
                                   (1.0 is normal above 1 increases passing below decrease): \t")

  home.yards.per.pass <-  home.passing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Yards.Per.Pass, 
                                sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Yards.Per.Pass)
  home.yards.per.carry <-  home.rushing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Yards.Per.Carry, 
                                 sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Yards.Per.Carry)
  home.points.per.play <-  home.scoring.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Points.Per.Play, 
                                 sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Points.Per.Play)
  home.penalties.per.play <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Penalties.Per.Play, 
                                    sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Penalties.Per.Play)
  home.turnovers.per.play  <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Int.Per.Pass, 
                                     sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Int.Per.Pass)
  home.possession.percent <- rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Possession.Percent, 
                                   sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Possession.Percent)
  home.yards.per.pass.allowed <-  away.passing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Opp.Yards.Per.Pass, 
                                        sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Opp.Yards.Per.Pass)
  home.yards.per.carry.allowed <-   away.rushing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Opp.Yards.Per.Carry, 
                                         sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Opp.Yards.Per.Carry)
  home.points.per.play.allowed <-  away.scoring.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Opp.Points.Per.Play, 
                                         sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Opp.Points.Per.Play)
  home.penalties.per.play.allowed <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Opp.Penalties.Per.Play, 
                                            sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Opp.Penalties.Per.Play)
  home.takeaways.per.play  <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Opp.Int.Per.Pass, 
                                     sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Opp.Int.Per.Pass)
  home.yards.per.penalty <- rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Yards.Per.Penalty, 
                                  sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Yards.Per.Penalty)
  
  home.percent.pass <-  home.play.calling.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Pass.Percent, 
                              sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Pass.Percent)
  home.percent.run <-  1-home.percent.pass
  
  
  home.time.per.play <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamA,]$Time.Per.Play, 
                               sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamA,]$Time.Per.Play)
  
  home.yards.per.pass.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Yards.Per.Pass, 
                                           sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Yards.Per.Pass)
  home.yards.per.carry.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Yards.Per.Carry, 
                                            sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Yards.Per.Carry)
  home.points.per.play.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Points.Per.Play, 
                                            sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Points.Per.Play)
  home.penalties.per.play.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Penalties.Per.Play, 
                                               sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Penalties.Per.Play)
  home.turnovers.per.play.adjustment  <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Int.Per.Pass, 
                                                sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Int.Per.Pass)
  home.possession.percent.adjustment <- rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Possession.Percent, 
                                              sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Possession.Percent)
  home.yards.per.pass.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Opp.Yards.Per.Pass, 
                                                   sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Opp.Yards.Per.Pass)
  home.yards.per.carry.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Opp.Yards.Per.Carry, 
                                                    sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Opp.Yards.Per.Carry)
  home.points.per.play.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Opp.Points.Per.Play, 
                                                    sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Opp.Points.Per.Play)
  home.penalties.per.play.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Opp.Penalties.Per.Play, 
                                                       sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Opp.Penalties.Per.Play)
  home.takeaways.per.play.adjustment  <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamA,]$Opp.Int.Per.Pass, 
                                                sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamA,]$Opp.Int.Per.Pass)
  
  away.yards.per.pass <-  away.passing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Yards.Per.Pass, 
                                sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Yards.Per.Pass)
  away.yards.per.carry <-  away.rushing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Yards.Per.Carry, 
                                 sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Yards.Per.Carry)
  away.points.per.play <-  away.scoring.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Points.Per.Play, 
                                 sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Points.Per.Play)
  away.penalties.per.play <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Penalties.Per.Play, 
                                    sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Penalties.Per.Play)
  away.turnovers.per.play  <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Int.Per.Pass, 
                                     sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Int.Per.Pass)
  away.possession.percent <- rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Possession.Percent, 
                                   sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Possession.Percent)
  away.yards.per.pass.allowed <-  home.passing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Opp.Yards.Per.Pass, 
                                        sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Opp.Yards.Per.Pass)
  away.yards.per.carry.allowed <-  home.rushing.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Opp.Yards.Per.Carry, 
                                         sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Opp.Yards.Per.Carry)
  away.points.per.play.allowed <-  home.scoring.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Opp.Points.Per.Play, 
                                         sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Opp.Points.Per.Play)
  away.penalties.per.play.allowed <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Opp.Penalties.Per.Play, 
                                            sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Opp.Penalties.Per.Play)
  away.takeaways.per.play  <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Opp.Int.Per.Pass, 
                                     sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Opp.Int.Per.Pass)
  away.yards.per.penalty <- rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Yards.Per.Penalty, 
                                  sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Yards.Per.Penalty)
  
  away.percent.pass <-   away.play.calling.off*rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Pass.Percent, 
                              sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Pass.Percent)
  away.percent.run <-  1-away.percent.pass
  
  
  away.time.per.play <-  rnorm(n=runCount, mean=stat.Averages[stat.Averages$TEAM == teamB,]$Time.Per.Play, 
                               sd=stat.Std.Dev[stat.Std.Dev$TEAM == teamB,]$Time.Per.Play)
  
  away.yards.per.pass.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Yards.Per.Pass, 
                                           sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Yards.Per.Pass)
  away.yards.per.carry.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Yards.Per.Carry, 
                                            sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Yards.Per.Carry)
  away.points.per.play.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Points.Per.Play, 
                                            sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Points.Per.Play)
  away.penalties.per.play.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Penalties.Per.Play, 
                                               sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Penalties.Per.Play)
  away.turnovers.per.play.adjustment  <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Int.Per.Pass, 
                                                sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Int.Per.Pass)
  away.possession.percent.adjustment <- rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Possession.Percent, 
                                              sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Possession.Percent)
  away.yards.per.pass.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Opp.Yards.Per.Pass, 
                                                   sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Opp.Yards.Per.Pass)
  away.yards.per.carry.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Opp.Yards.Per.Carry, 
                                                    sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Opp.Yards.Per.Carry)
  away.points.per.play.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Opp.Points.Per.Play, 
                                                    sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Opp.Points.Per.Play)
  away.penalties.per.play.allowed.adjustment <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Opp.Penalties.Per.Play, 
                                                       sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Opp.Penalties.Per.Play)
  away.takeaways.per.play.adjustment  <-  rnorm(n=runCount, mean=adjustment.Averages[adjustment.Averages$TEAM == teamB,]$Opp.Int.Per.Pass, 
                                                sd=adjustment.Std.Dev[adjustment.Std.Dev$TEAM == teamB,]$Opp.Int.Per.Pass)
  
  setup <- data.frame(home.team=teamA, away.team=teamB, home.yards.per.pass, home.yards.per.carry, 
                      home.points.per.play,  home.penalties.per.play ,home.turnovers.per.play,  
                      home.possession.percent , home.yards.per.pass.allowed ,home.yards.per.carry.allowed, 
                      home.points.per.play.allowed,  home.penalties.per.play.allowed , home.takeaways.per.play,  
                      home.percent.pass ,    home.percent.run ,   home.time.per.play ,   home.yards.per.pass.adjustment ,
                      home.yards.per.carry.adjustment ,  home.points.per.play.adjustment ,  home.penalties.per.play.adjustment ,
                      home.turnovers.per.play.adjustment  , home.possession.percent.adjustment ,
                      home.yards.per.pass.allowed.adjustment ,  home.yards.per.carry.allowed.adjustment ,
                      home.points.per.play.allowed.adjustment ,    home.penalties.per.play.allowed.adjustment ,
                      home.takeaways.per.play.adjustment  , away.yards.per.pass , away.yards.per.carry ,
                      away.points.per.play ,    away.penalties.per.play, away.turnovers.per.play , 
                      away.possession.percent ,  away.yards.per.pass.allowed ,  away.yards.per.carry.allowed ,
                      away.points.per.play.allowed , away.penalties.per.play.allowed, away.takeaways.per.play , 
                      away.percent.pass, away.percent.run,away.time.per.play, away.yards.per.pass.adjustment, 
                      away.yards.per.carry.adjustment, away.points.per.play.adjustment,away.penalties.per.play.adjustment, 
                      away.turnovers.per.play.adjustment,away.possession.percent.adjustment,
                      away.yards.per.pass.allowed.adjustment, away.yards.per.carry.allowed.adjustment, 
                      away.points.per.play.allowed.adjustment,  away.penalties.per.play.allowed.adjustment, 
                      away.takeaways.per.play.adjustment, home.yards.per.penalty, away.yards.per.penalty, home.sos, away.sos)
  
  return(setup)
  
}

neutral.game <- function(row)
{
  game <- c(row[["home.team"]], 0, 0,0,0 ,0 ,
            row[["away.team"]],0 , 0,0,0 , 0)
  names(game) <- c("Home", "Home Pass", "Home Run", "Home Penalty", "Home Turnovers", "Home Score",
                   "Away", "Away Pass", "Away Run", "Away Penalty", "Away Turnovers", "Away Score")
  
  home.SoS.adjust <- Offensive.SoS.Compare(as.numeric(row[["home.sos"]]),
                                           as.numeric(row[["away.sos"]]))
  away.SoS.adjust <- Defensive.SoS.Compare(as.numeric(row[["home.sos"]]),
                                           as.numeric(row[["away.sos"]]))
  home.plays <- (as.numeric(row[["home.possession.percent"]])*3600)/max(17.5, as.numeric(row[["home.time.per.play"]]))
  home.pass.plays <- as.numeric(row[["home.percent.pass"]])*home.plays
  home.run.plays <- as.numeric(row[["home.percent.run"]])*home.plays
  home.penalties <-((1.75*as.numeric(row[["home.penalties.per.play"]]) +
                       1.25*as.numeric(row[["away.penalties.per.play.allowed"]]))/3)*home.plays
  home.turnovers<-((2*as.numeric(row[["home.turnovers.per.play"]]) +
                      1*as.numeric(row[["away.takeaways.per.play"]]))/3)*home.plays
  
  away.plays <- (as.numeric(row[["away.possession.percent"]])*3600)/max(17.5, as.numeric(row[["away.time.per.play"]]))
  away.pass.plays <- as.numeric(row[["away.percent.pass"]])*away.plays
  away.run.plays <- as.numeric(row[["away.percent.run"]])*away.plays
  away.penalties <-((1.75*as.numeric(row[["away.penalties.per.play"]]) +
                       1.25*as.numeric(row[["home.penalties.per.play.allowed"]]))/3)*away.plays
  away.turnovers<-((2*as.numeric(row[["away.turnovers.per.play"]]) +
                      1*as.numeric(row[["home.takeaways.per.play"]]))/3)*away.plays
  
  home.penalty.yards <- home.penalties*as.numeric(row[["home.yards.per.penalty"]])  
  away.penalty.yards <- away.penalties*as.numeric(row[["away.yards.per.penalty"]])
  
  game[["Home Turnovers"]] <- max(0,home.turnovers)
  game[["Away Turnovers"]] <- max(0,away.turnovers)
  
  game[["Home Penalty"]] <- max(0,home.penalty.yards)
  game[["Away Penalty"]] <- max(0,away.penalty.yards)
  
  home.rush.yards <- max(0,home.run.plays*as.numeric(row[["home.yards.per.carry"]])*
                           as.numeric(row[["away.yards.per.carry.allowed.adjustment"]]))
  away.rush.yards.allowed <- max(0, home.run.plays*as.numeric(row[["home.yards.per.carry.adjustment"]])*
                                   as.numeric(row[["away.yards.per.carry.allowed"]]))
  
  away.rush.yards <- max(0,away.run.plays*as.numeric(row[["away.yards.per.carry"]])*
                           as.numeric(row[["home.yards.per.carry.allowed.adjustment"]]))
  
  home.rush.yards.allowed <- max(0, away.run.plays*as.numeric(row[["away.yards.per.carry.adjustment"]])*
                                   as.numeric(row[["home.yards.per.carry.allowed"]]))
  
  home.rush.yards <- (1.1*home.rush.yards + 0.9*away.rush.yards.allowed )/2
  away.rush.yards <- (1.1*away.rush.yards + 0.9*home.rush.yards.allowed )/2
  
  game[["Home Run"]] <- home.SoS.adjust*home.rush.yards
  game[["Away Run"]] <- away.SoS.adjust*away.rush.yards
  
  home.pass.yards <- max(0,home.pass.plays*as.numeric(row[["home.yards.per.pass"]])*
                           as.numeric(row[["away.yards.per.pass.allowed.adjustment"]]))
  away.pass.yards.allowed <- max(0, home.pass.plays*as.numeric(row[["home.yards.per.pass.adjustment"]])*
                                   as.numeric(row[["away.yards.per.pass.allowed"]]))
  
  away.pass.yards <- max(0,away.pass.plays*as.numeric(row[["away.yards.per.pass"]])*
                           as.numeric(row[["home.yards.per.pass.allowed.adjustment"]]))
  
  home.pass.yards.allowed <- max(0, away.pass.plays*as.numeric(row[["away.yards.per.pass.adjustment"]])*
                                   as.numeric(row[["home.yards.per.pass.allowed"]]))
  
  home.pass.yards <- (1.1*home.pass.yards + 0.9*away.pass.yards.allowed )/2
  away.pass.yards <- (1.1*away.pass.yards + 0.9*home.pass.yards.allowed )/2
  
  game[["Home Pass"]] <- home.SoS.adjust*home.pass.yards
  game[["Away Pass"]] <- away.SoS.adjust*away.pass.yards
  
  home.points <- max(0,home.plays*as.numeric(row[["home.points.per.play"]])*
                       as.numeric(row[["away.points.per.play.allowed.adjustment"]]))
  away.points.allowed <- max(0, home.plays*as.numeric(row[["home.points.per.play.adjustment"]])*
                               as.numeric(row[["away.points.per.play.allowed"]]))
  
  away.points <- max(0,away.plays*as.numeric(row[["away.points.per.play"]])*
                       as.numeric(row[["home.points.per.play.allowed.adjustment"]]))
  
  home.points.allowed <- max(0, away.plays*as.numeric(row[["away.points.per.play.adjustment"]])*
                               as.numeric(row[["home.points.per.play.allowed"]]))
  
  home.points <- (1.1*home.points + 0.9*away.points.allowed )/2
  away.points <- (1.1*away.points + 0.9*home.points.allowed )/2
  
  turnover.margin <- (home.turnovers-away.turnovers)
  yardage.margin <-  ((home.pass.yards+home.rush.yards)-(away.pass.yards+away.rush.yards))/100
  penalty.margin <- (away.penalty.yards-home.penalty.yards)/100
  
  game[["Home Score"]] <- max(0, ((home.SoS.adjust*home.points)+ (1.75*turnover.margin) +
                                    (1.75*yardage.margin) + (1.75*penalty.margin)))
  
  
  game[["Away Score"]] <- max(0, ((away.SoS.adjust*away.points)+ (-1.75*turnover.margin) +
                                    (-1.75*yardage.margin) + (-1.75*penalty.margin)))
  return(game)
}

game <- function(row)
{
  game <- c(row[["home.team"]], 0, 0,0,0 ,0 ,
            row[["away.team"]],0 , 0,0,0 , 0)
  names(game) <- c("Home", "Home Pass", "Home Run", "Home Penalty", "Home Turnovers", "Home Score",
                   "Away", "Away Pass", "Away Run", "Away Penalty", "Away Turnovers", "Away Score")
  
  home.SoS.adjust <- Offensive.SoS.Compare(as.numeric(row[["home.sos"]]),
                                           as.numeric(row[["away.sos"]]))
  away.SoS.adjust <- Defensive.SoS.Compare(as.numeric(row[["home.sos"]]),
                                           as.numeric(row[["away.sos"]]))
  home.plays <- (as.numeric(row[["home.possession.percent"]])*3600)/max(17.5, as.numeric(row[["home.time.per.play"]]))
  home.pass.plays <- as.numeric(row[["home.percent.pass"]])*home.plays
  home.run.plays <- as.numeric(row[["home.percent.run"]])*home.plays
  home.penalties <-((1.5*as.numeric(row[["home.penalties.per.play"]]) +
                       1.25*as.numeric(row[["away.penalties.per.play.allowed"]]))/3)*home.plays
  home.turnovers<-((2*as.numeric(row[["home.turnovers.per.play"]]) +
                      1*as.numeric(row[["away.takeaways.per.play"]]))/3)*home.plays
  
  away.plays <- (as.numeric(row[["away.possession.percent"]])*3600)/max(17.5, as.numeric(row[["away.time.per.play"]]))
  away.pass.plays <- as.numeric(row[["away.percent.pass"]])*away.plays
  away.run.plays <- as.numeric(row[["away.percent.run"]])*away.plays
  away.penalties <-((2*as.numeric(row[["away.penalties.per.play"]]) +
                       1.25*as.numeric(row[["home.penalties.per.play.allowed"]]))/3)*away.plays
  away.turnovers<-((2*as.numeric(row[["away.turnovers.per.play"]]) +
                      1*as.numeric(row[["home.takeaways.per.play"]]))/3)*away.plays
  
  home.penalty.yards <- home.penalties*as.numeric(row[["home.yards.per.penalty"]])  
  away.penalty.yards <- away.penalties*as.numeric(row[["away.yards.per.penalty"]])
  
  game[["Home Turnovers"]] <- max(0,home.turnovers)
  game[["Away Turnovers"]] <- max(0,away.turnovers)
  
  game[["Home Penalty"]] <- max(0,home.penalty.yards)
  game[["Away Penalty"]] <- max(0,away.penalty.yards)
  
  home.rush.yards <- max(0,home.run.plays*as.numeric(row[["home.yards.per.carry"]])*
                           as.numeric(row[["away.yards.per.carry.allowed.adjustment"]]))
  away.rush.yards.allowed <- max(0, home.run.plays*as.numeric(row[["home.yards.per.carry.adjustment"]])*
                                   as.numeric(row[["away.yards.per.carry.allowed"]]))
  
  away.rush.yards <- max(0,away.run.plays*as.numeric(row[["away.yards.per.carry"]])*
                           as.numeric(row[["home.yards.per.carry.allowed.adjustment"]]))
  
  home.rush.yards.allowed <- max(0, away.run.plays*as.numeric(row[["away.yards.per.carry.adjustment"]])*
                                   as.numeric(row[["home.yards.per.carry.allowed"]]))
  
  home.rush.yards <- (1.1*home.rush.yards + 0.9*away.rush.yards.allowed )/2
  away.rush.yards <- (1.1*away.rush.yards + 0.9*home.rush.yards.allowed )/2
  
  game[["Home Run"]] <- home.SoS.adjust*home.rush.yards
  game[["Away Run"]] <- away.SoS.adjust*away.rush.yards
  
  home.pass.yards <- max(0,home.pass.plays*as.numeric(row[["home.yards.per.pass"]])
                         *as.numeric(row[["away.yards.per.pass.allowed.adjustment"]]))
  away.pass.yards.allowed <- max(0, home.pass.plays*as.numeric(row[["home.yards.per.pass.adjustment"]])
                                 *as.numeric(row[["away.yards.per.pass.allowed"]]))
  
  away.pass.yards <- max(0,away.pass.plays*as.numeric(row[["away.yards.per.pass"]])
                         *as.numeric(row[["home.yards.per.pass.allowed.adjustment"]]))
  
  home.pass.yards.allowed <- max(0, away.pass.plays*as.numeric(row[["away.yards.per.pass.adjustment"]])
                                 *as.numeric(row[["home.yards.per.pass.allowed"]]))
  
  home.pass.yards <- (1.1*home.pass.yards + 0.9*away.pass.yards.allowed )/2
  away.pass.yards <- (1.1*away.pass.yards + 0.9*home.pass.yards.allowed )/2
  
  game[["Home Pass"]] <- home.SoS.adjust*home.pass.yards
  game[["Away Pass"]] <- away.SoS.adjust*away.pass.yards
  
  home.points <- max(0,home.plays*as.numeric(row[["home.points.per.play"]])*
                       as.numeric(row[["away.points.per.play.allowed.adjustment"]]))
  away.points.allowed <- max(0, home.plays*as.numeric(row[["home.points.per.play.adjustment"]])*
                               as.numeric(row[["away.points.per.play.allowed"]]))
  
  away.points <- max(0,away.plays*as.numeric(row[["away.points.per.play"]])*
                       as.numeric(row[["home.points.per.play.allowed.adjustment"]]))
  
  home.points.allowed <- max(0, away.plays*as.numeric(row[["away.points.per.play.adjustment"]])*
                               as.numeric(row[["home.points.per.play.allowed"]]))
  
  home.points <- (1.1*home.points + 0.9*away.points.allowed )/2
  away.points <- (1.1*away.points + 0.9*home.points.allowed )/2
  
  turnover.margin <- (home.turnovers-away.turnovers)
  yardage.margin <-  ((home.pass.yards+home.rush.yards)-(away.pass.yards+away.rush.yards))/100
  penalty.margin <- (away.penalty.yards-home.penalty.yards)/100
  
  game[["Home Score"]] <- max(0, (1.5+(home.SoS.adjust*home.points)+ (1.75*turnover.margin) +
                                    (1.75*yardage.margin) + (1.75*penalty.margin)))
  
  
  game[["Away Score"]] <- max(0, (-1.5+(away.SoS.adjust*away.points)+ (-1.75*turnover.margin) +
                                    (-1.75*yardage.margin) + (-1.75*penalty.margin)))
  return(game)
}


neutral.predict <- function(teamA, teamB, runCount)
{
  setup <- setup.predict(teamA,teamB, runCount)
  games <- apply(FUN = neutral.game, MARGIN = 1, setup)
  rm(setup)
  games <- data.frame( Home.Team = games["Home", ], 
                       Home.Pass = as.numeric(games[ "Home Pass",]),
                       Home.Run = as.numeric(games["Home Run", ]), 
                       Home.Penalty = as.numeric(games[ "Home Penalty",]),
                       Home.Turnover = as.numeric(games["Home Turnovers", ]), 
                       Home.Score = as.numeric(games["Home Score", ]),
                       Away.Team = games["Away", ], 
                       Away.Pass = as.numeric(games[ "Away Pass",]),
                       Away.Run = as.numeric(games["Away Run", ]), 
                       Away.Penalty = as.numeric(games[ "Away Penalty",]),
                       Away.Turnover = as.numeric(games["Away Turnovers", ]), 
                       Away.Score = as.numeric(games["Away Score", ]),
                       row.names = NULL)
  box.score <- aggregate(cbind(Home.Pass,Home.Run, Home.Penalty, Home.Turnover, 
                               Home.Score)~Home.Team, data = games, FUN= mean)
  away.box <- aggregate(cbind(Away.Pass,Away.Run, Away.Penalty, Away.Turnover, 
                              Away.Score)~Away.Team, data=games, FUN=mean)
  box.score$Wins <-length(games[games$Home.Score > games$Away.Score,]$Home.Score)
  away.box$Wins <-length(games[games$Home.Score < games$Away.Score,]$Away.Score)
  
  names(box.score) <- c("Team", "Pass Yards", "Rush Yards", "Penalty Yards", "Turnovers", "Score", "WINS")
  names(away.box) <- c("Team", "Pass Yards", "Rush Yards", "Penalty Yards", "Turnovers", "Score", "WINS")
  
  box.score <- rbind(box.score, away.box)
  
  return(box.score)
}

predict <- function(teamA, teamB, runCount)
{
  
  setup <- setup.predict(teamA,teamB, runCount)
  games <- apply(FUN =game, MARGIN = 1, setup)
  rm(setup)
  games <- data.frame( Home.Team = games["Home", ], 
                       Home.Pass = as.numeric(games[ "Home Pass",]),
                       Home.Run = as.numeric(games["Home Run", ]), 
                       Home.Penalty = as.numeric(games[ "Home Penalty",]),
                       Home.Turnover = as.numeric(games["Home Turnovers", ]), 
                       Home.Score = as.numeric(games["Home Score", ]),
                       Away.Team = games["Away", ], 
                       Away.Pass = as.numeric(games[ "Away Pass",]),
                       Away.Run = as.numeric(games["Away Run", ]), 
                       Away.Penalty = as.numeric(games[ "Away Penalty",]),
                       Away.Turnover = as.numeric(games["Away Turnovers", ]), 
                       Away.Score = as.numeric(games["Away Score", ]),
                       row.names = NULL)
  box.score <- aggregate(cbind(Home.Pass,Home.Run, Home.Penalty, Home.Turnover, 
                               Home.Score)~Home.Team, data = games, FUN= mean)
  away.box <- aggregate(cbind(Away.Pass,Away.Run, Away.Penalty, Away.Turnover, 
                              Away.Score)~Away.Team, data=games, FUN=mean)
  box.score$Wins <-length(games[games$Home.Score > games$Away.Score,]$Home.Score)
  away.box$Wins <-length(games[games$Home.Score < games$Away.Score,]$Away.Score)
  
  names(box.score) <- c("Team", "Pass Yards", "Rush Yards", "Penalty Yards", "Turnovers", "Score", "WINS")
  names(away.box) <- c("Team", "Pass Yards", "Rush Yards", "Penalty Yards", "Turnovers", "Score", "WINS")
  
  box.score <- rbind(box.score, away.box)
  
  return(box.score)
}
