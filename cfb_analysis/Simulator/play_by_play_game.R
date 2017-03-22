plays <- read.csv(file = 'data\\playbyplay.csv', sep=',', header=T)

plays$quarterValue <- 0
plays[plays$quarter == "1ST",]$quarterValue <- 1
plays[plays$quarter == "2ND",]$quarterValue <- 2
plays[plays$quarter == "3RD",]$quarterValue <- 3
plays[plays$quarter == "4TH",]$quarterValue <- 4
plays[plays$quarter == "OT",]$quarterValue <- 5

plays[is.na(plays$down), ]$down <- 10
plays[trim(plays$distance) == "",]$distance <- -15
plays[is.na(plays$yardsFromPlay),]$yardsFromPlay <- 0

teamNum <- 76
cleanRowsPlaybook <- function(row)
{
  result <- c(as.character(row[["gameDate"]]), as.character(row[["homeTeamCode"]]),  
              as.character(row[["awayTeamCode"]]), as.character(row[["play_type"]]),
              as.numeric(row[["quarterValue"]]), as.numeric((row[["down"]])),
              as.numeric(row[["distance"]]), as.character(row[["direction"]]),
              as.character(row[["player"]]), as.numeric(row[["player_number"]]),
              as.character(row[[("receiver")]]), as.character(row[["receiver_number"]]),
              as.numeric(row[["homeScore"]]), as.numeric(row[["visitorScore"]]),
              as.numeric(row[["yardsFromPlay"]]), as.character(row[["offenseTeamCode"]]),
              as.character(row[["defenseTeamCode"]]), as.numeric(row[["yardLine"]]), 0)
  names(result) <- c("Game.Date", "Home.Team", "Away.Team", "Play.Type", "Quarter" ,"Down", 
                    "Distance", "Direction", "Player", "Player.Number", "Receiver",
                    "Receiver.Number", "Home.Score","Away.Score", "Yards", "Offense", 
                    "Defense", "Yard.Line", "Drive.Time")

  timer <- strsplit(paste("0", as.character(row[["timeDriveStart"]]), sep=""), ":")
  result[["Drive.Time"]] <- ((4-as.numeric(row[["quarterValue"]]))*900)+((60*as.numeric(timer[[1]])[1])
                                      +as.numeric(timer[[1]])[2])
  return(result)
}

clean <- apply(plays, 1, cleanRowsPlaybook)

clean <-  data.frame(Game.Date = as.character(clean["Game.Date",]),
                       Home.Team = as.character(clean["Home.Team",]),
                       Away.Team = as.character(clean["Away.Team",]),
                      Play.Type = as.character(clean["Play.Type",]),
                       Quarter = as.numeric(clean["Quarter",]),
                       Down = as.numeric((clean["Down",])),
                       Distance = as.numeric(clean["Distance",]),
                       Direction = as.character(clean["Direction",]),
                       Player = as.character(clean["Player",]),
                       Player.Number = as.numeric(clean["Player.Number",]),
                       Receiver = as.character(clean["Receiver",]),
                       Receiver.Number = as.character(clean["Receiver.Number",]),
                       Home.Score = as.numeric(clean["Home.Score",]),
                       Away.Score = as.numeric(clean["Away.Score",]),
                       Yards = as.numeric(clean["Yards",]),
                       Offense = as.character(clean["Offense",]),
                       Defense = as.character(clean["Defense",]),
                       Yard.Line = as.numeric(clean["Yard.Line",]),
                       Drive.Time = as.numeric(clean["Drive.Time",]),
                       row.names=NULL)

teams <- tapply(clean$Offense, clean$Offense, length)
teams <- data.frame(Team = row.names(teams), Plays = teams, row.names=NULL)
teams$playRank <- 1 + length(teams$Plays) - rank(teams$Plays)
teams <- teams[teams$playRank <= 128,]
teams$playRank <- NULL

playbook <- clean[clean$Offense == as.character(teams[teamNum,]$Team),]
playbook[is.na(playbook$Distance),]$Distance <- -15 
generalBreakdown <- tapply(playbook$Play.Type, playbook$Play.Type, length)
generalBreakdown <- data.frame(type = row.names(generalBreakdown), Plays = generalBreakdown, row.names=NULL)


team <- as.character(teams[teamNum,]$Team)


down <- 3
distance <- 1
quarter <- 3
time <- "6:31"

yardline <- 35
score <- 21
oppScore <- 21
timer <- strsplit(paste("0", time, sep=""), ":")

time <- 1793

temp <- tapply(playbook$Direction, playbook$Direction, length)
temp[is.na(temp)] <- 0
teamAbbr <- names(temp)[temp == max(temp)]


direction <- "teamAbbr"

play_likelihood <- function(row)
{
  result <- c("", 0, 0)
  names(result) <- c("Type", "Plays", "Yards")
  downWeight <- ifelse((as.numeric(row[["Down"]])==down), 1 , 0 ) 
  timeWeight <- 1/(1+abs(time-as.numeric(row[["Drive.Time"]])))
  home <- ifelse(as.character(row[["Offense"]])==as.character(row[["Home.Team"]]), 1, -1)
  scoreWeight <- 1/(1+abs((score-oppScore)
                          -(home*(as.numeric(row[["Home.Score"]])-as.numeric(row[["Away.Score"]])))))
  distanceWeight <- 1/abs(1 + (2*(as.numeric(row[["Distance"]])-distance)))
  playYardLine <- ifelse(teamAbbr == as.character(row[["Direction"]]), 
                         100-(as.numeric(row[["Yard.Line"]])), as.numeric(row[["Yard.Line"]]))
  curYardLine <- ifelse(teamAbbr == direction, 
                         100-yardline, yardline)
  diff <- abs(playYardLine - curYardLine)
  lineWeight <- ifelse(diff > 15.5 , -.25, (0.00000658*(diff^4)) - (.00563*(diff^2)) + 1)
  weight <- max(0,(downWeight*((1.5*scoreWeight)+timeWeight+distanceWeight+3*lineWeight)))
  result[["Type"]] <- as.character(row[["Play.Type"]])
  result[["Weight"]] <- weight
  result[["Yards"]] <- weight*as.numeric(row[["Yards"]])        
  return(result)
}

situational_defense <- function(row)
{
  result <- c("", 0, 0)
  names(result) <- c("Type", "Plays", "Yards")
  downWeight <- ifelse((as.numeric(row[["Down"]])==down), 1 , 0 ) 
  timeWeight <- 1/(1+abs(time-as.numeric(row[["Drive.Time"]])))
  home <- ifelse(as.character(row[["Defense"]])==as.character(row[["Home.Team"]]), 1, -1)
  scoreWeight <- 1/(1+abs((score-oppScore)
                          -(home*(as.numeric(row[["Home.Score"]])-as.numeric(row[["Away.Score"]])))))
  distanceWeight <- 1/abs(1 + (2*(as.numeric(row[["Distance"]])-distance)))
  playYardLine <- ifelse(teamAbbr == as.character(row[["Direction"]]), 
                         100-(as.numeric(row[["Yard.Line"]])), as.numeric(row[["Yard.Line"]]))
  curYardLine <- ifelse(teamAbbr == direction, 
                        100-yardline, yardline)
  diff <- abs(playYardLine - curYardLine)
  lineWeight <- ifelse(diff > 15.5 , -.25, (0.00000658*(diff^4)) - (.00563*(diff^2)) + 1)
  weight <- max(0,(downWeight*((1.5*scoreWeight)+timeWeight+distanceWeight+3*lineWeight)))
  result[["Type"]] <- as.character(row[["Play.Type"]])
  result[["Weight"]] <- weight
  result[["Yards"]] <- as.numeric(row[["Yards"]])        
  return(result)
}



predict <- apply(playbook, 1, situational_defense)

predict <- data.frame(Type = as.character(predict["Type",]),
                      Weight = as.numeric(predict["Weight",]),
                      Yards = as.numeric(predict["Yards",]),
                      Sd = 0,
                      row.names=NULL)

variation <- data.frame(Type = as.character(predict$Type),
                      Weight = as.numeric(predict$Weight),
                      Yards = as.numeric(predict$Yards),
                      Sd = 0,
                      row.names=NULL)


predict <- aggregate(cbind(Weight, Yards) ~ Type, data = predict, FUN=sum)

for(i in 1:length(predict$Type))
{
  predict[i,]$Yards <- mean(variations[variations$Type == as.character(predict[i,]$Type),]$Weight,
                               variations[variations$Type == as.character(predict[i,]$Type),]$Yards)
  predict[i,]$Sd <- wt.sd(variations[variations$Type == as.character(predict[i,]$Type),]$Weight,
                               variations[variations$Type == as.character(predict[i,]$Type),]$Yards)
}


#return(predict)
#}
# runs <- plays[plays$play_type == "RUSH",]
# passes <- plays[plays$play_type == "PASS",]
# 
# passes[is.na(passes$yardsFromPlay),]$yardsFromPlay <- 0
# passes$joined <- paste(passes$player, passes$offenseTeamCode, sep="||")
# length(unique(passes$joined))
# 
# totals <- aggregate(cbind(1, is_complete, is_touchdown, yardsFromPlay)~joined, data = passes, FUN=sum, na.action=)
# 
# names(totals)[2] <- "attempts"
# 
# 
# qualifying <- totals[totals$attempts > 100,]
# 
# qualifying[,]$row.names <- NULL
# 
# max_completion_per <- max(qualifying$is_complete/qualifying$attempts)
# max_tds_per <- max(qualifying$is_touchdown/qualifying$attempts)
# max_yards_per <- max(qualifying$yardsFromPlay/qualifying$attempts)
# min_completion_per <- min(qualifying$is_complete/qualifying$attempts)
# min_tds_per <- min(qualifying$is_touchdown/qualifying$attempts)
# min_yards_per <- min(qualifying$yardsFromPlay/qualifying$attempts)
# 
# qb_score <- function(row)
# {
# 
#   completionScore <- (((as.numeric(row[["is_complete"]])/as.numeric(row[["attempts"]]))
#                        -min_completion_per)/(max_completion_per - min_completion_per))
#   tdScore <- (((as.numeric(row[["is_touchdown"]])/as.numeric(row[["attempts"]]))
#                -min_tds_per)/(max_tds_per - min_tds_per))
#   yardsScore <- (((as.numeric(row[["yardsFromPlay"]])/as.numeric(row[["attempts"]]))
#                   -min_yards_per)/(max_yards_per - min_yards_per))
#   QB <- as.character(row[["joined"]])
#   score <- 100*(.5*completionScore + .3*tdScore + .2*yardsScore)
#   return(c(QB, score))
# }
# 
# scores <- apply(FUN=qb_score, MARGIN=1, qualifying)
# 
# 
# scores  <- data.frame( QB = as.character(scores[1,]), 
#                     score = as.numeric(scores[2,]),
#                      row.names = NULL)
# 
# scores$Ranking <- 1 + length(scores$score) - rank(scores$score)
# 
