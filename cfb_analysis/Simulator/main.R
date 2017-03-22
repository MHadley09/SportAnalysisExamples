readinteger <- function(text)
{ 
  n <- readline(prompt=text)
  if(!grepl("^[0-9]+$",n))
  {
    t <- text
    return(readinteger(t))
  }
  
  return(as.integer(n))
}

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

select.simulator <- function()
{
  options <- data.frame(Option=as.character(),Description=as.character(), Simulator=as.character(), stringsAsFactors=FALSE)
  options[1,] <-  c("B", "Run standard bell simulator", "better_complex_predict.r")
  options[2,] <- c("S", "Run scores only simulator", "play_by_play_points_predict.r")
  options[3,] <- c("P", "Run play by play simulator", "play_by_play_predict.r")
  options[4,] <- c("O", "Run play by play simulator with manual offsets", "offset_play_by_play_predict.r" )
  predictChoice <- character();
  while(length(predictChoice) == 0)
  {
    print(data.frame(options$Option, options$Description))
    input <- readline("Select simulator to use:\t")
    predictChoice <-  options[options$Option == toupper(as.character(input)),]$Simulator
    if(length(predictChoice) == 0){
      print("Invalid Selection")
    }    
  }
  return(predictChoice)
}

homeAway <- function(predictor)
{
  source(predictor)
  homeTeam <- readline("Home team name(must be exact match of data):\t")
  awayTeam <- readline("Away team name(must be exact match of data):\t")
  runs <- readinteger("How many games should we simulate:\t")
  print(predict(homeTeam,awayTeam,runs))
  print("")
}

neutralSite <- function(predictor)
{
  source(predictor)
  teamA <- readline("First team name(must be exact match of data):\t")
  teamB <- readline("Second team name(must be exact match of data):\t")
  runs <- readinteger("How many games should we simulate:\t")
  print(neutral.predict(teamA,teamB,runs))
  print("")
}

homeAwayBet <- function()
{
  source("betting_predictor.R")
  homeTeam <- readline("Home team name(must be exact match of data):\t")
  awayTeam <- readline("Away team name(must be exact match of data):\t")
  spread <- read.double("What is the spread for or against the home team:\t")
  over <- read.double("What is the over?\t")
  runs <- readinteger("How many games should we simulate:\t")
  print(predict(homeTeam,awayTeam, spread, over, runs))
  print("")
}

neutralSiteBet <- function()
{
  source("betting_predictor.R")
  homeTeam <- readline("First team name(must be exact match of data):\t")
  awayTeam <- readline("Second team name(must be exact match of data):\t")
  spread <- read.double("What is the spread for or against the first team:\t")
  over <- read.double("What is the over?\t")
  runs <- readinteger("How many games should we simulate:\t")
  print(neutral.predict(homeTeam,awayTeam, spread, over, runs))
  print("")
}

select.simulator <- function()
{
  options <- data.frame(Option=as.character(),Description=as.character(), Simulator=as.character(), stringsAsFactors=FALSE)
  options[1,] <-  c("B", "Run standard bell simulator", "better_complex_predict.r")
  options[2,] <- c("S", "Run scores only simulator", "play_by_play_points_predict.r")
  options[3,] <- c("P", "Run play by play simulator", "play_by_play_predict.r")
  options[4,] <- c("O", "Run play by play simulator with manual offsets", "offset_play_by_play_predict.r" )
  
  pc <- character();
  while(length(pc) == 0)
  {
    print(data.frame(options$Option, options$Description))
    input <- readline("Select simulator to use:\t")
    pc <-  options[options$Option == toupper(as.character(input)),]$Simulator
    if(length(pc) == 0){
      print("Invalid Selection")
    }    
  }
  return(pc)
}

simulator.selection <- function(){
  
  predictChoice <- select.simulator();
  while(T){
    input <- readline("Do you want to simulate game at a neutral site? (Y/N)  ")
    if(input == "N" || input == "n")
    {
      tryCatch(homeAway(predictChoice), error=function(e){
        print("An error occurred, most likely an invalid team name was given. Returning to menu")
      },
      warning=function(w){
        print("A warning occurred, most likely an invalid team name was given. Returning to menu")
      })
      return()
    } 
    else if(input == "Y" || input == "y")
    {
      tryCatch(neutralSite(predictChoice), error=function(e){
        print("An error occurred, most likely an invalid team name was given. Returning to menu")
      },
      warning=function(w){
        print("A warning occurred, most likely an invalid team name was given. Returning to menu")
      })
      return()
    }
    else
    {
      print("Invalid input! Try again.")
    }
  }
  
}

bet.selection <- function(){
  
  while(T){
    input <- readline("Do you want to simulate game at a neutral site? (Y/N)  ")
    if(input == "N" || input == "n")
    {
      tryCatch(homeAwayBet(), error=function(e){
        print("An error occurred, most likely an invalid team name was given. Returning to menu")
      },
      warning=function(w){
        print("A warning occurred, most likely an invalid team name was given. Returning to menu")
      })
      return()
    } 
    else if(input == "Y" || input == "y")
    {
      tryCatch(neutralSiteBet(), error=function(e){
        print("An error occurred, most likely an invalid team name was given. Returning to menu")
      },
      warning=function(w){
        print("A warning occurred, most likely an invalid team name was given. Returning to menu")
      })
      return()
    }
    else
    {
      print("Invalid input! Try again.")
    }
  }
  
}
rankings.selection <- function()
{
  while(T)
  {
    
    input <- readline("Running rankings may take a while. Are you sure you want to continue? (Y/N)  ")
    if(input == "N" || input == "n")
    {
      return()
    } 
    else if(input == "Y" || input == "y")
    {
      predictChoice <- select.simulator();

      source('ranking.r')
      rCount <- readinteger("How many times would you like to simulate each match?\t")
      print("Select location to write output.")
      fname <- file.choose()
      print("Running rankings.  Please wait.")
      write.table(calculate.rankings.typed(predictChoice, rCount), fname, row.names=FALSE, sep=",") 
      return()
    }
    else{
      print("Invalid input try again!")
    }
  }
}

conference.ranking.selection <- function()
{
  while(T)
  {
    input <- readline("Running rankings may take a while. Are you sure you want to continue? (Y/N)  ")
    if(input == "N" || input == "n")
    {
      return()
    } 
    else if(input == "Y" || input == "y")
    {
      predictChoice <- select.simulator();
    
      source('conferenceRanking.r')
      conference <- readline("Which conference would you like to rank?\t")
      rCount <- readinteger("How many times would you like to simulate each match?\t")      
      print("Select location to write output.")
      fname <- file.choose()
      print("Running rankings.  Please wait.")
      write.table(conf.rankings(conference, predictChoice, rCount), fname, row.names=FALSE, sep=",") 
      return()
    }
    else{
      print("Invalid input try again!")
    }
  }
}

simulator <- function(){
  options <- data.frame(Option=as.character(),Description=as.character(),  stringsAsFactors=FALSE)
  options[1,] <-  c("(R)", "Run full rankings")
  options[2,] <- c("(C)", "Run conference rankings")
  options[3,] <- c("(S)", "Simulate a match up")
  options[4,] <- c("(B)", "Generate betting odds")
  options[5,] <- c("(Q)", "Quit Application")
  while(T)
  {
    print("Choose from following options.")
    print(options)
    input <- readline("Input selection letter:\t")
    if(input == "Q" || input == "q")
    {
      break
    } 
    else if(input == "S" || input == "s")
    {
      tryCatch(simulator.selection(), error=function(e){
        print("An error occurred returning to menu")
      },
      warning=function(w){
        print("A warning was thrown returning to menu")
      })
    }
    else if(input == "C" || input == "c")
    {
      tryCatch(conference.ranking.selection(), error=function(e){
        print("An error occurred, most likely a bad conference name, returning to menu")
      },
      warning=function(w){
        print("A warning was thrown, most likely a bad conference name, returning to menu")
      })
    }
    else if(input == "R" || input == "r")
    {
      tryCatch(rankings.selection(), error=function(e){
        print("An error occurred returning to menu")
      },
      warning=function(w){
        print("A warning was thrown returning to menu")
      })
    }
    else if(input == "B" || input == "b")
    {
      tryCatch(bet.selection(), error=function(e){
        print("An error occurred returning to menu")
      },
      warning=function(w){
        print("A warning was thrown returning to menu")
      })
    }
    else{
      print("Invalid input try again!")
    }
  }
  
}
