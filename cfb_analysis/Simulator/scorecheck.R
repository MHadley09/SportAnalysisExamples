predict <- function(eloA, eloB)
{
  return (1/(1+10^((eloB-eloA)/400))) 
}

box.data <- read.csv(file = 'data\\2016\\perplaydata.csv', sep=',', header=T)
polls9 <-  read.csv(file = 'data\\2016\\week 9\\mle.csv', sep=',', header=T)
polls10<-  read.csv(file = 'data\\2016\\week 10\\mle.csv', sep=',', header=T)


box.data  <- box.data[box.data$Home.Team %in% polls9$Team,]
box.data  <- box.data[box.data$Visitor.Team %in% polls9$Team,]

box.data.future <- box.data[as.Date(box.data$Date,"%m/%d/%Y") >= as.Date("10/27/2016", "%m/%d/%Y"),]

right <- 0
wrong <- 0
sum <- 0

for(i in 1:nrow(box.data))
{
  game <- box.data[i,]
  eloA <- polls10[polls10$Team ==  as.character(game$Home.Team),]$Score
  eloB <-  polls10[polls10$Team == as.character(game$Visitor.Team),]$Score
  winA <- (if(game$Home.Score > game$Visitor.Score) 1 else 0)
  
    
  if((eloA > eloB && game$Home.Score > game$Visitor.Score) ||
       (eloA < eloB && game$Home.Score < game$Visitor.Score))
  {
    right <- right + 1
  }
  else
  {
    wrong <- wrong + 1
  }
  sum <- sum + ((winA*log(predict(eloA, eloB)))+((1-winA)*log(predict(eloB, eloA))))
  
}

rightPast <- right
wrongPast <- wrong
percentPast <- right/(right+wrong)
scorePast <- -1*sum/(right+wrong)

right <- 0
wrong <- 0
sum <- 0

for(i in 1:nrow(box.data.future))
{
  game <- box.data.future[i,]
  eloA <- polls9[polls9$Team ==  as.character(game$Home.Team),]$Score
  eloB <-  polls9[polls9$Team == as.character(game$Visitor.Team),]$Score
  winA <- (if(game$Home.Score > game$Visitor.Score) 1 else 0)
  
  
  if((eloA > eloB && game$Home.Score > game$Visitor.Score) ||
       (eloA < eloB && game$Home.Score < game$Visitor.Score))
  {
    right <- right + 1
  }
  else
  {
    wrong <- wrong + 1
  }
  sum <- sum + ((winA*log(predict(eloA, eloB)))+((1-winA)*log(predict(eloB, eloA))))
}  

percent <- right/(right+wrong)
score <- -1*sum/(right+wrong)