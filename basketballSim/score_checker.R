source("bball_bell_predictor.R")

neutral.games <- data[grepl("N",data$Neutral),]

neutral.compared.diff <- data.frame(actual= neutral.games$Home.Score - neutral.games$Visitor.Score,
                            predicted = 1:nrow(neutral.games))

for(i in 1:nrow(neutral.games))
{
  game <- neutral.games[i,]
  result <- neutral.predict(game$Home.Team, game$Visitor.Team, 10000)
  neutral.compared.diff[i,]$predicted <- result[1,]$Score -  result[2,]$Score 
}

plot(neutral.compared.diff)
neutral.diff.line <- lm(predicted~actual, data=neutral.compared.diff)
abline(neutral.diff.line,col="green")
summary(neutral.diff.line,data=neutral.compared.diff)

abs_error <- sqrt(mean((neutral.compared.diff$actual - neutral.compared.diff$predicted)^2))

neutral.compared.sum <- data.frame(actual= neutral.games$Home.Score + neutral.games$Visitor.Score,
                                    predicted = 1:nrow(neutral.games))

for(i in 1:nrow(neutral.games))
{
  game <- neutral.games[i,]
  result <- neutral.predict(game$Home.Team, game$Visitor.Team, 10000)
  neutral.compared.sum[i,]$predicted <- result[1,]$Score +  result[2,]$Score 
}

plot(neutral.compared.sum)
neutral.sum.line <- lm(predicted~actual, data=neutral.compared.sum)
abline(neutral.sum.line,col="green")
summary(neutral.sum.line,data=neutral.compared.sum)

abs_error <- sqrt(mean((neutral.compared.sum$actual - neutral.compared.sum$predicted)^2))
sd(neutral.compared.sum$actual)
sd(neutral.compared.sum$predicted)
sd(neutral.compared.diff$actual)
sd(neutral.compared.diff$predicted)

sum(home.compared.diff$actual > 0 & home.compared.diff$predicted > 0)
sum(home.compared.diff$actual < 0 & home.compared.diff$predicted < 0)
sum(home.compared.diff$actual > 0 & home.compared.diff$predicted < 0)
sum(home.compared.diff$actual < 0 & home.compared.diff$predicted > 0)