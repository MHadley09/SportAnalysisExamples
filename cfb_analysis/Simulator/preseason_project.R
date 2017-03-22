source('cross_year_max_likelihood_ranking.r')

teams <-  read.csv(file = 'data\\2016\\conferenceWithFCS.csv', sep=',', header=T)

teams <- teams[teams$Conference != "FCS",]
names(teams)[names(teams) == "TEAM"] <- "Team"

for(y in 2011:2015)
{
  year <- calculate.rankings(y);
  year <- year[year$Team %in% teams$Team,]
  year$Previous.Score <- NULL
  year$Rank <- NULL
  teams <- merge(teams, year, all=T)
  teams$Score <-  names(teams)[names(teams) == "Score"] <- paste0("Score",y)
  teams$Score <- NULL
}

fname <- file.choose()
print("Running rankings.  Please wait.")
write.table(teams, fname, row.names=FALSE, sep=",") 