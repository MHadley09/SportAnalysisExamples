source('True_SoS_played.R')
source('py_record.R')
expected.wins <- All.SoS()
ranking <-  Py.Wins()

ranking <- merge(x= expected.wins, y=ranking, by.x = "Team", by.y = "TEAM")

ranking$Wins.Over <- (ranking$Py.Wins) - ranking$Expected.Wins

Conferences <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

FBS.Team <- Conferences[Conferences$Conference != "FCS",]


ranking <- ranking[ranking$Team %in% FBS.Team$TEAM ,]

ranking$Rank <- (1 + length(ranking$Wins.Over ) - rank(ranking$Wins.Over ))

fname <- file.choose()
print("Running rankings.  Please wait.")
write.table(ranking, fname, row.names=FALSE, sep=",") 