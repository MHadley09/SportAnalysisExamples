trim <- function (x) gsub("^\\s+|\\s+$", "", x)

betting <- read.csv(file = 'data\\2016\\betting.csv', sep=',', header=T)

betting$TeamB <- trim(betting$TeamB)
betting$TeamA <- trim(betting$TeamA)







betting[betting$TeamB == "Cent Michigan",]$TeamB <- "Central Michigan"
betting[betting$TeamA == "Cent Michigan",]$TeamA <- "Central Michigan"



betting[betting$TeamB == "OSU",]$TeamB <- "Ohio State"
betting[betting$TeamA == "OSU",]$TeamA <- "Ohio State"



betting[betting$TeamB == "UConn",]$TeamB <- "Connecticut"
betting[betting$TeamA == "UConn",]$TeamA <- "Connecticut"




betting[betting$TeamB == "W Michigan",]$TeamB <- "Western Michigan"
betting[betting$TeamA == "W Michigan",]$TeamA <- "Western Michigan"




betting[betting$TeamB == "USF",]$TeamB <- "South Florida"
betting[betting$TeamA == "USF",]$TeamA <- "South Florida"




betting[betting$TeamB == "Cal",]$TeamB <- "California"
betting[betting$TeamA == "Cal",]$TeamA <- "California"




betting[betting$TeamB == "Appalachian St",]$TeamB <- "Appalachian State"
betting[betting$TeamA == "Appalachian St",]$TeamA <- "Appalachian State"




betting[betting$TeamB == "Mid Tennessee",]$TeamB <- "Middle Tennessee"
betting[betting$TeamA == "Mid Tennessee",]$TeamA <- "Middle Tennessee"




betting[betting$TeamB == "N Illinois",]$TeamB <- "Northern Illinois"
betting[betting$TeamA == "N Illinois",]$TeamA <- "Northern Illinois"




betting[betting$TeamB == "E Michigan",]$TeamB <- "Eastern Michigan"
betting[betting$TeamA == "E Michigan",]$TeamA <- "Eastern Michigan"




betting[betting$TeamB == "Washington St",]$TeamB <- "Washington State"
betting[betting$TeamA == "Washington St",]$TeamA <- "Washington State"




betting[betting$TeamB == "LA Tech",]$TeamB <- "Louisiana Tech"
betting[betting$TeamA == "LA Tech",]$TeamA <- "Louisiana Tech"




betting[betting$TeamB == "ECU",]$TeamB <- "East Carolina"
betting[betting$TeamA == "ECU",]$TeamA <- "East Carolina"




betting[betting$TeamB == "North Carolina St",]$TeamB <- "North Carolina State"
betting[betting$TeamA == "North Carolina St",]$TeamA <- "North Carolina State"




betting[betting$TeamB == "Pitt",]$TeamB <- "Pittsburgh"
betting[betting$TeamA == "Pitt",]$TeamA <- "Pittsburgh"




betting[betting$TeamB == "FSU",]$TeamB <- "Florida State"
betting[betting$TeamA == "FSU",]$TeamA <- "Florida State"




betting[betting$TeamB == "VT",]$TeamB <- "Virginia Tech"
betting[betting$TeamA == "VT",]$TeamA <- "Virginia Tech"




betting[betting$TeamB == "W Kentucky",]$TeamB <- "Western Kentucky"
betting[betting$TeamA == "W Kentucky",]$TeamA <- "Western Kentucky"




betting[betting$TeamB == "FAU",]$TeamB <- "Florida Atlantic"
betting[betting$TeamA == "FAU",]$TeamA <- "Florida Atlantic"




betting[betting$TeamB == "FIU",]$TeamB <- "Florida International"
betting[betting$TeamA == "FIU",]$TeamA <- "Florida International"




betting[betting$TeamB == "UNC",]$TeamB <- "North Carolina"
betting[betting$TeamA == "UNC",]$TeamA <- "North Carolina"




betting[betting$TeamB == "UL Monroe",]$TeamB <- "Louisiana-Monroe"
betting[betting$TeamA == "UL Monroe",]$TeamA <- "Louisiana-Monroe"




betting[betting$TeamB == "LA-Lafayette",]$TeamB <- "Louisiana-Lafayette"
betting[betting$TeamA == "LA-Lafayette",]$TeamA <- "Louisiana-Lafayette"




betting[betting$TeamB == "Ga Southern",]$TeamB <- "Georgia Southern"
betting[betting$TeamA == "Ga Southern",]$TeamA <- "Georgia Southern"




betting[betting$TeamB == "UVA",]$TeamB <- "Virginia"
betting[betting$TeamA == "UVA",]$TeamA <- "Virginia"




betting[betting$TeamB == "New Mexico St",]$TeamB <- "New Mexico State"
betting[betting$TeamA == "New Mexico St",]$TeamA <- "New Mexico State"




betting[betting$TeamB == "Miss St",]$TeamB <- "Mississippi State"
betting[betting$TeamA == "Miss St",]$TeamA <- "Mississippi State"




betting[betting$TeamB == "UMass",]$TeamB <- "Massachusetts"
betting[betting$TeamA == "UMass",]$TeamA <- "Massachusetts"



betting$TeamB <- trim(betting$TeamB)
betting$TeamA <- trim(betting$TeamA)



write.table(betting, 'data\\2016\\betting.csv', row.names=FALSE, sep=",") 


rec <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
conf <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

conf[!conf$TEAM %in% rec$TEAM,]$TEAM