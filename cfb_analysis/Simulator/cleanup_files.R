trim <- function (x) gsub("^\\s+|\\s+$", "", x)

perplay <- read.csv(file = 'data\\2016\\perplaydata.csv', sep=',', header=T)

perplay$Visitor.Team <- trim(perplay$Visitor.Team)
perplay$Home.Team <- trim(perplay$Home.Team)


gp <- read.csv(file = 'data\\2016\\gamesPlayed.csv', sep=',', header=T)

gp$Visitor.Team <- trim(gp$Visitor.Team)
gp$Home.Team <- trim(gp$Home.Team)

perplay[perplay$Visitor.Team == "Cent Michigan",]$Visitor.Team <- "Central Michigan"
perplay[perplay$Home.Team == "Cent Michigan",]$Home.Team <- "Central Michigan"
gp[gp$Home.Team == "Cent Michigan",]$Home.Team <- "Central Michigan"
gp[gp$Visitor.Team == "Cent Michigan",]$Visitor.Team <- "Central Michigan"

perplay[perplay$Visitor.Team == "OSU",]$Visitor.Team <- "Ohio State"
perplay[perplay$Home.Team == "OSU",]$Home.Team <- "Ohio State"
gp[gp$Home.Team == "OSU",]$Home.Team <- "Ohio State"
gp[gp$Visitor.Team == "OSU",]$Visitor.Team <- "Ohio State"

perplay[perplay$Visitor.Team == "UConn",]$Visitor.Team <- "Connecticut"
perplay[perplay$Home.Team == "UConn",]$Home.Team <- "Connecticut"
gp[gp$Home.Team == "UConn",]$Home.Team <- "Connecticut"
gp[gp$Visitor.Team == "UConn",]$Visitor.Team <- "Connecticut"


perplay[perplay$Visitor.Team == "W Michigan",]$Visitor.Team <- "Western Michigan"
perplay[perplay$Home.Team == "W Michigan",]$Home.Team <- "Western Michigan"
gp[gp$Home.Team == "W Michigan",]$Home.Team <- "Western Michigan"
gp[gp$Visitor.Team == "W Michigan",]$Visitor.Team <- "Western Michigan"


perplay[perplay$Visitor.Team == "USF",]$Visitor.Team <- "South Florida"
perplay[perplay$Home.Team == "USF",]$Home.Team <- "South Florida"
gp[gp$Home.Team == "USF",]$Home.Team <- "South Florida"
gp[gp$Visitor.Team == "USF",]$Visitor.Team <- "South Florida"


perplay[perplay$Visitor.Team == "Cal",]$Visitor.Team <- "California"
perplay[perplay$Home.Team == "Cal",]$Home.Team <- "California"
gp[gp$Home.Team == "Cal",]$Home.Team <- "California"
gp[gp$Visitor.Team == "Cal",]$Visitor.Team <- "California"


perplay[perplay$Visitor.Team == "Appalachian St",]$Visitor.Team <- "Appalachian State"
perplay[perplay$Home.Team == "Appalachian St",]$Home.Team <- "Appalachian State"
gp[gp$Home.Team == "Appalachian St",]$Home.Team <- "Appalachian State"
gp[gp$Visitor.Team == "Appalachian St",]$Visitor.Team <- "Appalachian State"


perplay[perplay$Visitor.Team == "Mid Tennessee",]$Visitor.Team <- "Middle Tennessee"
perplay[perplay$Home.Team == "Mid Tennessee",]$Home.Team <- "Middle Tennessee"
gp[gp$Home.Team == "Mid Tennessee",]$Home.Team <- "Middle Tennessee"
gp[gp$Visitor.Team == "Mid Tennessee",]$Visitor.Team <- "Middle Tennessee"


perplay[perplay$Visitor.Team == "N Illinois",]$Visitor.Team <- "Northern Illinois"
perplay[perplay$Home.Team == "N Illinois",]$Home.Team <- "Northern Illinois"
gp[gp$Home.Team == "N Illinois",]$Home.Team <- "Northern Illinois"
gp[gp$Visitor.Team == "N Illinois",]$Visitor.Team <- "Northern Illinois"


perplay[perplay$Visitor.Team == "E Michigan",]$Visitor.Team <- "Eastern Michigan"
perplay[perplay$Home.Team == "E Michigan",]$Home.Team <- "Eastern Michigan"
gp[gp$Home.Team == "E Michigan",]$Home.Team <- "Eastern Michigan"
gp[gp$Visitor.Team == "E Michigan",]$Visitor.Team <- "Eastern Michigan"


perplay[perplay$Visitor.Team == "Washington St",]$Visitor.Team <- "Washington State"
perplay[perplay$Home.Team == "Washington St",]$Home.Team <- "Washington State"
gp[gp$Home.Team == "Washington St",]$Home.Team <- "Washington State"
gp[gp$Visitor.Team == "Washington St",]$Visitor.Team <- "Washington State"


perplay[perplay$Visitor.Team == "LA Tech",]$Visitor.Team <- "Louisiana Tech"
perplay[perplay$Home.Team == "LA Tech",]$Home.Team <- "Louisiana Tech"
gp[gp$Home.Team == "LA Tech",]$Home.Team <- "Louisiana Tech"
gp[gp$Visitor.Team == "LA Tech",]$Visitor.Team <- "Louisiana Tech"


perplay[perplay$Visitor.Team == "ECU",]$Visitor.Team <- "East Carolina"
perplay[perplay$Home.Team == "ECU",]$Home.Team <- "East Carolina"
gp[gp$Home.Team == "ECU",]$Home.Team <- "East Carolina"
gp[gp$Visitor.Team == "ECU",]$Visitor.Team <- "East Carolina"


perplay[perplay$Visitor.Team == "North Carolina St",]$Visitor.Team <- "North Carolina State"
perplay[perplay$Home.Team == "North Carolina St",]$Home.Team <- "North Carolina State"
gp[gp$Home.Team == "North Carolina St",]$Home.Team <- "North Carolina State"
gp[gp$Visitor.Team == "North Carolina St",]$Visitor.Team <- "North Carolina State"


perplay[perplay$Visitor.Team == "Pitt",]$Visitor.Team <- "Pittsburgh"
perplay[perplay$Home.Team == "Pitt",]$Home.Team <- "Pittsburgh"
gp[gp$Home.Team == "Pitt",]$Home.Team <- "Pittsburgh"
gp[gp$Visitor.Team == "Pitt",]$Visitor.Team <- "Pittsburgh"


perplay[perplay$Visitor.Team == "FSU",]$Visitor.Team <- "Florida State"
perplay[perplay$Home.Team == "FSU",]$Home.Team <- "Florida State"
gp[gp$Home.Team == "FSU",]$Home.Team <- "Florida State"
gp[gp$Visitor.Team == "FSU",]$Visitor.Team <- "Florida State"


perplay[perplay$Visitor.Team == "VT",]$Visitor.Team <- "Virginia Tech"
perplay[perplay$Home.Team == "VT",]$Home.Team <- "Virginia Tech"
gp[gp$Home.Team == "VT",]$Home.Team <- "Virginia Tech"
gp[gp$Visitor.Team == "VT",]$Visitor.Team <- "Virginia Tech"


perplay[perplay$Visitor.Team == "W Kentucky",]$Visitor.Team <- "Western Kentucky"
perplay[perplay$Home.Team == "W Kentucky",]$Home.Team <- "Western Kentucky"
gp[gp$Home.Team == "W Kentucky",]$Home.Team <- "Western Kentucky"
gp[gp$Visitor.Team == "W Kentucky",]$Visitor.Team <- "Western Kentucky"


perplay[perplay$Visitor.Team == "FAU",]$Visitor.Team <- "Florida Atlantic"
perplay[perplay$Home.Team == "FAU",]$Home.Team <- "Florida Atlantic"
gp[gp$Home.Team == "FAU",]$Home.Team <- "Florida Atlantic"
gp[gp$Visitor.Team == "FAU",]$Visitor.Team <- "Florida Atlantic"


perplay[perplay$Visitor.Team == "FIU",]$Visitor.Team <- "Florida International"
perplay[perplay$Home.Team == "FIU",]$Home.Team <- "Florida International"
gp[gp$Home.Team == "FIU",]$Home.Team <- "Florida International"
gp[gp$Visitor.Team == "FIU",]$Visitor.Team <- "Florida International"


perplay[perplay$Visitor.Team == "UNC",]$Visitor.Team <- "North Carolina"
perplay[perplay$Home.Team == "UNC",]$Home.Team <- "North Carolina"
gp[gp$Home.Team == "UNC",]$Home.Team <- "North Carolina"
gp[gp$Visitor.Team == "UNC",]$Visitor.Team <- "North Carolina"


perplay[perplay$Visitor.Team == "UL Monroe",]$Visitor.Team <- "Louisiana-Monroe"
perplay[perplay$Home.Team == "UL Monroe",]$Home.Team <- "Louisiana-Monroe"
gp[gp$Home.Team == "UL Monroe",]$Home.Team <- "Louisiana-Monroe"
gp[gp$Visitor.Team == "UL Monroe",]$Visitor.Team <- "Louisiana-Monroe"


perplay[perplay$Visitor.Team == "LA-Lafayette",]$Visitor.Team <- "Louisiana-Lafayette"
perplay[perplay$Home.Team == "LA-Lafayette",]$Home.Team <- "Louisiana-Lafayette"
gp[gp$Home.Team == "LA-Lafayette",]$Home.Team <- "Louisiana-Lafayette"
gp[gp$Visitor.Team == "LA-Lafayette",]$Visitor.Team <- "Louisiana-Lafayette"


perplay[perplay$Visitor.Team == "Ga Southern",]$Visitor.Team <- "Georgia Southern"
perplay[perplay$Home.Team == "Ga Southern",]$Home.Team <- "Georgia Southern"
gp[gp$Home.Team == "Ga Southern",]$Home.Team <- "Georgia Southern"
gp[gp$Visitor.Team == "Ga Southern",]$Visitor.Team <- "Georgia Southern"


perplay[perplay$Visitor.Team == "UVA",]$Visitor.Team <- "Virginia"
perplay[perplay$Home.Team == "UVA",]$Home.Team <- "Virginia"
gp[gp$Home.Team == "UVA",]$Home.Team <- "Virginia"
gp[gp$Visitor.Team == "UVA",]$Visitor.Team <- "Virginia"


perplay[perplay$Visitor.Team == "New Mexico St",]$Visitor.Team <- "New Mexico State"
perplay[perplay$Home.Team == "New Mexico St",]$Home.Team <- "New Mexico State"
gp[gp$Home.Team == "New Mexico St",]$Home.Team <- "New Mexico State"
gp[gp$Visitor.Team == "New Mexico St",]$Visitor.Team <- "New Mexico State"


perplay[perplay$Visitor.Team == "Miss St",]$Visitor.Team <- "Mississippi State"
perplay[perplay$Home.Team == "Miss St",]$Home.Team <- "Mississippi State"
gp[gp$Home.Team == "Miss St",]$Home.Team <- "Mississippi State"
gp[gp$Visitor.Team == "Miss St",]$Visitor.Team <- "Mississippi State"


perplay[perplay$Visitor.Team == "UMass",]$Visitor.Team <- "Massachusetts"
perplay[perplay$Home.Team == "UMass",]$Home.Team <- "Massachusetts"
gp[gp$Home.Team == "UMass",]$Home.Team <- "Massachusetts"
gp[gp$Visitor.Team == "UMass",]$Visitor.Team <- "Massachusetts"

perplay$Visitor.Team <- trim(perplay$Visitor.Team)
perplay$Home.Team <- trim(perplay$Home.Team)
gp$Visitor.Team <- trim(gp$Visitor.Team)
gp$Home.Team <- trim(gp$Home.Team)

write.table(perplay, 'data\\2016\\perplaydata.csv', row.names=FALSE, sep=",") 
write.table(gp, 'data\\2016\\gamesPlayed.csv', row.names=FALSE, sep=",") 


rec <- read.csv(file = 'data\\2016\\records.csv', sep=',', header=T)
conf <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)

conf[!conf$TEAM %in% rec$TEAM,]$TEAM