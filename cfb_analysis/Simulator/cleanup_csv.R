fname <- file.choose()

teams <- read.csv(file = fname, sep=',', header=T)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

teams$Team <- trim(teams$Team)


#teams <- as.data.frame(sapply(teams,gsub,pattern="\\.",replacement=""))

if(as.character.factor(teams$Team) != NULL){
  teams$Team  <- as.character.factor(teams$Team)
}
teams[teams$Team == "Cent Michigan",]$Team <- "Central Michigan"

teams[teams$Team == "OSU",]$Team <- "Ohio State"

teams[teams$Team == "UConn",]$Team <- "Connecticut"

teams[teams$Team == "W Michigan",]$Team <- "Western Michigan"


teams[teams$Team == "USF",]$Team <- "South Florida"


teams[teams$Team == "Cal",]$Team <- "California"


teams[teams$Team == "Appalachian St",]$Team <- "Appalachian State"


teams[teams$Team == "Mid Tennessee",]$Team <- "Middle Tennessee"


teams[teams$Team == "N Illinois",]$Team <- "Northern Illinois"

teams[teams$Team == "E Michigan",]$Team <- "Eastern Michigan"


teams[teams$Team == "Washington St",]$Team <- "Washington State"


teams[teams$Team == "LA Tech",]$Team <- "Louisiana Tech"

teams[teams$Team == "ECU",]$Team <- "East Carolina"


teams[teams$Team == "North Carolina St",]$Team <- "North Carolina State"


teams[teams$Team == "Pitt",]$Team <- "Pittsburgh"


teams[teams$Team == "FSU",]$Team <- "Florida State"

teams[teams$Team == "VT",]$Team <- "Virginia Tech"


teams[teams$Team == "W Kentucky",]$Team <- "Western Kentucky"


teams[teams$Team == "FAU",]$Team <- "Florida Atlantic"
teams[teams$Home.Team == "FAU",]$Home.Team <- "Florida Atlantic"


teams[teams$Team == "FIU",]$Team <- "Florida International"


teams[teams$Team == "UNC",]$Team <- "North Carolina"


teams[teams$Team == "UL Monroe",]$Team <- "Louisiana-Monroe"


teams[teams$Team == "LA-Lafayette",]$Team <- "Louisiana-Lafayette"


teams[teams$Team == "UL-Lafayette",]$Team <- "Louisiana-Lafayette"

teams[teams$Team == "UL Lafayette",]$Team <- "Louisiana-Lafayette"

teams[teams$Team == "Ga Southern",]$Team <- "Georgia Southern"

teams[teams$Team == "UVA",]$Team <- "Virginia"

teams[teams$Team == "New Mexico St",]$Team <- "New Mexico State"


teams[teams$Team == "Miss St",]$Team <- "Mississippi State"


teams[teams$Team == "UMass",]$Team <- "Massachusetts"


teams[teams$Team == "Florida Intl",]$Team <- "Florida International"


teams[teams$Team == "Southern Mississippi",]$Team <- "Southern Miss"

teams[teams$Team == "So Miss",]$Team <- "Southern Miss"

teams[teams$Team == "No Illinois",]$Team <- "Northern Illinois"

teams[teams$Team == "Texas A&amp;M",]$Team <- "Texas A&M"

teams[teams$Team == "C Michigan",]$Team <- "Central Michigan"

teams[teams$Team == "Mid Tenn St",]$Team <- "Middle Tennessee"


teams[teams$Team == "Miami OH",]$Team <- "Miami (OH)"

teams[teams$Team == "Miami FL",]$Team <- "Miami (FL)"

teams[teams$Team == "GA Southern",]$Team <- "Georgia Southern"

teams[teams$Team == "NC St",]$Team <- "North Carolina State"

teams[teams$Team == "NC State",]$Team <- "North Carolina State"

teams[teams$Team == "Mississippi",]$Team <- "Ole Miss"

teams[teams$Team == "Miami (Florida)",]$Team <- "Miami (FL)"

teams[teams$Team == "Miami (Ohio)",]$Team <- "Miami (OH)"

teams[teams$Team == "Hawai'i",]$Team <- "Hawaii"

teams[teams$Team == "UCONN",]$Team <- "Connecticut"

teams[teams$Team == "Southern Cal",]$Team <- "USC"

teams[teams$Team == "Brigham Young",]$Team <- "BYU"

teams[teams$Team == "Ohio U.",]$Team <- "Ohio"

teams[teams$Team == "San José State",]$Team <- "San Jose State"

teams[teams$Team == "Hawai`i",]$Team <- "Hawaii"

teams[teams$Team == "UNC-Charlotte",]$Team <- "Charlotte"

teams[teams$Team == "Middle Tennessee State",]$Team <- "Middle Tennessee"

teams[teams$Team == "Texas-San Antonio",]$Team <- "UTSA"

teams[teams$Team == "Texas State-San Marcos",]$Team <- "Texas State"

teams[teams$Team == "Florida Int'l",]$Team <- "Florida International"

teams[teams$Team == "Central Florida",]$Team <- "UCF"

teams[teams$Team == "Umass",]$Team <- "Massachusetts"

teams[teams$Team == "Louisiana State",]$Team <- "LSU"

teams[teams$Team == "Bowling Green State",]$Team <- "Bowling Green"

teams[teams$Team == "Texas-San Antonio",]$Team <- "UTSA"

teams[teams$Team == "Texas-El Paso",]$Team <- "UTEP"

teams[teams$Team == "Texas State-San Marcos",]$Team <- "Texas State"

teams[teams$Team == "Texas Christian",]$Team <- "TCU"

teams[teams$Team == "Southern California",]$Team <- "USC"

teams[teams$Team == "Southern Methodist",]$Team <- "SMU"

teams[teams$Team == "Nevada-Las Vegas",]$Team <- "UNLV"

teams[teams$Team == "Umass",]$Team <- "Massachusetts"

teams[teams$Team == "Miami (Fl)",]$Team <- "Miami (FL)"

teams[teams$Team == "Miami (Oh)",]$Team <- "Miami (OH)"

teams[teams$Team == "Miami(Fl)",]$Team <- "Miami (FL)"

teams[teams$Team == "Miami(Oh)",]$Team <- "Miami (OH)"

teams[teams$Team == "Miami Florida",]$Team <- "Miami (FL)"

teams[teams$Team == "Miami Ohio",]$Team <- "Miami (OH)"

teams[teams$Team == "AirForce",]$Team <- "Air Force"

teams[teams$Team == "Louisiana Lafayette",]$Team <- "Louisiana-Lafayette"

teams[teams$Team == "Louisiana Monroe",]$Team <- "Louisiana-Monroe"

teams[teams$Team == "North Carolina St.",]$Team <- "North Carolina State"


teams[teams$Team == "Miami-Ohio",]$Team <- "Miami (OH)"

teams[teams$Team == "Miami-Florida",]$Team <- "Miami (FL)"

teams[teams$Team == "Central florida",]$Team <- "UCF"

teams[teams$Team == "Louisana Tech",]$Team <- "Louisiana Tech"

teams[teams$Team == "Middle Tennessee State",]$Team <- "Middle Tennessee"

teams[teams$Team == "Middle Tennessee St",]$Team <- "Middle Tennessee"


teams[teams$Team == "Texas San Antonio",]$Team <- "UTSA"

teams[teams$Team == "Cincinatti",]$Team <- "Cincinnati"
teams[teams$Team == "UF",]$Team <- "Florida"
teams[teams$Team == "BC",]$Team <- "Boston College"
teams[teams$Team == "TAMU",]$Team <- "Texas A&M"
teams[teams$Team == "GT",]$Team <- "Georgia Tech"
teams[teams$Team == "Kan State",]$Team <- "Kansas State"
teams[teams$Team == "Ok State",]$Team <- "Oklahoma State"
teams[teams$Team == "TTU",]$Team <- "Texas Tech"
teams[teams$Team == "ISU",]$Team <- "Iowa State"
teams[teams$Team == "KU",]$Team <- "Kansas"
teams[teams$Team == "Mich State",]$Team <- "Michigan State"
teams[teams$Team == "NW",]$Team <- "Northwestern"
teams[teams$Team == "MTSU",]$Team <- "Middle Tennessee"
teams[teams$Team == "Miss State",]$Team <- "Mississippi State"
teams[teams$Team == "ODU",]$Team <- "Old Dominion"
teams[teams$Team == "WKU",]$Team <- "Western Kentucky"
teams[teams$Team == "La Tech",]$Team <- "Louisiana Tech" 
teams[teams$Team == "USM",]$Team <- "Southern Miss"
teams[teams$Team == "UMASS",]$Team <- "Massachusetts"
teams[teams$Team == "BGSU",]$Team <- "Bowling Green"
teams[teams$Team == "NIU",]$Team <- "Northern Illinois"
teams[teams$Team == "WMU",]$Team <- "Western Michigan"
teams[teams$Team == "CMU",]$Team <- "Central Michigan"
teams[teams$Team == "EMU",]$Team <- "Eastern Michigan"
teams[teams$Team == "SDSU",]$Team <- "San Diego State"
teams[teams$Team == "SJSU",]$Team <- "San Jose State"
teams[teams$Team == "Wash State",]$Team <- "Washington State"
teams[teams$Team == "UGA",]$Team <- "Georgia"
teams[teams$Team == "Mizzou",]$Team <- "Missouri"
teams[teams$Team == "UK",]$Team <- "Kentucky"
teams[teams$Team == "UT",]$Team <- "Tennessee"
teams[teams$Team == "Vandy",]$Team <- "Vanderbilt"
teams[teams$Team == "TAMU",]$Team <- "Texas A&M"
teams[teams$Team == "Ark State",]$Team <- "Arkansas State"
teams[teams$Team == "ULL",]$Team <- "Louisiana-Lafayette"
teams[teams$Team == "USA",]$Team <- "South Alabama"
teams[teams$Team == "ULM",]$Team <- "Louisiana-Monroe"
teams[teams$Team == "App State",]$Team <- "Appalachian State"
teams[teams$Team == "NMSU",]$Team <-"New Mexico State"
teams[teams$Team == "Ga State",]$Team <- "Georgia State"


teams[teams$Team == "Bama",]$Team <- "Alabama"
teams[teams$Team == "tOSU",]$Team <- "Ohio State"
teams[teams$Team == "ND",]$Team <- "Notre Dame"
teams[teams$Team == "Mich",]$Team <- "Michigan"
teams[teams$Team == "MSU",]$Team <- "Michigan State"
teams[teams$Team == "Tenn",]$Team <- "Tennessee"
teams[teams$Team == "OU",]$Team <- "Oklahoma"
teams[teams$Team == "Ok State",]$Team <- "Oklahoma State"
teams[teams$Team == "App State",]$Team <- "Appalachian State"
teams[teams$Team == "Wash",]$Team <- "Washington"
teams[teams$Team == "S Miss",]$Team <- "Southern Miss"
teams[teams$Team == "Miss",]$Team <- "Ole Miss"
teams[teams$Team == "WVU",]$Team <- "West Virginia"
teams[teams$Team == "ASU",]$Team <- "Arizona State"
teams[teams$Team == "PSU",]$Team <- "Penn State"
teams[teams$Team == "WSU",]$Team <-"Washington State"
teams[teams$Team == "GaSo",]$Team <- "Georgia Southern"
teams[teams$Team == "Cincy",]$Team <- "Cincinnati"
teams[teams$Team == "SCAR",]$Team <- "South Carolina"
teams[teams$Team == "UConn",]$Team <- "Connecticut"
teams[teams$Team == "MD",]$Team <- "Maryland"
teams[teams$Team == "Syr",]$Team <- "Syracuse"
teams[teams$Team == "Colo State",]$Team <- "Colorado State"
teams[teams$Team == "NU",]$Team <- "Northwestern"
teams[teams$Team == "WF",]$Team <- "Wake Forest"
teams[teams$Team == "KSU",]$Team <- "Kansas State"
teams[teams$Team == "UNM",]$Team <- "New Mexico"
teams[teams$Team == "Colo",]$Team <- "Colorado"
teams[teams$Team == "Mass",]$Team <- "Massachusetts"
teams[teams$Team == "GSU",]$Team <- "Georgia State"
teams[teams$Team == "UNT",]$Team <- "North Texas"
teams[teams$Team == "Wash State",]$Team <-"Washington State"



teams[teams$Team == "Ohio.State",]$Team <- "Ohio State"
teams[teams$Team == "Tcu",]$Team <- "TCU"
teams[teams$Team == "Lsu",]$Team <- "LSU"
teams[teams$Team == "Texas Am",]$Team <- "Texas A&M"
teams[teams$Team == "Byu",]$Team <- "BYU"
teams[teams$Team == "Ucla",]$Team <- "UCLA"
teams[teams$Team == "Miami Fl",]$Team <- "Miami (FL)"
teams[teams$Team == "Usc",]$Team <-"USC"
teams[teams$Team == "Miami Oh",]$Team <- "Miami (OH)"
teams[teams$Team == "Ull",]$Team <-  "Louisiana-Lafayette"
teams[teams$Team == "Ulm",]$Team <-  "Louisiana-Monroe"
teams[teams$Team == "Mtsu",]$Team <- "Middle Tennessee"
teams[teams$Team == "Utep",]$Team <- "UTEP"
teams[teams$Team == "Unlv",]$Team <- "UNLV"
teams[teams$Team == "Ut San Antonio",]$Team <- "UTSA"
teams[teams$Team == "Smu",]$Team <- "SMU"
teams[teams$Team == "Fl Atlantic",]$Team <- "Florida Atlantic"
teams[teams$Team == "Nc State",]$Team <- "North Carolina State"

teams[teams$Team == "NC St.",]$Team <- "North Carolina State"
teams[teams$Team == "North Carolina St.",]$Team <- "North Carolina State"

teams[teams$Team == "AppalachianSt",]$Team <- "Appalachian State"
teams[teams$Team == "ArizonaState",]$Team <- "Arizona State"
teams[teams$Team == "ArkansasState",]$Team <- "Arkansas State"
teams[teams$Team == "BallState",]$Team <- "Ball State"
teams[teams$Team == "BoiseState",]$Team <- "Boise State"
teams[teams$Team == "ColoradoState",]$Team <- "Colorado State"
teams[teams$Team == "FresnoState",]$Team <- "Fresno State"
teams[teams$Team == "GASouthern",]$Team <- "Georgia Southern"
teams[teams$Team == "GeorgiaState",]$Team <- "Georgia State"
teams[teams$Team == "IowaState",]$Team <- "Iowa State"
teams[teams$Team == "KansasState",]$Team <- "Kansas State"
teams[teams$Team == "KentState",]$Team <- "Kent State"
teams[teams$Team == "LATech",]$Team <- "Louisiana Tech"
teams[teams$Team == "Miami(FL)",]$Team <- "Miami (FL)"
teams[teams$Team == "Miami(OH)",]$Team <- "Miami (OH)"
teams[teams$Team == "MissState",]$Team <- "Mississippi State"
teams[teams$Team == "NCState",]$Team <- "North Carolina State"
teams[teams$Team == "NIllinois",]$Team <- "Northern Illinois"
teams[teams$Team == "OkState",]$Team <- "Oklahoma State"
teams[teams$Team == "NewMexico",]$Team <- "New Mexico"
teams[teams$Team == "OldDominion",]$Team <- "Old Dominion"
teams[teams$Team == "OleMiss",]$Team <- "Ole Miss"
teams[teams$Team == "OregonState",]$Team <- "Oregon State"
teams[teams$Team == "SouthAlabama",]$Team <- "South Alabama"
teams[teams$Team == "SouthCarolina",]$Team <- "South Carolina"
teams[teams$Team == "SouthernMiss",]$Team <- "Southern Miss"
teams[teams$Team == "TexasState",]$Team <- "Texas State"
teams[teams$Team == "UND",]$Team <- "Notre Dame"
teams[teams$Team == "UtahState",]$Team <- "Utah State"
teams[teams$Team == "WakeForest",]$Team <- "Wake Forest"
teams[teams$Team == "WKentucky",]$Team <- "Western Kentucky"


teams[teams$Team == "Ok St",]$Team <- "Oklahoma State"
teams[teams$Team == "Uconn",]$Team <- "Connecticut"
teams[teams$Team == "App St",]$Team <- "Appalachian State"
teams[teams$Team == "Colo St",]$Team <- "Colorado State"
teams[teams$Team == "Colo State",]$Team <- "Colorado State"

teams[teams$Team == "Northern Illinois NIU",]$Team <- "Northern Illinois"
teams[teams$Team == "Louisiana Lafayette-Lafayette",]$Team <- "Louisiana-Lafayette"
teams[teams$Team == "Texas A&M&M",]$Team <- "Texas A&M"
teams[teams$Team == "North Carolina State NC",]$Team <- "North Carolina State"
teams[teams$Team == "Miami (FL) (FL)",]$Team <- "Miami (FL)" 
teams[teams$Team == "Miami (OH) (OH)",]$Team <- "Miami (OH)"        
teams[teams$Team == "Florida International FIU",]$Team <- "Florida International"

teams[teams$Team == "Louisiana - Lafayette",]$Team <- "Louisiana-Lafayette"
teams[teams$Team == "Louisiana - Monroe",]$Team <- "Louisiana-Monroe"

teams[teams$Team == "Mississippi",]$Team <- "Ole Miss"
teams[teams$Team == "Western Mich",]$Team <- "Western Michigan"
teams[teams$Team == "Northern Ill",]$Team <- "Northern Illinois"
teams[teams$Team == "NC State",]$Team <- "North Carolina State"
teams[teams$Team == "NC St",]$Team <- "North Carolina State"
teams[teams$Team == "Miami (Fla)",]$Team <- "Miami (FL)" 
teams[teams$Team == "Miami (Ohio)",]$Team <- "Miami (OH)"        
teams[teams$Team == "Ga Southern",]$Team <- "Georgia Southern"       
teams[teams$Team == "Western Ky",]$Team <- "Western Kentucky"       
teams[teams$Team == "Middle Tenn",]$Team <- "Middle Tennessee"       
teams[teams$Team == "La-Lafayette",]$Team <- "Louisiana-Lafayette"      
teams[teams$Team == "FIU",]$Team <- "Florida International"       
teams[teams$Team == "FAU",]$Team <- "Florida Atlantic"       
teams[teams$Team == "La-Monroe",]$Team <- "Louisiana-Monroe"
teams[teams$Team == "Eastern Mich",]$Team <- "Eastern Michigan"
teams[teams$Team == "Ucf",]$Team <- "UCF"

teams[teams$Team == "Western Mich.",]$Team <- "Western Michigan"
teams[teams$Team == "Northern Ill.",]$Team <- "Northern Illinois"
teams[teams$Team == "Miami (Fla.)",]$Team <- "Miami (FL)" 
teams[teams$Team == "Ga. Southern",]$Team <- "Georgia Southern"       
teams[teams$Team == "Western Ky.",]$Team <- "Western Kentucky"       
teams[teams$Team == "Middle Tenn",]$Team <- "Middle Tennessee"       
teams[teams$Team == "La.-Lafayette",]$Team <- "Louisiana-Lafayette"    
teams[teams$Team == "La.-Monroe",]$Team <- "Louisiana-Monroe"
teams[teams$Team == "Eastern Mich.",]$Team <- "Eastern Michigan"
teams[teams$Team == "Cinncinnati",]$Team <- "Cincinnati"
teams[teams$Team == "Conneticut",]$Team <- "Connecticut"
teams[teams$Team == "Nevada Las Vegas",]$Team <- "UNLV"
teams[teams$Team == "V Tech",]$Team <- "Virginia Tech"
teams[teams$Team == "Oregon Stae",]$Team <- "Oregon State"
teams[teams$Team == "Texas El Paso",]$Team <- "UTEP"
teams[teams$Team == "Washington sTate",]$Team <- "Washington State"
teams[teams$Team == "Washington sTate",]$Team <- "Tennessee"
teams[teams$Team == "OK State",]$Team <- "Oklahoma State"


teams <- as.data.frame(sapply(teams,gsub,pattern="\\<St\\>",replacement="State"))
teams <- as.data.frame(sapply(teams,gsub,pattern="\\<State\\>",replacement="State"))

conf <- read.csv(file = 'data\\2016\\ConferenceWithFCS.csv', sep=',', header=T)
conf$TEAM <- trim(conf$TEAM)
conf <- conf[conf$Conference != "FCS",]

conf[!(conf$TEAM %in% as.character(teams$Team)) ,]$TEAM

if(length(conf[!(conf$TEAM %in% as.character(teams$Team)) ,]$TEAM) == 0)
{
  teams <- teams[teams$Team %in% as.character(conf$TEAM),]
}

teams[!(teams$Team %in% as.character(conf$TEAM)) ,]$Team

length(teams[!(teams$Team %in% as.character(conf$TEAM)) ,]$Team)

if(length(teams[!(teams$Team %in% as.character(conf$TEAM)) ,]$Team) == 0)
{
  write.table(teams, fname, row.names=FALSE, sep=",") 
}

fname
