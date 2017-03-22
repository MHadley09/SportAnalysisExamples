Conference.Records <- function(){
  
  records <- read.csv(file = 'data\\2015\\records.csv', sep=',', header=T)
schedules <- read.csv(file = 'data\\2015\\gamesPlayed.csv', sep=',', header=T)
conferences <-  read.csv(file = 'data\\2015\\conferenceWithFCS.csv', sep=',', header=T)

conferenceRecords <- merge(conferences, records, by="TEAM")

conferenceRecordTable <- aggregate(cbind(OVERALL.W, OVERALL.L, CONF.W,CONF.L) ~ Conference, data = conferenceRecords, sum)

percent.w <- (conferenceRecordTable$OVERALL.W-conferenceRecordTable$CONF.W)/
  ((conferenceRecordTable$OVERALL.W-conferenceRecordTable$CONF.W)
   +(conferenceRecordTable$OVERALL.L-conferenceRecordTable$CONF.L))

conferenceRecordTable <- cbind(conferenceRecordTable, percent.w)

conferenceRecordCalculationTable <- conferenceRecordTable[conferenceRecordTable$Conference != "FCS",]

maxPercent <- max(conferenceRecordCalculationTable$percent.w)
minPercent <- min(conferenceRecordCalculationTable$percent.w)

rangePercent <- (conferenceRecordCalculationTable$percent.w - minPercent) / (maxPercent - minPercent)

conferenceOffset <- 0.975 + 0.05*rangePercent

conferenceRecordCalculationTable <- cbind(conferenceRecordCalculationTable,conferenceOffset)

FCS.Row <- conferenceRecordTable[conferenceRecordTable$Conference == "FCS",]

FCS.Row$conferenceOffset <-  0.75


conferenceRecordTable <- rbind(conferenceRecordCalculationTable, FCS.Row[1,])

conferenceRecords$ConferenceOffset = conferenceRecordTable[match(conferenceRecords$Conference, conferenceRecordTable$Conference),"conferenceOffset"] 

return(conferenceRecords)}