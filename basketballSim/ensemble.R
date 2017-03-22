kenpom <- read.csv(file = 'data\\competition\\kenpom.csv', sep=',', header=T)

colnames(kenpom)[2] <- "kenPred"

myPredOriginal <-  read.csv(file = 'data\\competition\\output\\alt_official_submission.csv', sep=',', header=T)

colnames(myPredOriginal)[2] <- "myOriginalPred"

myPredAlt <-  read.csv(file = 'data\\competition\\output\\first_official_submission.csv', sep=',', header=T)

colnames(myPredAlt)[2] <- "myAltPred"

netProphet <-  read.csv(file = 'data\\competition\\kaggle_submission_public.csv', sep=',', header=T)

colnames(netProphet)[2] <- "netPred"


thor <-  read.csv(file = 'data\\competition\\kaggle_submission_public.csv', sep=',', header=T)

colnames(thor)[2] <- "thorPred"

combo <- merge(kenpom, myPredOriginal, by="id")

combo <- merge(combo, myPredAlt, by="id")

combo <- merge(combo, netProphet, by="id")

combo <- merge(combo, thor, by="id")


combo$pred <- (combo$kenPred+combo$myOriginalPred+combo$myAltPred+combo$netPred+combo$thorPred)/5

results <- data.frame(id=combo$id, pred =combo$pred)

fname <- file.choose()
write.table(results, fname, row.names=FALSE, sep=",") 
# 
# myPredOriginal$myOriginalPred <- pmin(.95, pmax(.05, myPredOriginal$myOriginalPred))
# 
# myPredAlt$myAltPred <- pmin(.95, pmax(.05, myPredAlt$myAltPred))
# 
# kenOrig <- merge(kenpom, myPredOriginal, by="id")
# kenOrig$id <- NULL
# plot(kenOrig)
# line <- lm(myOriginalPred~kenPred,data=kenOrig)
# abline(line,col="green")
# summary(line, data=kenOrig)
# 
# 
# kenAlt<- merge(kenpom, myPredAlt, by="id")
# kenAlt$id <- NULL
# plot(kenAlt)
# line2 <- lm(myAltPred~kenPred,data=kenAlt)
# abline(line2,col="green")
# summary(line2, data=kenAlt)
# 
# origAlt<- merge(myPredOriginal, myPredAlt, by="id")
# origAlt$id <- NULL
# plot(kenAlt)
# line3 <- lm(myAltPred~myOriginalPred,data=origAlt)
# abline(line3,col="green")
# summary(line3, data=origAlt)
# 
# thorAlt<- merge(thor, myPredAlt, by="id")
# thorAlt$id <- NULL
# plot(thorAlt)
# line4 <- lm(myAltPred~thorPred,data=thorAlt)
# abline(line4,col="green")
# summary(line4, data=thorAlt)
