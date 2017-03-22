source('Py_SoS_played.r')

strength <- FBS.SoS()
strength$Value <-  strength$ConferenceOffset*((2*strength$OVERALL.W)+strength$First.Pass.SoS)/3
strength$Rank <-   length(strength$TEAM) - rank(strength$Value) + 1

fname <- file.choose()
write.table(strength, fname, row.names=FALSE, sep=",") 