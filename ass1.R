dataset <- get(load("/Users/andrea/Desktop/TFCM/keyPressDataWithLaneDeviation.Rdata"))
dataset <- subset(dataset, typingErrorMadeOnTrial == 0)

dualSteerFocus <- subset(dataset, partOfExperiment == "dualSteerFocus")
dualDialFocus <- subset(dataset, partOfExperiment == "dualDialFocus")


meanTime <- function(data){
  finished <- subset(data, phoneNrLengthAfterKeyPress == 11)
  times <- finished[,13]
  mean1 <- mean(times)
  standardDev <- sd(times)
  standaerErr <- standardDev / sqrt(length(times)) #divided by the number of participants or number of observations in the array ?
  resultVector <- c(mean1, standardDev, standaerErr)
  return(resultVector)
  
}

meanDualSteer <- meanTime(dualSteerFocus)
meanDualDial <- meanTime(dualDialFocus)

#print(meanDualDial)
#print(meanDualSteer)

#--------------------------BBBBBBBBBBBBBBBBBBBB----------------------------------------------
dataset <- na.omit(dataset)
latDevMean <- aggregate(dataset$lanePosition, list(dataset$partOfExperiment), mean, na.rm = TRUE)
latDevSd <- aggregate(dataset$lanePosition, list(dataset$partOfExperiment), sd, na.rm = TRUE)
#print(max(unique(dataset$pp)))
StandErr_Dial <- latDevSd[2] / sqrt(max(unique(dataset$pp)))
#-------------------------CCCCCCCCCCCCCCCCCCCCc----------------





