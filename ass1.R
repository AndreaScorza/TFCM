require(ggplot2)
dataset <- get(load("/Users/andrea/Desktop/TFCM/key.Rdata"))

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

steer_pp_mean <- aggregate(abs(dualSteerFocus$lanePosition)
                                , list(participant = dualSteerFocus$pp, keypress = dualSteerFocus$phoneNrLengthAfterKeyPress)
                                , mean)
dial_pp_mean <- aggregate(abs(dualDialFocus$lanePosition)
                          , list(participant = dualDialFocus$pp, keypress = dualDialFocus$phoneNrLengthAfterKeyPress)
                          , mean)

avg_pp_steer <- aggregate(abs(steer_pp_mean$x), list(keypress = steer_pp_mean$keypress), mean)
avg_pp_dial <- aggregate(abs(dial_pp_mean$x), list(keypress = dial_pp_mean$keypress), mean)


result <- data.frame(avg_pp_dial$keypress, avg_pp_dial$x
                          , avg_pp_steer$keypress, avg_pp_steer$x)

#can use geom_point instead of geom_line
gplot <- gplot + geom_line(aes(x = result$avg_pp_dial.keypress
                                , y = result$avg_pp_dial.x
                                , colour = "Dialing Focus"), shape = 15)
gplot <- gplot + geom_line(aes(x = result$avg_pp_steer.keypress
                                , y = result$avg_pp_steer.x
                                , colour = "Steering Focus"), shape = 15)

gplot <- gplot + scale_colour_manual(limits=c("Dialing Focus", "Steering Focus"), 
                                      values=c("red", "blue"))

gplot <- gplot + labs(title="Dual Dial vs Dual Steer Focus Condition", x ="Key Presses", y = "Lane Deviation")

gplot$labels$colour <- "Experimental Conditions"
#plot(gplot)

#2222222222222222222222222222222222222222222222

drift <- get(load("/Users/andrea/Desktop/TFCM/Drift"))

drift_short <- table_drift[with(table_drift, trialTime >= 15000 & trialTime <= 18000), ]
colors <- sample(colors(), 20)
plot(NULL, xlim = c(15000, 18000), ylim = c(-1, 1.5) )

#prende le righe in base al trial e te le disegna

i <- 1;
while(i <= 20) { 
  with(drift_short[drift_short$trial == i,] , lines(trialTime, posX, col = colors[i]))
  posXs <- drift_short[with(drift_short, trial == i), ]$posX
  i <- i + 1
}

#2bbbbbbbbbbbbbbbbbbb------------------------------------













