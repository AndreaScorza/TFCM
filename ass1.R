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
print(length(latDevSd))
print(latDevSd[2])
StandErr_Dial <- latDevSd[2] / sqrt(max(unique(dataset$pp)))
print(StandErr_Dial)
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

drift <- get(load("/Users/andrea/Desktop/TFCM/drift.Rdata"))

drift_short <- drift[with(drift, trialTime >= 15000 & trialTime <= 18000), ]
colors <- sample(colors(), 20)
plot(NULL, xlim = c(15000, 18000), ylim = c(-1, 1.5) , ylab = "Lateral Position", xlab = "trialTime", main = "Drift Over Time")

#prende le righe in base al trial e te le disegna

histvect1 <- c()
i <- 1;
while(i <= 20) { 
  with(drift_short[drift_short$trial == i,] , lines(trialTime, posX, col = colors[i]))
  posXg <- drift_short[with(drift_short, trial == i), ]$posX
  #print(i)
  histvect1 <- c(histvect1, posXg)
  i <- i + 1
  
}


#2bbbbbbbbbbbbbbbbbbb------------------------------------
#a	<- rnorm(10000,	0.0,	0.13)
#hist(a,	col="grey")
#summary(a)


plot(NULL, xlim = c(0, 3000), ylim = c(-2, 2), xlab="Trial time (ms)",
     ylab="Lateral Position (m)", main="Simulated Data (based on original model)")

histvect2 <- c()
var <- 0
while(var < 20){
  x <- seq(0, 3000, length=60)
  hx <- c()
  lat_dev <- 0
  for (value in x){
    lat_dev = rnorm(1,lat_dev,0.13)
    
    hx <- c(hx, lat_dev)
    #print(value)
    #print(hx)
    histvect2 <- c(histvect2, hx)
  }
  lines(x, hx, col = var)
  #hist(hx,	col=var)
  var <- var +1
}

#2cccccccccccccccccccccccccc-----------------------------
#we decided not to use ylim for the histvec2 otherwise the graph would be cut and not be indicative of the real frequency of the car position
#la frequenza è quante volte sono stati rilevati i valori nell'intorno es di 0, (3000 valori vicini a 0)
hist(main = "Human Trial", histvect1, xlab="Car Position", col = c("green", "red"), xlim = c(-2, 2), breaks = seq(-2, 2, by = 0.1))
hist(main = "Simulated Data", histvect2, xlab="Car Position", col = c("green", "blue"), xlim= c(-2, 3), breaks = seq(-2, 3, by = 0.1))

#2dddddddddddddddddddddddd----------------------------------

stand_dev_human = sd(histvect1)
stand_dev_simulated = sd(histvect2)
print(stand_dev_human)
print(stand_dev_simulated)

#2eeeeeeeeeeeeeeeeeeeeee--------------------------------

#par(mfrow = c(1,2))

plot(NULL, xlim = c(0, 3000), ylim = c(-2, 2), xlab="Trial time (ms)",
     ylab="Lateral Position (m)", main="Simulated Data (based on original model)")

# scelto 0.06 perchè lo span (over the car position axis) è nel human trial circa da -1 a 1 come con 0.06, 
# e con gli estremi circa intorno a -0.5 e +0.5 abbastanza bassini come in 0.06
histvect3 <- c()
var <- 0
while(var < 50){
  x <- seq(0, 3000, length=60)
  hx <- c()
  lat_dev <- 0
  for (value in x){
    lat_dev = rnorm(1,lat_dev,0.05)
    
    hx <- c(hx, lat_dev)
    histvect3 <- c(histvect3, hx)
  }
  
  #plot of how lane	position	changes	over	time	for	the	individual	simulated	trials
  lines(x, hx, col = var)
  var <- var +1
}

#plot of the resulting distribution
hist(main = "third graph", histvect3, xlab="Car Position", col = c("green", "red"), xlim = c(-2, 2), breaks = seq(-2, 2, by = 0.1))

stand_dev_histvect3 = sd(histvect3)

#33333333333333333333-------------------------------------


