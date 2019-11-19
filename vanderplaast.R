---
  title: 'Lab assignment 1: Processing models'
author: "Luka van der Plas (4119142) & Rianne Lam (6888216)"
date: "15 november 2019"
output: pdf_document
---
  

### Question 1A


load("keyPressDataWithLaneDeviation.Rdata")
no_errors <- subset(keyPressDataWithLaneDeviation, typingErrorMadeOnTrial == 0)
finished_dialling <- subset(no_errors, phoneNrLengthAfterKeyPress == 11)
mean_sd_se <- function(data){
  #summarise data into mean, SD and SE
  mean_value <- mean(data)
  sd <- sd(data)
  se <- sd / sqrt(length(data))
  
  
  
  resultVector <- c(mean_value, sd, se)
  print(resultVector)

}
summarise <- function(dialling_data) {
  #calculate mean per participant
  mean_dialling_times <- aggregate(dialling_data[, c('pp', 'timeRelativeToTrialStart')], by = list(dialling_data$pp), FUN = mean)$timeRelativeToTrialStart
  print(mean_dialling_times)
  #get summary over all participants
  summary <- mean_sd_se(mean_dialling_times)
  summary
}
#data per condition
steer_focus <-  summarise(subset(finished_dialling, partOfExperiment == 'dualSteerFocus'))
dial_focus <- summarise(subset(finished_dialling, partOfExperiment == 'dualDialFocus'))