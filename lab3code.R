library(fitdistrplus)
library(ADGofTest)

ad <- function(data, dist) {
  left <- c()
  right <- c()
  
  for (i in 1:length(data$Time)) {
    if (data$Censor[i] == 1) {
      left <- c(left, data$Time[i])
      right <- c(right, data$Time[i])
    } 
    else {
      left <- c(left, data$Time[i])
      right <- c(right, NA)
    }
  }
  
  d <- data.frame(left, right)
  fit <- fitdistcens(d, dist)
  rate <- fit$estimate[["rate"]]
  start <- floor(qexp(.01, rate))
  end <- ceiling(qexp(.99, rate))
  ad.test(data$Time, pexp, rate) #is ad test same for censored and complete?
  
}
data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")
data$Time <- data$T
data$Censor <- data$C
ad(data, "exp") #not same as minitab


