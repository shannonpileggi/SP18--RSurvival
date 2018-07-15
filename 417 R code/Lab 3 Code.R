#Lab 3 code

#MLE's Question 1

rearrest <- read.csv("Data sets/Rearrest.txt", sep = "\t")
data <- rearrest
names(data)[names(data) == 'censor'] <- 'Censor'
names(data)[names(data) == 'months'] <- 'Time'

#Formatting data for fitdist function
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


#Exponential distribution parameter estimates
check <- fitdistcens(d, "exp")
rate <- 1/check$estimate

library('fitdistrplus')
plot(fitdist(data$Time,"exp"))


#Weibull distribution parameter estimates
check <- fitdistcens(d, "weibull")
check$estimate

library(car)
###qPlot(d, distribution="weibull", shape=0.8279898, scale=26.8386386)

#Lognormal distribution parameter estimates
check <- fitdistcens(d, "lnorm")
check$estimate
##qqPlot(d, distribution="lognormal", shape=2.798188, scale=1.746138)

#Logistic distribution 
check <- fitdistcens(d, "logis")
check$estimate
##qqPlot(d, distribution="logis", location=18.791516, scale=9.784775)

#Loglogistic distribution
library(flexsurv)
check <- fitdistcens(d, "llogis")
check$estimate


