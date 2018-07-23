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


#Weibull distribution parameter estimates
check <- fitdistcens(d, "weibull")
parameters <- check$estimate
parameters

library(car)
qqPlot(d, distribution="weibull", shape=parameters[1], scale=parameters[2])

#Lognormal distribution parameter estimates
check <- fitdistcens(d, "lnorm")
parameters <- check$estimate
parameters
#qqPlot(d, distribution="lognormal", shape=parameters[1], scale=parameters[2])

#Logistic distribution 
check <- fitdistcens(d, "logis")
parameters <- check$estimate
parameters
qqPlot(d, distribution="logis", location=parameters[1], scale=parameters[2])

#Loglogistic distribution
###Parameterization differs between R and Minitab:
# 1/Shape parameter in R = Scale parameter in Minitab
# log(scale parameter in R) = Shape parameter in Minitab
library(flexsurv)
check <- fitdistcens(d, "llogis")
parameters <- check$estimate
parameters



