library(fitdistrplus)

#Question 1

#part a
pexp(60, 1 / 95, lower.tail = F) #1 / lambda

pexp(90, 1 / 95, lower.tail = F)

pexp(45, 1 / 95) - pexp(35, 1 / 95)

#part b
plnorm(60, 4.3, .2, lower.tail = F)

plnorm(35, 4.3, .2, lower.tail = F)

plnorm(45, 4.3, .2) - plnorm(35, 4.3, .2)

#part c
pweibull(80, 7.1, 77, lower.tail = T)

pweibull(70, 7.1, 77, lower.tail = F)

pweibull(70, 7.1, 77, lower.tail = T) - pweibull(50, 7.1, 77, lower.tail = T)

#Question 2

data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")

prob <- function(data, dist, num, lower.tail = F) { #"prob" is a placeholder name, should be better
  
  #finds probability of survival beyond time = num
  #data is a dataframe with columns T (time) and C (censor) where for complete times C = 1
  #dist is a string which corresponds to the name of the distribution (right now only "lnorm" and "exp")
  #num is in int
  
  #this whole section formats the data for the function fitdistcens
  ##
  left <- c()
  right <- c()
  
  for (i in 1:length(data$T)) {
    if (data$C[i] == 1) {
      left <- c(left, data$T[i])
      right <- c(right, data$T[i])
    } 
    else {
      left <- c(left, data$T[i])
      right <- c(right, NA)
    }
  }
  
  d <- data.frame(left, right)
  ##
  
  #fits data to distribution
  fit <- fitdistcens(d, dist)
  
  if (dist == "lnorm") {
    shape = fit$estimate[["meanlog"]]
    scale = fit$estimate[["sdlog"]]
    
    #prints probability of survival beyond time = num
    cat("P(T > ", num, ") = ", plnorm(num, shape, scale, lower.tail = lower.tail), sep = "")
  }
  
  else if (dist == "exp") {
    rate <- fit$estimate[["rate"]]
    
    #prints probability of survival beyond time = num
    cat("P(T > ", num, ") = ", pexp(num, rate, lower.tail = lower.tail), sep = "")
  }
}

plot_surv <- function(data, dist) { #"plot_surv" is also a placeholder name, I'm not creative
  
  #plots survival curve for data given that it follows the distribution dist
  #data is a dataframe with columns T (time) and C (censor) where for complete times C = 1
  #dist is a string which corresponds to the name of the distribution (right now only "lnorm" and "exp")
  
  #this whole section formats the data for the function fitdistcens
  ##
  left <- c()
  right <- c()
  
  for (i in 1:length(data$T)) {
    if (data$C[i] == 1) {
      left <- c(left, data$T[i])
      right <- c(right, data$T[i])
    } 
    else {
      left <- c(left, data$T[i])
      right <- c(right, NA)
    }
  }
  
  d <- data.frame(left, right)
  ##
  
  #fits data to distribution
  fit <- fitdistcens(d, dist)
  
  if (dist == "lnorm") {
    shape = fit$estimate[["meanlog"]]
    scale = fit$estimate[["sdlog"]]
    
    lnorm.plot <- 1 - plnorm(seq(0, 130), shape, scale) #is there a way to know the max of the distribution, 130 is hard coded
    
    #finds where the probability is 99%
    start <- which(lnorm.plot <= .99)[1] 
    #finds where the probability is 1%
    end <- tail(which(lnorm.plot >= .01), 1)
    
    plot(lnorm.plot, xlim = c(start, end), type = "l", xlab = "Time", ylab = "Percent",
         main = "Lognormal Survival Plot for Time")
  }
  
  else if (dist == "exp") {
    rate <- fit$estimate[["rate"]]
    
    exp.plot <- 1 - pexp(seq(0, 2200), rate)  #same problem as above, 2200 is hard coded
    
    #finds where the probability is 99%
    start <- which(exp.plot <= .99)[1]
    #finds where the probability is 1%
    end <- tail(which(exp.plot >= .01), 1)
    
    plot(exp.plot, xlim = c(start, end), type = "l", xlab = "Time", ylab = "Percent",
         main = "Exponential Survival Plot for Time")
  }
}

#part a
prob(data, "lnorm", 90)
plot_surv(data, "lnorm")

#part b
prob(data, "exp", 90) #different than answer key, but double checked with minitab
plot_surv(data, "exp")

#Question 3

