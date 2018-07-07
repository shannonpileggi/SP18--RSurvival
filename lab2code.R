library(fitdistrplus)

#Question 1 ----

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

#Functions ----

prob <- function(data, dist, num, lower.tail = F) { #"prob" is a placeholder name, should be better
  
  #finds probability of survival beyond time = num
  #data is a dataframe with columns Time and Censor where for complete times C = 1
  #dist is a string which corresponds to the name of the distribution (right now only "lnorm" and "exp")
  #num is in int
  
  #this whole section formats the data for the function fitdistcens
  ####
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
  ####
  
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

plot_surv <- function(data, dist, by = c()) { #"plot_surv" is also a placeholder name, I'm not creative
  
  #plots survival curve for data given that it follows the distribution dist
  #data is a dataframe with columns Time and Censor where for complete times C = 1
  #dist is a string which corresponds to the name of the distribution (right now only "lnorm" and "exp")
  #by is a vector that contains the grouping variable
  #if by is specified, plots multiple lines; else, plots only one line
  
  #this whole section formats the data for the function fitdistcens
  ####
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
  ####
  
  #fits data to distribution by the "by" variable and puts estimates in vector "fit"
  if (length(by) > 1) { #if there's a grouping variable
    by <- as.factor(by)
    d <- data.frame(left, right, by)
    
    fit <- c()
    for (i in levels(by)) {
      #subsets dataframe
      d2 <- d[d$by == i, ]
      d2$by <- NULL
      #adds estimate to vector
      fit <- c(fit, fitdistcens(d2, dist)$estimate)
    }
  } else fit <- fitdistcens(d, dist)$estimate
  
  for(j in 1:length(levels(by))) {
    #for each group, plots line
    
    if (dist == "lnorm") {
      #if no grouping variable, there's just one estimate for each parameter
      #else, for loop runs multiple times
      if (length(levels(by)) == 0 | length(levels(by)) == 1) {
        shape = fit[["meanlog"]]
        scale = fit[["sdlog"]]
      } else {
        print(length(levels(by)))
        shape = fit[j][["meanlog"]]
        scale = fit[j][["sdlog"]]
      }
      
      #fits distribution to data
      lnorm.plot <- 1 - plnorm(seq(0, 130), shape, scale) 
      #***is there a way to know the max of the distribution, 130 is hard coded***
      
      #if first element in vector, uses plot() instead of lines() and sets xlim
      if (j == 1) {
        #finds where the probability is 99%
        start <- which(lnorm.plot <= .99)[1] 
        #finds where the probability is 1%
        end <- tail(which(lnorm.plot >= .01), 1)
        
        plot(lnorm.plot, xlim = c(start, end), type = "l", xlab = "Time", ylab = "Percent",
             main = "Lognormal Survival Plot for Time", col = j)
      }
      else {
        #only hits condition if there is a grouping variable and j >= the second group
        lines(lnorm.plot, type = "l", col = j, lty = j)
      }
    } else if (dist == "exp") {
      #if no grouping variable, there's just one estimate for each parameter
      #else, for loop runs multiple times
      if (length(levels(by)) == 0 | length(levels(by)) == 1) {
        rate <- fit[["rate"]]
      } else {
        rate <- fit[j][["rate"]]
      }
      
      #fits distribution to data
      exp.plot <- 1 - pexp(seq(0, 2200), rate)  
      #***same problem as above, 2200 is hard coded***
      
      #if first element in vector, uses plot() instead of lines() and sets xlim
      if (j == 1) {
        #finds where the probability is 99%
        start <- which(exp.plot <= .99)[1]
        #finds where the probability is 1%
        end <- tail(which(exp.plot >= .01), 1)
        
        plot(exp.plot, xlim = c(start, end), type = "l", xlab = "Time", ylab = "Percent",
             main = "Exponential Survival Plot for Time", col = j)
      }
      else {
        #only hits condition if there is a grouping variable and j >= the second group
        lines(exp.plot, type = "l", col = j, lty = j)
      }
    }
  }
  #adds legend if more than one group
  if (j > 1) {
    legend("topright", legend = levels(by),
           col = 1:length(levels(by)), lty = 1:length(levels(by)), cex = 0.8)
  }
}

#Question 2 ----

#part a
data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")
data$Time <- data$T
data$Censor <- data$C

prob(data, "lnorm", 90)
plot_surv(data, "lnorm")

#part b
prob(data, "exp", 90) #different than answer key, but double checked with minitab
plot_surv(data, "exp")

#Question 3 ----

#part a
fly <- read.csv("Data sets/Fruitfly.txt", sep = "\t")
fly$Time <- fly$Longevity

plot_surv(fly, "exp", fly$Partners)

