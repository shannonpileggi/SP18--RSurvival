##Lab 3 Code

#MLE's Question 1
data <- rearrest

#Formatting data for fitdist function
left <- c()
right <- c()

for (i in 1:length(data$months)) {
  if (data$censor[i] == 1) {
    left <- c(left, data$months[i])
    right <- c(right, data$months[i])
  } 
  else {
    left <- c(left, data$months[i])
    right <- c(right, NA)
  }
}

d <- data.frame(left, right)

#Exponential distribution parameter estimates
check <- fitdistcens(d, "exp")
1/check$estimate

#Weibull distribution parameter estimates
check <- fitdistcens(d, "weibull")
check$estimate


#Lognormal distribution parameter estimates
check <- fitdistcens(d, "lnorm")
check$estimate

#Logistic distribution 
check <- fitdistcens(d, "logis")
check$estimate

#Loglogistic distribution
dllogis(x, shape = 1, scale = 1, log = FALSE) 

check <- fitdistcens(d, "llogis")
check$estimate


