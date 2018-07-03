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

prob <- function(data, dist, num) {
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
  fit <- fitdistcens(d, dist)
  
  if (dist == "lnorm") {
    plnorm(num, fit$estimate[["meanlog"]], fit$estimate[["sdlog"]], lower.tail = F)
  }
  
  else if (dist == "exp") {
    pexp(num, fit$estimate[["rate"]], lower.tail = F)
  }
}

#part a
prob(data, "lnorm", 90)

#part b
prob(data, "exp", 90)

