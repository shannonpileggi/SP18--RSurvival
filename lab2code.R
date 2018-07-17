library(fitdistrplus)
library(ggplot2)

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

surv_summary <- function(data, dist, by = c()) {
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
  
  if (length(by) > 1) { #if there's a grouping variable
    by <- as.factor(by)
    d <- data.frame(left, right, by)
    
    fit <- c()
    for (i in levels(by)) {
      #subsets dataframe
      d2 <- d[d$by == i, ]
      d2$by <- NULL
      #adds estimate to vector
      fit <- fitdistcens(d2, dist)
      rate <- fit$estimate[["rate"]]
      
      cat("\n\nFor level =", i, "\n")
      cat("Log Liklihood", logLik(fit), sep = "\t")
      cat("\nMean", 1 / rate, sep = "\t\t")
      cat("\nFirst Quantile", qexp(.25, rate), sep = "\t")
      cat("\nMedian", qexp(.5, rate), sep = "\t\t")
      cat("\nThird Quantile", qexp(.75, rate), sep = "\t")
    }
  } else {
    fit <- fitdistcens(d, dist)
    if (dist == "exp") {
      rate <- fit$estimate[["rate"]]
      
      cat("Log Liklihood", logLik(fit), sep = "\t")
      cat("\nMean", 1 / rate, sep = "\t\t")
      cat("\nFirst Quantile", qexp(.25, rate), sep = "\t")
      cat("\nMedian", qexp(.5, rate), sep = "\t\t")
      cat("\nThird Quantile", qexp(.75, rate), sep = "\t")
    } else if (dist == "lnorm") {
      m <- fit$estimate[["meanlog"]]
      sd <- fit$estimate[["sdlog"]]
      
      cat("Log Liklihood", logLik(fit), sep = "\t")
      cat("\nMean", exp(m), sep = "\t\t")
      cat("\nFirst Quantile", qlnorm(.25, m, sd), sep = "\t")
      cat("\nMedian", qlnorm(.5, m, sd), sep = "\t\t")
      cat("\nThird Quantile", qlnorm(.75, m, sd), sep = "\t")
    }
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
    
    d2 <- d[d$by == levels(by)[1], ]
    d2$by <- NULL
    if (dist == "exp") {
      rate <- fitdistcens(d2, dist)$estimate["rate"]
      start <- floor(qexp(.01, rate))
      end <- ceiling(qexp(.99, rate))
    } else if (dist == "lnrom") {
      meanlog <- fitdistcens(d2, dist)$estimate["meanlog"]
      sdlog <- fitdistcens(d2, dist)$estimate["sdlog"]
      start <- floor(qlnrom(.01, meanlog, sdlog))
      end <- ceiling(qlnorm(.99, meanlog, sdlog))
    }
    
    y <- NULL
    for (i in levels(by)) {
      #subsets dataframe
      d2 <- d[d$by == i, ]
      d2$by <- NULL
      if (dist == "exp") {
        rate <- fitdistcens(d2, dist)$estimate["rate"]
        y <- rbind(y, cbind(matrix(1 - pexp(start:end, rate), ncol = 1), i))
      } else if (dist == "lnrom") {
        meanlog <- fitdistcens(d2, dist)$estimate["meanlog"]
        sdlog <- fitdistcens(d2, dist)$estimate["sdlog"]
      }
    }
    df <- data.frame(y)
    df$x <- rep(start:end, length(levels(by)))
    df$V1 <- as.numeric(as.character(df$V1))
    p <- ggplot(df, aes(x = x, y = V1, group = i, color = factor(i))) + geom_line()# +
      #scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
      #scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      #ggtitle("Exponential survival function") +
      #theme(axis.text.x = element_text(size = rel(1.5)),
      #      axis.text.y = element_text(size = rel(1.5)),
      #      axis.title.y = element_text(size = rel(1.5)),
      #      axis.title.x = element_text(size = rel(1.5)),
      #      plot.title = element_text(size = rel(2)))
    plot(p)
  } else {
    if (dist == "exp") {
      rate <- fitdistcens(d, dist)$estimate[["rate"]]
      
      start <- floor(qexp(.01, rate))
      end <- ceiling(qexp(.99, rate))
      
      p <- ggplot(data.frame(x = c(start, end)), aes(x)) +
        stat_function(fun = function(x) {1 - pexp(x, rate = rate)}) +
        scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
        scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
        ggtitle("Exponential survival function") +
        theme(axis.text.x = element_text(size = rel(1.5)),
              axis.text.y = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              axis.title.x = element_text(size = rel(1.5)),
              plot.title = element_text(size = rel(2)))
      plot(p)
    } else if (dist == "lnorm") {
      
      meanlog <- fitdistcens(d, dist)$estimate["meanlog"]
      sdlog <- fitdistcens(d, dist)$estimate["sdlog"]
      
      start <- floor(qlnorm(.01, meanlog, sdlog))
      end <- ceiling(qlnorm(.99, meanlog, sdlog))
      
      p <- ggplot(data.frame(x = c(start, end)), aes(x)) +
        stat_function(fun = function(x) {1 - plnorm(x, meanlog, sdlog)}) +
        scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
        scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
        ggtitle("Exponential survival function") +
        theme(axis.text.x = element_text(size = rel(1.5)),
              axis.text.y = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              axis.title.x = element_text(size = rel(1.5)),
              plot.title = element_text(size = rel(2)))
      plot(p)
      
    }
  }
}    

#Question 2 ----

#part a
data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")
data$Time <- data$T
data$Censor <- data$C

prob(data, "lnorm", 90)
plot_surv(data, "lnorm")
surv_summary(data, "lnorm")

#part b
prob(data, "exp", 90) #different than answer key, but double checked with minitab
plot_surv(data, "exp") 
surv_summary(data, "exp")

#Question 3 ----

#part a
fly <- read.csv("Data sets/Fruitfly.txt", sep = "\t")
fly$Time <- fly$Longevity
plot_surv(fly, "exp", fly$Partners) 

#part b

surv_summary(fly, "exp", fly$Partners)
