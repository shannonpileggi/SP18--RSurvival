library(fitdistrplus)
library(ggplot2)
library(flexsurv)

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

fit_data <- function(data, dist, time = "Time", censor = "Censor", by = "") {
  
  #fits data to distribution and returns a Fit object
  #if by (grouping variable) is specified, returns list of Fit objects by group
  #time, censor, and by are all string names of the colums
  #dist is the string name of the distribution that corresponds to the r function name
  #data is a dataframe
  #complete data corresponds to a censor value of 1
  
  left <- c()
  right <- c()
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  
  #if there's a grouping variable, stores it as a vector
  if (by != "") {
    data[[by]] <- as.factor(data[[by]])
    b <- as.factor(as.vector(data[[by]]))
  } 
  else {
    b <- c()
  }
  #formats data so it'll work with fitdistcens function
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      left <- c(left, time[i])
      right <- c(right, time[i])
    } 
    else {
      left <- c(left, time[i])
      right <- c(right, NA)
    }
  }
  
  d <- data.frame(left, right) #the correct format for fitdistcens
  j <- 1 #iterator for index of list
  fit <- list()
  
  #if there's a grouping variable, adds fit object for each group to list 
  if (length(levels(b)) > 1) {
    
    for (i in levels(b)) {
      #subsets dataframe
      d2 <- d[data[[by]] == i, ]
      d2[[by]] <- NULL
      #adds fit object to list
      fit[[j]] <- fitdistcens(d2, dist)
      j <- j + 1
    }
    
    #returns list
    fit
    
  }
  #if there's no grouping variable, returns fit object
  else {
    fitdistcens(d, dist)
  }
}

prob <- function(data, dist, num, lower.tail = F, time = "Time", censor = "Censor") {
  #"prob" is a placeholder name, should be better
  
  #finds probability of survival beyond time = num
  #data is a dataframe where complete times = 1 and incomplete = 0
  #dist is a string which corresponds to the name of the distribution
  #num is in int
  #time and censor are string names of the colums
  
  #fits data to distribution
  fit <- fit_data(data, dist, time, censor)
  
  #creates argument list
  l <- c(q = num, fit$estimate, lower.tail = lower.tail)
  args <- split(unname(l),names(l))
  
  #finds cdf funciton
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  #prints probability
  if (lower.tail == F) {
    cat("P(T > ", num, ") = ", do.call(pfunc, args), sep = "")
  } 
  else {
    cat("P(T < ", num, ") = ", do.call(pfunc, args), sep = "")
  }
}

surv_summary <- function(data, dist, time = "Time", censor = "Censor", by = "") {

  #prints a summary of data fitted to a distribution
  #summary includes parameter estimates, log likelihood, mean, and quantiles
  ### standard deviation needs to be added ###
  #data is a dataframe where complete times = 1 and incomplete = 0
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  #fits data
  fit <- fit_data(data, dist, time, censor, by)
  #finds quantile and cdf functions for distribution
  qfunc <- match.fun(paste("q", dist, sep = ""))
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  #creates survival function
  surv <- function(t) {
    args <- c(fit$estimate)
    args <- split(unname(args), names(args))
    args$q <- t
    1 - do.call(pfunc, args)
  }
  
  #if there's a grouping variable
  if (nchar(by) > 0) {
    
    for (i in 1:length(fit)) {
      #stored fit object for each group
      f <- fit[[i]]
      
      #creates a survival function for that group
      surv <- function(t) {
        args <- c(f$estimate)
        args <- split(unname(args), names(args))
        args$q <- t
        1 - do.call(pfunc, args)
      }
      
      #creats argument lists for first, second, and third quantiles
      l <- c(p = .25, f$estimate)
      first <- split(unname(l), names(l))
      l <- c(p = .5, f$estimate)
      second <- split(unname(l), names(l))
      l <- c(p = .75, f$estimate)
      third <- split(unname(l), names(l))
  
      #prints level, parameter estimates, and log liklihood
      cat("\n\nFor level =", levels(as.factor(data[[by]]))[i], "\n")
      for (name in names(f$estimate)) {
        if (nchar(name) > 7) {
          cat(name, f$estimate[name], sep = "\t")
        }
        else {
          cat(name, f$estimate[name], sep = "\t\t")
        }
        cat("\n")
      }
      cat("Log Liklihood", logLik(f), sep = "\t")
      
      #if loglogistic, checks if mean exists and prints it
      if (dist == "llogis" && 1 / f$estimate[["shape"]] > 1) {
        cat("\nMean", "N/A", sep = "\t\t")
      } 
      else {
        cat("\nMean", integrate(surv, 0, Inf)[["value"]], sep = "\t\t")
      }
      
      ###need to add standard deviation
      
      #prints quantiles
      cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
      cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
      cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
    }
  } 
  else { #if there's no grouping variable
    
    #argument list for quantiles
    l <- c(p = .25, fit$estimate)
    first <- split(unname(l), names(l))
    l <- c(p = .5, fit$estimate)
    second <- split(unname(l), names(l))
    l <- c(p = .75, fit$estimate)
    third <- split(unname(l), names(l))
    
    #prints parameter estimates and log likihood
    for (name in names(fit$estimate)) {
      if (nchar(name) > 7) {
        cat(name, fit$estimate[name], sep = "\t")
      }
      else {
        cat(name, fit$estimate[name], sep = "\t\t")
      }
      cat("\n")
    }
    cat("Log Liklihood", logLik(fit), sep = "\t")
    
    #if loglogistic, checks if mean exists and prints it
    if (dist == "llogis" && 1 / fit$estimate[["shape"]] > 1) {
      cat("\nMean", "N/A", sep = "\t\t")
    } 
    else {
      cat("\nMean", integrate(surv, 0, Inf)[["value"]], sep = "\t\t")
    }
    
    ###need to add standard deviation
    
    #prints quantiles
    cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
    cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
    cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
  }
}

plot_surv <- function(data, dist, time = "Time", censor = "Censor", by = "") { 
  #"plot_surv" is also a placeholder name, I'm not creative
  
  #plots survival curve for data given that it follows the distribution dist
  #if by is specified, plots multiple lines; else, plots only one line
  #data is a dataframe where complete times = 1 and imcomplete = 0
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  #fits data
  fit <- fit_data(data, dist, time, censor, by)
  #finds cdf and quantile functions for distribution
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))

  
  if (nchar(by) > 0) { #if there's a grouping variable
    
    #stores first fit 
    f <- fit[[1]]
    
    #argument list for first fit
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    #the start and end times for the first fit
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      #fit object for each group
      f <- fit[[i]]
      
      #argument list
      args <- c(f$estimate)
      args <- split(unname(args), names(args))
      args$q <- start:end
      
      #stores S(t) and group in a matrix
      y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1), levels(as.factor(data[[by]]))[i]))
    }
    
    #stores S(t), t, and group in a dataframe
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    
    #plots survival function
    p <- ggplot(df, aes(x = x, y = X1, group = X2, color = factor(X2))) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "survival function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } 
  else {
    #if there's no grouping variable
    
    #argument list
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    #start and end times
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    #argument list
    args <- c(fit$estimate)
    args <- split(unname(args), names(args))
    args$q <- start:end
    
    #stores S(t) in a matrix
    y <- NULL
    y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1)))
    
    #stores S(t) and t in a dataframe
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    
    #plots survival function
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "survival function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
    plot(p)
  }
}    

plot_haz <- function(data, dist, time = "Time", censor = "Censor", by = "") { 
  #"plot_haz" is also a placeholder name, I'm not creative
  
  #plots hazard curve for data given that it follows the distribution dist
  #if by is specified, plots multiple lines; else, plots only one line
  #data is a dataframe where for complete times censor = 1
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  #fits data
  fit <- fit_data(data, dist, time, censor, by) 
  #finds cdf, quantile, and pdf functions for distribution
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))
  dfunc <- match.fun(paste("d", dist, sep = ""))

  
  if (nchar(by) > 0) { #if there's a grouping variable
    
    #stores first fit
    f <- fit[[1]]
    
    #argument list for first fit
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    #the start and end times for the first fit
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      #fit object for each group
      f <- fit[[i]]
      #argument list
      args <- c(f$estimate)
      pargs <- split(unname(args), names(args))
      dargs <- split(unname(args), names(args))
      pargs$q <- start:end
      dargs$x <- start:end
      
      #stores h(t) and group in a matrix
      y <- rbind(y, cbind(matrix(do.call(dfunc, dargs) / (1 - do.call(pfunc, pargs)), ncol = 1), 
                          levels(as.factor(data[[by]]))[i]))
    }
    
    #stores h(t), t, and group in a matrix
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    
    #plots hazard curve
    p <- ggplot(df, aes(x = x, y = X1, group = X2, color = factor(X2))) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "h(t)") +
      ggtitle(paste(dist, "hazard function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } 
  else {
    #if there's no grouping variable
    
    #argument list
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    #start and end times
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    #argument list
    args <- c(fit$estimate)
    pargs <- split(unname(args), names(args))
    pargs$q <- start:end
    args <- c(fit$estimate)
    dargs <- split(unname(args), names(args))
    dargs$x <- start:end
    
    #stores h(t) in a matrix
    y <- NULL
    y <- rbind(y, cbind(matrix(do.call(dfunc, dargs) / (1 - do.call(pfunc, pargs)), ncol = 1))) 
    #stores h(t) and t in a dataframe
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    
    #plots hazard curve
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
      scale_x_continuous(name = "T") + #, breaks = seq(start, end, by = (end - start) / 5)) +
      scale_y_continuous(name = "h(t)") + #, breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "hazard function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
    plot(p)
  }
}    

plot_cumhaz <- function(data, dist, time = "Time", censor = "Censor", by = "") { 
  #"plot_cumhaz" is also a placeholder name, I'm not creative
  
  #plots cumulative hazard curve for data given that it follows the distribution dist
  #if by is specified, plots multiple lines; else, plots only one line
  #data is a dataframe where for complete times censor = 1
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  #fits data
  fit <- fit_data(data, dist, time, censor, by) 
  #finds cdf and quantile functions for distribution
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))

  
  if (nchar(by) > 0) { #if there's a grouping variable
    
    #stores first fit 
    f <- fit[[1]]
    
    #argument list for first fit
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    #the start and end times for the first fit
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      #fit object for each group
      f <- fit[[i]]
      #argument list
      args <- c(f$estimate)
      pargs <- split(unname(args), names(args))
      pargs$q <- start:end
      
      #stores H(t) and group in a matrix
      y <- rbind(y, cbind(matrix(-log(1 - do.call(pfunc, pargs)), ncol = 1), 
                          levels(as.factor(data[[by]]))[i]))
    }
    #stores H(t), t, and group in a dataframe
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    
    #plots curves
    p <- ggplot(df, aes(x = x, y = X1, group = X2, color = factor(X2))) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "H(t)") + 
      ggtitle(paste(dist, "cumulative hazard function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } 
  else {
    #if there's no grouping variable
    
    #argument list
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    #start and end times
    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    #argument list
    args <- c(fit$estimate)
    pargs <- split(unname(args), names(args))
    pargs$q <- start:end
    args <- c(fit$estimate)
    dargs <- split(unname(args), names(args))
    dargs$x <- start:end
    
    #stores H(t) in a matrix
    y <- NULL
    y <- rbind(y, cbind(matrix(-log(1 - do.call(pfunc, pargs)), ncol = 1))) 
    
    #stores H(t) and t in a dataframe
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    
    #plots H(t)
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
      scale_x_continuous(name = "T") +
      scale_y_continuous(name = "H(t)") +
      ggtitle(paste(dist, "cumulative hazard function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
    plot(p)
  }
}    

#Question 2 ----

#part a
melt <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")

prob(melt, "lnorm", 90, time = "T", censor = "C")
plot_surv(melt, "lnorm", time = "T", censor = "C")
plot_haz(melt, "lnorm", time = "T", censor = "C")
plot_cumhaz(melt, "lnorm", time = "T", censor = "C")
surv_summary(melt, "lnorm", time = "T", censor = "C")

#part b
prob(melt, "exp", 90, time = "T", censor = "C") #different than answer key, but checked with minitab (.82)
plot_surv(melt, "exp", time = "T", censor = "C") 
plot_haz(melt, "exp", time = "T", censor = "C")
plot_cumhaz(melt, "exp", time = "T", censor = "C")
surv_summary(melt, "exp", time = "T", censor = "C")

#Question 3 ----

#part a
fly <- read.csv("Data sets/Fruitfly.txt", sep = "\t")

plot_surv(fly, "exp", time = "Longevity", by = "Partners") 
plot_haz(fly, "exp", time = "Longevity", by = "Partners") 
plot_cumhaz(fly, "exp", time = "Longevity", by = "Partners") 

#part b
surv_summary(fly, "exp", time = "Longevity", by = "Partners")

