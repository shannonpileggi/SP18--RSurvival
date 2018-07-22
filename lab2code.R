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
  
  if (by != "") {
    data[[by]] <- as.factor(data[[by]])
    b <- as.factor(as.vector(data[[by]]))
  } else b <- c()

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
  
  d <- data.frame(left, right)
  j <- 1
  fit <- list()
  if (length(levels(b)) > 1) {
    for (i in levels(b)) {
      #subsets dataframe
      d2 <- d[data[[by]] == i, ]
      d2[[by]] <- NULL
      #adds estimate to list
      fit[[j]] <- fitdistcens(d2, dist)
      j <- j + 1
    }
    fit
  } else {
    fitdistcens(d, dist)
  }
}

prob <- function(data, dist, num, lower.tail = F, time = "Time", censor = "Censor") {
  #"prob" is a placeholder name, should be better
  
  #finds probability of survival beyond time = num
  #data is a dataframe where for complete times censor = 1
  #dist is a string which corresponds to the name of the distribution
  #num is in int
  #time and censor are string names of the colums
  
  #fits data to distribution
  fit <- fit_data(data, dist, time, censor)
  
  #creates argument list
  l <- c(q = 90, fit$estimate, lower.tail = lower.tail)
  args <- split(unname(l),names(l))
  
  #finds distribution funciton
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  #prints probability
  if (lower.tail == F) {
    cat("P(T > ", num, ") = ", do.call(pfunc, args), sep = "")
  } else {
    cat("P(T < ", num, ") = ", do.call(pfunc, args), sep = "")
  }
}

surv_summary <- function(data, dist, time = "Time", censor = "Censor", by = "") {

  #prints a summary of data fitted to a distribution
  #data is a dataframe where for complete times censor = 1
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  fit <- fit_data(data, dist, time, censor, by)
  qfunc <- match.fun(paste("q", dist, sep = ""))
  
  if (nchar(by) > 0) { #if there's a grouping variable
    
    for (i in 1:length(fit)) {
      f <- fit[[i]]
      
      l <- c(p = .25, f$estimate)
      first <- split(unname(l),names(l))
      l <- c(p = .5, f$estimate)
      second <- split(unname(l),names(l))
      l <- c(p = .75, f$estimate)
      third <- split(unname(l),names(l))
  
      cat("\n\nFor level =", levels(as.factor(data[[by]]))[i], "\n")
      for (name in names(f$estimate)) {
        cat(name, f$estimate[name], sep = "\t\t")
        cat("\n")
      }
      cat("Log Liklihood", logLik(f), sep = "\t")
      cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
      cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
      cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
    }
  } else {
    
    qfunc <- match.fun(paste("q", dist, sep = ""))
    l <- c(p = .25, fit$estimate)
    first <- split(unname(l),names(l))
    l <- c(p = .5, fit$estimate)
    second <- split(unname(l),names(l))
    l <- c(p = .75, fit$estimate)
    third <- split(unname(l),names(l))
    
    for (name in names(fit$estimate)) {
      cat(name, fit$estimate[name], sep = "\t\t")
      cat("\n")
    }
    cat("Log Liklihood", logLik(fit), sep = "\t")
    cat("\nFirst Quantile", do.call(qfunc, first), sep = "\t")
    cat("\nMedian", do.call(qfunc, second), sep = "\t\t")
    cat("\nThird Quantile", do.call(qfunc, third), sep = "\t")
  }
}

plot_surv <- function(data, dist, time = "Time", censor = "Censor", by = "") { 
  #"plot_surv" is also a placeholder name, I'm not creative
  
  #plots survival curve for data given that it follows the distribution dist
  #if by is specified, plots multiple lines; else, plots only one line
  #data is a dataframe where for complete times censor = 1
  #dist is a string which corresponds to the name of the distribution
  #time, censor, and by are string names of the colums
  
  fit <- fit_data(data, dist, time, censor, by) 
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))

  
  #fits data to distribution by the "by" variable and puts estimates in vector "fit"
  if (nchar(by) > 0) { #if there's a grouping variable
    f <- fit[[1]]
    
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      f <- fit[[i]]
      #subsets dataframe
      args <- c(f$estimate)
      args <- split(unname(args), names(args))
      args$q <- start:end
      y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1), levels(as.factor(data[[by]]))[i]))
    }
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    p <- ggplot(df, aes(x = x, y = X1, group = X2, color = factor(X2))) + geom_line() +
      scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
      scale_y_continuous(name = "S(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "survival function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } else {
    
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    args <- c(fit$estimate)
    args <- split(unname(args), names(args))
    args$q <- start:end
    
    y <- NULL
    y <- rbind(y, cbind(matrix(1 - do.call(pfunc, args), ncol = 1)))
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
      scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
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
  
  fit <- fit_data(data, dist, time, censor, by) 
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))
  dfunc <- match.fun(paste("d", dist, sep = ""))

  
  #fits data to distribution by the "by" variable and puts estimates in vector "fit"
  if (nchar(by) > 0) { #if there's a grouping variable
    f <- fit[[1]]
    
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      f <- fit[[i]]
      #subsets dataframe
      args <- c(f$estimate)
      pargs <- split(unname(args), names(args))
      dargs <- split(unname(args), names(args))
      pargs$q <- start:end
      dargs$x <- start:end
      ###
      y <- rbind(y, cbind(matrix(do.call(dfunc, dargs) / (1 - do.call(pfunc, pargs)), ncol = 1), 
                          levels(as.factor(data[[by]]))[i]))
      ###
    }
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
    p <- ggplot(df, aes(x = x, y = X1, group = X2, color = factor(X2))) + geom_line() +
      scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
      scale_y_continuous(name = "h(t)", breaks = seq(0, 1, by = 0.2)) +
      ggtitle(paste(dist, "hazard function")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2))) + 
      scale_colour_discrete(name = "Group")
    plot(p)
  } else {
    
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    args <- c(fit$estimate)
    pargs <- split(unname(args), names(args))
    pargs$q <- start:end
    args <- c(fit$estimate)
    dargs <- split(unname(args), names(args))
    dargs$x <- start:end
    
    y <- NULL
    ###
    y <- rbind(y, cbind(matrix(do.call(dfunc, dargs) / (1 - do.call(pfunc, pargs)), ncol = 1))) 
    ###
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
    p <- ggplot(df, aes(x = x, y = y)) + geom_line() +
      scale_x_continuous(name = "T", breaks = seq(start, end, by = (end - start) / 5)) +
      scale_y_continuous(name = "h(t)", breaks = seq(0, 1, by = 0.2)) +
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
  
  fit <- fit_data(data, dist, time, censor, by) 
  pfunc <- match.fun(paste("p", dist, sep = ""))
  qfunc <- match.fun(paste("q", dist, sep = ""))

  
  #fits data to distribution by the "by" variable and puts estimates in vector "fit"
  if (nchar(by) > 0) { #if there's a grouping variable
    f <- fit[[1]]
    
    l <- c(p = .01, f$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, f$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    y <- NULL
    for (i in 1:length(fit)) {
      f <- fit[[i]]
      #subsets dataframe
      args <- c(f$estimate)
      pargs <- split(unname(args), names(args))
      pargs$q <- start:end
      ###
      y <- rbind(y, cbind(matrix(-log(1 - do.call(pfunc, pargs)), ncol = 1), 
                          levels(as.factor(data[[by]]))[i]))
      ###
    }
    df <- data.frame(y)
    df$x <- rep(start:end, length(fit))
    df$X1 <- as.numeric(as.character(df$X1))
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
  } else {
    
    l <- c(p = .01, fit$estimate)
    s <- split(unname(l), names(l))
    l <- c(p = .99, fit$estimate)
    e <- split(unname(l), names(l))

    start <- floor(do.call(qfunc, s))
    end <- ceiling(do.call(qfunc, e))
    
    args <- c(fit$estimate)
    pargs <- split(unname(args), names(args))
    pargs$q <- start:end
    args <- c(fit$estimate)
    dargs <- split(unname(args), names(args))
    dargs$x <- start:end
    
    y <- NULL
    ###
    y <- rbind(y, cbind(matrix(-log(1 - do.call(pfunc, pargs)), ncol = 1))) 
    ###
    df <- data.frame(y)
    df$x <- start:end
    df$y <- as.numeric(as.character(df$y))
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
data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")

prob(data, "lnorm", 90, time = "T", censor = "C")
plot_surv(data, "lnorm", time = "T", censor = "C")
plot_haz(data, "lnorm", time = "T", censor = "C")
plot_cumhaz(data, "lnorm", time = "T", censor = "C")
surv_summary(data, "lnorm", time = "T", censor = "C")

#part b
prob(data, "exp", 90, time = "T", censor = "C") #different than answer key, but checked with minitab (.82)
plot_surv(data, "exp", time = "T", censor = "C") 
plot_haz(data, "exp", time = "T", censor = "C")
plot_cumhaz(data, "exp", time = "T", censor = "C")
surv_summary(data, "exp", time = "T", censor = "C")

#Question 3 ----

#part a
fly <- read.csv("Data sets/Fruitfly.txt", sep = "\t")

plot_surv(fly, "exp", time = "Longevity", by = "Partners") 
plot_haz(fly, "exp", time = "Longevity", by = "Partners") 
plot_cumhaz(fly, "exp", time = "Longevity", by = "Partners") 

#part b
surv_summary(fly, "exp", time = "Longevity", by = "Partners")

