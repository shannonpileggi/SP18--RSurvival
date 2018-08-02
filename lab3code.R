library(fitdistrplus)
library(flexsurv)

ad <- function(data, dist, time, censor) {
  
  #calculates Anderson Darling test statistic
  #data is a dataframe
  #dist is a string name of a distribution
  #time and censor are string names of columns
  
  #fits data to distribution
  fit <- fit_data(data, dist, time, censor) 
  #orders data by time
  data <- data[order(data[[time]]),]
  #stores time and censor as vectors
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  #cdf function
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  #finds cdf of the data for the complete times
  x <- c()
  Percent <- c()
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      x <- c(x, time[i])
      Percent <- c(Percent, (i / length(time)))
    } 
  }
  
  ad <- 0
  #z (i - 1)
  lastz <- 0
  #Fn(z (i - 1))
  lastf <- 0
  
  #from i = 1 to n + 1
  for (j in 1:(length(x) + 1)) {
    
    if (j == length(x) + 1) {
      z <- 1 - (10 ^ -12)
    } else {
      args <- c(q = x[j], fit$estimate)
      args <- split(unname(args), names(args))
      
      z <- do.call(pfunc, args) #cdf of the distribution
    }
    
    f <- Percent[j] #cdf of the data
    
    a <- -1 * z - log(1 - z) + lastz + log(1 - lastz)
    b <- 2 * log(1 - z) * lastf - 2 * log(1 - lastz) * lastf
    
    if (j == 1) {
      c <- log(z) * lastf ^ 2 - log(1 - z) * lastf ^ 2 + log(1 - lastz) * lastf ^ 2
    } else {
      c <- log(z) * (lastf ^ 2) - log(1 - z) * (lastf ^ 2) - log(lastz) * (lastf ^ 2) + log(1 - lastz) * (lastf ^ 2)
    }
    
    ad <- ad + a + b + c
    
    lastz <- z
    lastf <- f
  }
  
  ad <- ad * length(x)
  print(ad)
}

qqplot <- function(data, dist, time, censor) {
  
  #makes a qqplot of the data with a fitted distribution line
  #data is a dataframe
  #dist is a string name of a distribution
  #time and censor are string names of columns
  
  fit <- fit_data(data, dist, time, censor) 
  data <- data[order(data[[time]]),]
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  Percent <- c()
  x <- c()
  cdf <- c()
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      x <- c(x, time[i])
      Percent <- c(Percent, ((i - 1) / length(time)) * 100)
      args <- c(q = time[i], fit$estimate)
      args <- split(unname(args), names(args))
      cdf <- c(cdf, do.call(pfunc, args) * 100)
    } 
  }
  
  df <- data.frame(x, Percent, cdf)
  p <- ggplot(df, aes(x = Percent, y = cdf)) + geom_point() +
      geom_line(aes(x = cdf, y = cdf)) +
      scale_x_continuous(name = "Sample") +
      scale_y_continuous(name = "Theoretical")  
      ggtitle(paste(dist, "probability plot")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
  plot(p)
}

#Question 1 ----

#part a
rearrest <- read.csv("Data sets/Rearrest.txt", sep = "\t")

#part i
qqplot(rearrest, "exp", "months", "censor")
surv_summary(rearrest, "exp", "months", "censor") #reports 1/lambda
ad(rearrest, "exp", "months", "censor")
#part ii
qqplot(rearrest, "llogis", "months", "censor")
surv_summary(rearrest, "llogis", "months", "censor") #see parameterization
ad(rearrest, "llogis", "months", "censor")
#part iii
qqplot(rearrest, "logis", "months", "censor")
surv_summary(rearrest, "logis", "months", "censor") 
ad(rearrest, "logis", "months", "censor")
#part iv
qqplot(rearrest, "lnorm", "months", "censor")
surv_summary(rearrest, "lnorm", "months", "censor") 
ad(rearrest, "lnorm", "months", "censor")
#part v
qqplot(rearrest, "weibull", "months", "censor")
surv_summary(rearrest, "weibull", "months", "censor") 
ad(rearrest, "weibull", "months", "censor")

#Question 2 ----

#part a
plot_surv(rearrest, "llogis", "months", "censor")
surv_summary(rearrest, "llogis", "months", "censor") 
#part b
fit <- fit_data(rearrest, "llogis", "months", "censor")
qllogis(.2, fit$estimate[["shape"]], fit$estimate[["scale"]])
#part c
prob(rearrest, "llogis", 11, time = "months", censor = "censor", lower.tail = T)
prob(rearrest, "llogis", 11, time = "months", censor = "censor")
#part d
plot_surv(rearrest, "llogis", "months", "censor", "personal")
#part e
surv_summary(rearrest, "llogis", "months", "censor", "personal")
#part f
plot_haz(rearrest, "llogis", "months", "censor", "personal")
