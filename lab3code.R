library(fitdistrplus)
library(ADGofTest)

ad <- function(data, dist, time, censor) {
  
  fit <- fit_data(data, dist, time, censor) 
  data <- data[order(data[[time]]),]
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  x <- c()
  Percent <- c()
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      x <- c(x, time[i])
      Percent <- c(Percent, (i / length(time)))
    } 
  }
  
  ad <- 0
  lastz <- 0
  lastf <- 0
  for (j in 1:(length(x) + 1)) {
    if (j == length(x) + 1) {
      z <- 1 - (10 ^ -12)
    } else {
      args <- c(q = x[j], fit$estimate)
      args <- split(unname(args), names(args))
      z <- do.call(pfunc, args)
    }
    f <- Percent[j]
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
      Percent <- c(Percent, (i / length(time)) * 100)
      args <- c(q = time[i], fit$estimate)
      args <- split(unname(args), names(args))
      cdf <- c(cdf, do.call(pfunc, args) * 100)
    } 
  }
  
  df <- data.frame(x, Percent, cdf)
  p <- ggplot(df, aes(x = Percent, y = cdf)) + geom_point() +
      geom_line(aes(x = Percent, y = Percent)) +
      scale_x_continuous(name = "Sample") +#, 
                         #breaks = seq(start, 400, by = (400) / 5), 
                         #limits = c(start, 400)) +
      scale_y_continuous(name = "Theoretical") + #, 
                         #breaks = seq(0, 100, by = 20),
                         #limits = c(0, 100)) +
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
data <- read.csv("Data sets/Rearrest.txt", sep = "\t")
#part i
qqplot(data, "exp", "months", "censor")
surv_summary(data, "exp", "months", "censor") #reports 1/lambda
ad(data, "exp", "months", "censor")
#part ii
qqplot(data, "llogis", "months", "censor")
surv_summary(data, "llogis", "months", "censor") #see parameterization
ad(data, "llogis", "months", "censor")
#part iii
qqplot(data, "logis", "months", "censor")
surv_summary(data, "logis", "months", "censor") 
ad(data, "logis", "months", "censor")
#part iv
qqplot(data, "lnorm", "months", "censor")
surv_summary(data, "lnorm", "months", "censor") 
ad(data, "lnorm", "months", "censor")
#part v
qqplot(data, "weibull", "months", "censor")
surv_summary(data, "weibull", "months", "censor") 
ad(data, "weibull", "months", "censor")

#Question 2 ----

#part a
plot_surv(data, "llogis", "months", "censor")
surv_summary(data, "llogis", "months", "censor") 
#part b
fit <- fit_data(data, "llogis", "months", "censor")
qllogis(.2, fit$estimate[["shape"]], fit$estimate[["scale"]])
#part c
prob(data, "llogis", 11, time = "months", censor = "censor", lower.tail = )
prob(data, "llogis", 11, time = "months", censor = "censor")
#part d
plot_surv(data, "llogis", "months", "censor", "personal")
#part e
surv_summary(data, "llogis", "months", "censor", "personal")
#part f
plot_haz(data, "llogis", "months", "censor", "personal")
