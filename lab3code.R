library(fitdistrplus)
library(ADGofTest)

ad <- function(data, dist) {

  ad.test(data$Time, pexp, rate) #is ad test same for censored and complete?
  
}
data <- read.csv("Data sets/Rearrest.txt", sep = "\t")
head(data)

data <- read.csv("Data sets/MELT TIMES V2 W2018.txt", sep = "\t")

qqplot <- function(data, dist, time, censor) {
  
  fit <- fit_data(data, dist, time, censor) 
  data <- data[order(data[[time]]),]
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  
  Percent <- c()
  x <- c()
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      x <- c(x, time[i])
      Percent <- c(Percent, (i / length(time)) * 100)
    } 
  }
  
  qfunc <- match.fun(paste("q", dist, sep = ""))
  l <- c(p = .01, fit$estimate)
  s <- split(unname(l), names(l))
  l <- c(p = .99, fit$estimate)
  e <- split(unname(l), names(l))

  start <- floor(do.call(qfunc, s))
  end <- ceiling(do.call(qfunc, e))
  
  df <- data.frame(x, Percent)
  p <- ggplot(df, aes(x = x, y = Percent)) + geom_point() +
      #geom_line(data = data.frame(x = c(start, end), y = c(1, 99)), aes(x = x, y = y)) +
      scale_x_continuous(name = "Time", 
                         breaks = seq(start, 400, by = (400) / 5), 
                         limits = c(start, 400)) +
      scale_y_continuous(name = "Percent", 
                         breaks = seq(0, 100, by = 20),
                         limits = c(0, 100)) +
      ggtitle(paste(dist, "probability plot")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
  plot(p)
}
qqplot(data, "exp", "months", "censor")
