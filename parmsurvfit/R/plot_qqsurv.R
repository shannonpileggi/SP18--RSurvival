
#' Plotting quantile-quantile plots for parametric fitting of data
#'
#' Creates quantile-quantile plot of right censored data given that it follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", "llogis", "gompertz", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "Time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "Censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @import ggplot2 graphics
#' @examples
#' library(survival) 
#' data("kidney")
#' plot_qqsurv(kidney, "logis", time = "time", censor = "status")
#' plot_qqsurv(kidney, "weibull", time = "time", censor = "status")
#' @export

plot_qqsurv <- function(data, dist, time = "Time", censor = "Censor") {
  
  #makes a qqplot of the data with a fitted distribution line
  #data is a dataframe
  #dist is a string name of a distribution
  #time and censor are string names of columns
  
  #fits data
  fit <- fit_data(data, dist, time, censor) 
  #orders data by time
  data <- data[order(data[[time]]),]
  #stored time and censor variables as vectors
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  #stores distribution function
  pfunc <- match.fun(paste("p", dist, sep = ""))
  
  #initializes vectors
  Percent <- c()
  x <- c()
  cdf <- c()
  
  #loopes through each element of time vector
  for (i in 1:length(time)) {
    #if the data point is not censored
    if (censor[i] == 1) {
      #adds the time to the vector of complete times
      x <- c(x, time[i])
      #calculates the cdf of that point, including the censored points
      Percent <- c(Percent, ((i - 1) / length(time)) * 100)
      #calls the distribution function on that point and adds to vector containing the cdfs
      args <- c(q = time[i], fit$estimate)
      args <- split(unname(args), names(args))
      cdf <- c(cdf, do.call(pfunc, args) * 100)
    } 
  }
  
  #vector that contains points to make the line y = x
  line <- seq(0, 100, length.out = length(x))
  
  #creates a dataframe of the data
  df <- data.frame(x, Percent, cdf, line)
  
  #plots qq plot
  p <- ggplot(df, aes(x = Percent, y = cdf)) + geom_point() +
      geom_line(aes(x = line, y = line)) +
      scale_x_continuous(name = "Sample") +
      scale_y_continuous(name = "Theoretical") +  
      expand_limits(x = c(0, 100), y = c(0, 100)) +
      ggtitle(paste(dist, "probability plot")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
  plot(p)
}
