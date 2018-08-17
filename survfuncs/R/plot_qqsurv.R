
#' Plotting quantile-quantile plots for parametric fitting of data
#'
#' Creates quantile-quantile plot of right censored data given that it follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", etc.
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
      scale_y_continuous(name = "Theoretical") +  
      ggtitle(paste(dist, "probability plot")) +
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))
  plot(p)
}