
#' Plotting density function overlayed on top of a histogram of data
#'
#' Creates histogram of right censored data with the density function of a fitted parametric distribution overlayed.
#' @param data a dataframe containing a time column and a censor column.
#' @param dist a string name for a distribution that has a corresponding desnity function and distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", etc.
#' @param time the string name of the time column of the dataframe. defaults to "time".
#' @param censor the string name of the censor column of the dataframe. defaults to "censor". the censor column must be 
#' a numeric indicator variable where complete times correspond to a value of 1 and incomplete times correspond to 0.
#' @import ggplot2 graphics
#' @examples
#' library(survival) 
#' data("kidney")
#' plot_density(kidney, "logis", time = "time", censor = "status")
#' plot_density(kidney, "weibull", time = "time", censor = "status")
#' @export
 
plot_density <- function(data, dist, time, censor) {
  
  fit <- fit_data(data, dist, time, censor)
  dfunc <- match.fun(paste("d", dist, sep = ""))
  t <- time
  
  args <- c(fit$estimate)
  args <- split(unname(args), names(args))
  p <- ggplot(data, aes(x = data[[t]])) +
    geom_histogram(aes_string(y = "..density.."), bins = 30) +
    stat_function(fun = dfunc, args = args, 
                  aes(color = paste(round(unname(fit$estimate), 3), collapse = ", ")), size = 1.5) +
    labs(color = paste(names(fit$estimate), collapse = ", ")) +
    scale_x_continuous(name = "time") +
    ggtitle(paste(dist, "curve superimposed")) +
    theme(axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(2)),
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)),
          legend.position = c(0.8,0.8))
  
  p 
  
}
