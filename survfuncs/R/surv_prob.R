
#' Survival probability based on parametric distribution
#'
#' Computes probability of survival beyond time t given that the data follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and a distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", etc.
#' @param num A scalar quantity, time at which the probability of survival is computed
#' @param lower.tail logical; if \code{F} (default), probability is P(T > \code{num}), otherwise, P(T < \code{num}).
#' @param time The string name of the time column of the dataframe. Defaults to "Time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "Censor". The censor column must be 
#' a numeric indicator variable where complete times correspond to a value of 1 and incomplete times correspond to 0.
#' @examples 
#' library(survival) 
#' data("rats")
#' surv_prob(rats, "lnorm", 110, time = "time", censor = "status")
#' surv_prob(rats, "weibull", 90, time = "time", censor = "status")  
#' @export

surv_prob <- function(data, dist, num, lower.tail = F, time = "Time", censor = "Censor") {
  #"prob" is a placeholder name, should be better
  
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
  } else {
    cat("P(T < ", num, ") = ", do.call(pfunc, args), sep = "")
  }
}