
#' Survival probability based on parametric distribution
#'
#' Compute survival probabilities given that the data follows a specified parametric distribution.
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding desnity function and a distribution function.
#' @param num A scalar quantity
#' @param lower.tail logical; if \code{F} (default), probability is P(T > \code{num}), otherwise, P(T < \code{num}).
#' @param time The string name of the time column of the dataframe. Defaults to "Time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "Censor". The censor column must be 
#' a numeric indicator variable where complete times correspond to a value of 1 and incomplete times correspond to 0.

prob <- function(data, dist, num, lower.tail = F, time = "Time", censor = "Censor") {
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