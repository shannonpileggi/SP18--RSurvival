
#' Fitting right censored survival data to distribution
#'
#' Fits right censored data to a distribution using maximum liklihood estimates. 
#' @param data A dataframe containing a time column and a censor column.
#' @param dist A string name for a distribution that has a corresponding density function and a distribution function.
#' Examples include "norm", "lnorm", "exp", "weibull", "logis", etc.
#' @param time The string name of the time column of the dataframe. Defaults to "Time".
#' @param censor The string name of the censor column of the dataframe. Defaults to "Censor". 
#' The censor column must be a numeric indicator variable where complete times correspond 
#' to a value of 1 and incomplete times correspond to 0.
#' @param by The string name of a grouping variable. if specified, the function returns a list. 
#' Variable can contain logical, string, character, or numeric data.
#' 
#' @examples
#' library(survival) 
#' data("rats")
#' fit_data(rats, "lnorm", time = "time", censor = "status")
#' 
#' fit_data(rats, "weibull", time = "time", censor = "status", by = "sex")
#' 
#' @seealso \code{\link[fitdistrplus]{fitdistcens}}
#' @import fitdistrplus
#' @export

fit_data <- function(data, dist, time = "Time", censor = "Censor", by = "") {
  
  left <- c()
  right <- c()
  time <- as.vector(data[[time]])
  censor <- as.vector(data[[censor]])
  
  #if there's a grouping variable, stores it as a vector
  if (by != "") {
    data[[by]] <- as.factor(data[[by]])
    b <- as.factor(as.vector(data[[by]]))
  } else {
    b <- c()
  }
  #formats data so it'll work with fitdistcens function
  for (i in 1:length(time)) {
    if (censor[i] == 1) {
      left <- c(left, time[i])
      right <- c(right, time[i])
    } else {
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
    
  } else {
    #if there's no grouping variable, returns fit object
    fitdistcens(d, dist)
  }
}
