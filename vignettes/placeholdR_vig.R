#'---
#'title: "Guide to Using survfuncs"
#'author: "Ashley Jacobson & Victor Wilson"
#'output: rmarkdown::html_vignette
#'vignette: >
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteIndexEntry{Guide to using PlaceholdR}
#'  %\VignetteEncoding{UTF-8}
#'---

#+ label=setup, include = FALSE
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)

#'
# /*
# =============================================================================
# */
#'
#' # Introduction
#' 
#' The motivation for this package is to create functions that display output for Survival Analysis
#' that is easily available in Minitab, but much more difficult to obtain in R. All of the
#' functions in this package are assuming that the data follows a specified parametric distribution. 
#' Some examples of some of the items that are readily available in Minitab but not in R, that we 
#' attempted to easily recreate in R are as follows:  
#'  
#' * Computing survival probablities based on a specified parametric distribution  
#' * Displaying an estimated survival, hazard, or cumulative hazard curve based on a specified
#' parametric distribution (singular as well as by groups)  
#' * Estimate survival times based on a specified parametric distribution   
#' * Estimate parameters based on a specified parametric distribution    
#' * Estimate various statistics, including mean, median, standard deviation, and percentiles of
#' survival time based on a specified parametric distribution   
#' * Plot estimated hazard/cumulative hazard functions    
#' * Compute the Wilcoxon test-statistic and p-value to compare survival experiences of 
#' two groups.   


#'
# /*
# =============================================================================
# */
#'
#' # Fitting Right Censored Survival Data 
#'
#'In order for any of the functions in this package to work, the dataset must be formatted
#'correctly, using the 'fit_data' function, which fits right censored data to a distribution
#'using maximum likelihood estimates. 
#'
#'## Examples
library(placeholdR)
library(survival)
data("rats")
fit_data(rats, "logis", "time", "status")
#'
# /*

# =============================================================================
# */
#'
#' # Plotting Survival Curves
#' The plot_surv function plots the survival curve of right censored data, once it has been
#' formatted using the fit_data function, given that it follows a specified parametric
#' distribution. Plotted on the x-axis is time after start of observation, and plotted
#' on the y-axis is proportion of subjects surviving.
#' 
#' ##Example
library(survival) 
data("rats")
plot_surv(rats, "lnorm", time = "time", censor = "status")

#' As seen in this survival curve, at time = 200, roughly 40% of rats are still surviving.
#' 
# /*

# =============================================================================
# */ 
#'
#' # Plotting Hazard Curves
#'  The plot_haz function plots the hazard curve of right censored data, given that it follows 
#'  a specified parametric distribution. The hazard displays the conditional risk that a subject 
#'  will experience the event of interest given that the subject has survived beyond a certain
#'  amount of time. 
#'  
#' ##Examples
library(survival)
data("rats")
plot_haz(rats, "weibull", time="time", censor="status")
#'
# /*
# =============================================================================
#*/
#' 
#' # Plotting Cumulative Hazard Curves
#' The plot_cumhaz funtion plots the cumulative hazard curve of right censored data, given that
#' it follows a specified parametric distribution. It is important to note that the cumulative
#' hazard function is neither a probability nor a rate, it is merely an accumlation of hazard
#' rates over time.
#'  
#' ##Example
library(survival)
data("rats")
plot_cumhaz(rats, "weibull", time="time", censor="status")
#'
# /*
# =============================================================================
# */
#'
#' #Computing Survival Probabilities
#' One of the most important features of survival analysis is being able to compute the
#' probability that a subject survives beyond a certain time t. Thus, we developed the function
#' surv_prob. This function computes survival probabilites, given that the data follows a
#' specified parametric distribution. 
#' 
#' ##Example
library(survival)
data("rats")
prob(rats, "lnorm", 110, time = "time", censor = "status")

#' Here we see that using the rats dataset found in the survival package, assuming that the data
#' follows a log-normal distribution, the probability that a rat survives beyond 110 days is 
#' rougly 0.8. 
#' 
# /*
# =============================================================================
# */
#'
#' #Computing Summary Statistics 
#' Another form of output that is easily computed in Minitab but not in R are various summary
#' statistics based on a specified parametric distribution. We developed the surv_summary function
#' to combat this. This function estimates various statistics, including mean, median, standard
#' deviation, and percentiles of survival time given that the data follows a specified parametric
#' distribution. Requried parameters are dataset, distribution, name of the Time column of the
#' data frame, and name of the Censor column of the data frame. There is also an optional
#' parameter "by", for a grouping variable, that when specified will return summary statistics
#' for each group. 
#' 
#' ##Example
library(survival)
data("rats")
surv_summary(rats, "lnorm", time = "time", censor = "status")
