#'---
#'title: "Guide to using PlaceholdR"
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
#' that is easily available in Minitab, but much more difficult to obtain in R. Some examples of some
#' of the items that are readily available in Minitab but not in R, that we attempted to easily 
#' recreate in R are as follows:  
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

# /*
# =============================================================================
# */
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)

# /*
# =============================================================================
# */ 
