# helpful pages
# http://www.engineeredsoftware.com/nasa/pe_median.htm
# https://weibull.com/hotwire/issue8/relbasics8.htm


time <- c(95, 93, 90, 91, 93, 76, 79, 73, 75, 66)
# censor <- rep(1, 10)
censor <- c(rep(1, 3), rep(0, 7))
data <- data.frame(time, censor)
dist <- "weibull"

n <- sum(censor)
shape <- 64.1298
scale <- 94.0785

data

#fits data to distribution
fit <- fit_data(data, dist, "time", "censor") 


#orders data by time
data <- data[order(data[["time"]]),]

# complete data
data <- data[data[["censor"]]==1, ]

#stores time and censor as vectors
time <- as.vector(data[["time"]])
censor <- as.vector(data[["censor"]])
                    
calc <- data.frame(time = rep(NA, n+1), 
                           censor = rep(NA, n+1),
                           z = rep(NA, n+1),
                           Fz = rep(NA, n+1),
                           z_lag = rep(NA, n+1),
                           Fz_lag = rep(NA, n+1),
                           A = rep(NA, n+1),
                           B = rep(NA, n+1),
                           C = rep(NA, n+1))                    

calc$time[1:n] <- time
calc$censor[1:n] <- censor

# fitted estimate of cdf
calc$z[1:n] <- pweibull(time, shape, scale)

# empirical estimate of cdf based on Median Rank method
calc$Fz[1:n] <- (1:n - 0.3) / (n + 0.4) 

# fixed values
calc$z[n+1] <-0.999999999999
calc$Fz[n+1] <- 1
calc$z_lag[1] <- 0
calc$Fz_lag[1] <- 0

# lag values
calc$z_lag[2:(n+1)] <- calc$z[1:n]
calc$Fz_lag[2:(n+1)] <- calc$Fz[1:n]

# A, B, Cs
calc$A <- -1 * calc$z - log(1 - calc$z) + calc$z_lag + log(1 - calc$z_lag)
calc$B <- 2 * log(1 - calc$z) * calc$Fz_lag - 2 * log(1 - calc$z_lag) * calc$Fz_lag
calc$C <- log(calc$z) * calc$Fz_lag**2 - log(1 - calc$z) * calc$Fz_lag**2 - log(calc$z_lag)*calc$Fz_lag**2  + log(1 - calc$z_lag)*calc$Fz_lag**2
calc$C[1] <- 0


AD <- n * sum(calc$A, calc$B, calc$C)

AD
