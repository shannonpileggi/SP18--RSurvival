rearrest<-read.table(paste0(datapath,"Rearrest.txt"),header=T)

library(MASS)
library(ggplot2)
library(survival)
library(fitdistrplus)

df <- data.frame(y = rt(200, df = 5))
p <- ggplot(df, aes(sample = y))
p
p + stat_qq()
p + geom_point(stat = "qq")

# Use fitdistr from MASS to estimate distribution params
params <- as.list(MASS::fitdistr(df$y, "t")$estimate)

params

#> Warning: NaNs produced
#> Warning: NaNs produced
ggplot(df, aes(sample = y)) +
  stat_qq(distribution = qt, dparams = params["df"])


p <- ggplot(rearrest, aes(sample = months)) 

p + stat_qq()


# Use fitdistr from MASS to estimate distribution params
params <- as.list(MASS::fitdistr(rearrest$y, "t")$estimate)

check<-fitdistr(rearrest$months, "exponential")
1/check$estimate


set.seed(123)
x3 <- rweibull(100, shape = 4, scale = 100)
fitdistr(x3, "weibull")


df<-Surv(time=rearrest$months,event=rearrest$censor)
df
# Weibull parametrisation
y<-rweibull(1000, shape=2, scale=5)
survreg(Surv(y)~1, dist="weibull")
# survreg scale parameter maps to 1/shape, linear predictor to log(scale)

survreg(df~1, dist="exponential")

data("salinity")
salinity

df2<-data.frame(left=rearrest$months,right=rearrest$months)
df2[rearrest$censor==0,2]<-NA

fit<-fitdistcens(df2, "exp")
names(fit)
1/fit$estimate
