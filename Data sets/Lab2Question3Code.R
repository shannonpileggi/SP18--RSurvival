#Question 3

library(survminer)

data <- read.csv("Data sets/Fruitfly.txt", sep = "\t")

fit <- surv_fit(Surv(Longevity, Censor) ~ Partners,
                data = data)

ggsurvplot(fit, data)

