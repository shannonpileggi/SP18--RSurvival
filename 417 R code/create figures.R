datapath<-"C:/Users/spileggi/Google Drive/STAT 417/Jeffs stuff/Data Sets/"
figpath<-"C:/Users/spileggi/Google Drive/STAT 417/Slides/Figures/"


library(ggplot2)
library(flexsurv)
library(gridExtra)
library(survival)
library(KMsurv)
library(survminer)


drink<-read.table(paste0(datapath,"Firstdrink.txt"),header=T)
motorists<-read.table(paste0(datapath,"Aggressive.txt"),header=T)
lung<-read.table(paste0(datapath,"Lung.txt"),header=T)
rearrest<-read.table(paste0(datapath,"Rearrest.txt"),header=T)
veteran<-read.table(paste0(datapath,"veteran.txt"),header=T)
graduate<-read.table(paste0(datapath,"Graduate.txt"),header=T)


#-------------------------------------------------------------------------------
#Set 1 - distribution of survival times
#-------------------------------------------------------------------------------

#stack four survival times
stacked <- data.frame(label=c(rep("Age at first drink",length(drink$Age)),
                             rep("Seconds until horn honk",length(motorists$seconds)),
                             rep("Days until death from cancer",length(lung$time)),
                             rep("Months until rearrest",length(rearrest$months))),
                     time=c(drink$Age, motorists$seconds, lung$time, rearrest$months))


png(paste0(figpath,"exampletimes.png"))
ggplot(stacked, aes(time)) + facet_wrap(~label, scales='free') + 
  geom_histogram() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5))
        )
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - parametric distributions
#-------------------------------------------------------------------------------
#Exponential density curves
p <- ggplot(data.frame(x=c(0,3)),aes(x=x)) +    
  stat_function(fun=dexp,args=(mean=1/0.2),aes(colour = "0.2"),size=1.5) +
  stat_function(fun=dexp,args=(mean=1/0.4),aes(colour = "0.4"),size=1.5) +
  stat_function(fun=dexp,args=(mean=1/0.6),aes(colour = "0.6"),size=1.5) +
  stat_function(fun=dexp,args=(mean=1/0.8),aes(colour = "0.8"),size=1.5) +
  scale_x_continuous(name="T") +
  #scale_y_continuous(name="Density") +
  labs(colour="Lambda") +
  ggtitle("Exponential density curves") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.9,0.8)) 

png(paste0(figpath,"exponential.png"))
p  
dev.off()

#Weibull density curves
p <- ggplot(data.frame(x=c(0,10)),aes(x=x)) +    
  stat_function(fun=dweibull,args=list(shape=1,scale=1),aes(colour = "1, 1"),size=1.5) +
  stat_function(fun=dweibull,args=list(shape=1,scale=2),aes(colour = "1, 2"),size=1.5) +
  stat_function(fun=dweibull,args=list(shape=2,scale=1),aes(colour = "2, 1"),size=1.5) +
  stat_function(fun=dweibull,args=list(shape=2,scale=2),aes(colour = "2, 2"),size=1.5) +
  scale_x_continuous(name="T") +
  #scale_y_continuous(name="Density") +
  labs(colour="Beta, Lambda") +
  ggtitle("Weibull density curves") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.8,0.8))

png(paste0(figpath,"weibull.png"))
p  
dev.off()


#Lognormal density curves
p <- ggplot(data.frame(x=c(0,35)),aes(x=x)) +
  stat_function(fun=dlnorm,args=list(meanlog=1,sdlog=1),aes(colour = "1, 1"),size=1.5) +
  stat_function(fun=dlnorm,args=list(meanlog=1,sdlog=2),aes(colour = "1, 2"),size=1.5) +
  stat_function(fun=dlnorm,args=list(meanlog=2,sdlog=1),aes(colour = "2, 1"),size=1.5) +
  stat_function(fun=dlnorm,args=list(meanlog=2,sdlog=2),aes(colour = "2, 2"),size=1.5) +
  scale_x_continuous(name="T") +
  #scale_y_continuous(name="Density") +
  labs(colour="Mu, Sigma") +
  ggtitle("Lognormal density curves") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.8,0.8))

png(paste0(figpath,"lognormal.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - overlay exponential distribution on motorists
#-------------------------------------------------------------------------------
#Note - exponential parameter is 1 over lambda
p <- ggplot(motorists,aes(x=seconds)) +
   geom_histogram(aes(y=..density..)) +
   stat_function(fun=dexp,args=(mean=1/5.8),aes(colour = "5.8"),size=1.5) +
   labs(colour="Lambda") +
   scale_x_continuous(name="Time until horn honk (seconds)") +
   ggtitle("Exponential curve superimposed") +
   theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.8,0.8))
  
png(paste0(figpath,"motorists_exp.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - overlay lognormal distribution on motorists
#-------------------------------------------------------------------------------
p <- ggplot(motorists,aes(x=seconds)) +
  geom_histogram(aes(y=..density..),binwidth=1) +
  stat_function(fun=dlnorm,args=list(meanlog=1.4,sdlog=0.6),aes(colour = "1.4,0.6"),size=1.5) +
  labs(colour="Mu, Sigma") +
  scale_x_continuous(name="Time until horn honk (seconds)") +
  ggtitle("Lognormal curve superimposed") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.8,0.8))

png(paste0(figpath,"motorists_lognormal.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - pdf of normal distribution shaded
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm) + 
  stat_function(fun = dnorm,xlim = c(-3,1), geom = "area") +
  scale_x_continuous(name="T", breaks=seq(-3,3,by=1)) +
  scale_y_continuous(name="Density") +
  ggtitle("Standard normal density") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"shadednormalpdf.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#Set 1 - cdf of normal distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = pnorm) +
  scale_x_continuous(name="T", breaks=seq(-3,3,by=1)) +
  scale_y_continuous(name="Density", breaks=seq(0,1,by=0.2)) +
  ggtitle("CDF of standard normal") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"normalcdf.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - pdf of exponential distribution shaded
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 15)), aes(x)) +
  stat_function(fun = dexp,args=(mean=1/5)) + 
  stat_function(fun = dexp,args=(mean=1/5),xlim = c(0,5), geom = "area") +
  scale_x_continuous(name="T", breaks=seq(0,15,by=5)) +
  scale_y_continuous(name="Density") +
  ggtitle("Exponential density (lambda=5)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"shadedexppdf.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 1 - cdf of exp distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 15)), aes(x)) +
  stat_function(fun = pexp,args=(mean=1/5)) + 
  scale_x_continuous(name="T", breaks=seq(0,15,by=5)) +
  scale_y_continuous(name="Density", breaks=seq(0,1,by=0.2)) +
  ggtitle("CDF of exponential (lambda=5)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"expcdf.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 1 - pdf of gompertz distribution shaded
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01)) + 
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01),xlim = c(0,12), geom = "area") +
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="Density") +
  ggtitle("Gompertz density (theta=0.01, alpha=0.25)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"shadedgompertzpdf.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 1 - CDF of gompertz distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = pgompertz,args=list(shape=0.25,rate=0.01)) + 
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="Density") +
  ggtitle("CDF of gompertz (theta=0.01, alpha=0.25)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"gompertzcdf.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


n = 2
cols = gg_color_hue(n)

dev.new(width = 4, height = 4)
plot(1:n, pch = 16, cex = 2, col = cols)

cols

#-------------------------------------------------------------------------------
#Set 2 - pdf of gompertz distribution shaded lower
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01)) + 
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01),xlim = c(0,16), geom = "area") +
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="f(t)") +
  ggtitle("Gompertz f(t) (theta=0.01, alpha=0.25)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"shadedgompertzpdfL.png"))
p  
dev.off()

pgompertz(q=16,shape=0.25,rate=0.01)

#-------------------------------------------------------------------------------
#Set 2 - pdf of gompertz distribution shaded upper
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01)) + 
  stat_function(fun = dgompertz,args=list(shape=0.25,rate=0.01),xlim = c(16,24), geom = "area") +
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="f(t)") +
  ggtitle("Gompertz f(t) (theta=0.01, alpha=0.25)") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"shadedgompertzpdfU.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 2 - CDF of gompertz distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = pgompertz,args=list(shape=0.25,rate=0.01)) + 
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="F(t)",breaks=seq(0,1,by=0.2)) +
  ggtitle("Gompertz F(t) (theta=0.01, alpha=0.25)") +
  geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"gompertzcdfL12.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#Set 2 - Survival of exponential distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = function(x) {1-pexp(x,rate=1/5)}) + 
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  ggtitle("Exponential S(t) (lambda=5)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"expsurv.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 2 - Survival of gompertz distribution 
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 24)), aes(x)) +
  stat_function(fun = function(x) {1-pgompertz(x,shape=0.25,rate=0.01)}) + 
  scale_x_continuous(name="T", breaks=seq(0,24,by=4)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  ggtitle("Gompertz S(t) (theta=0.01, alpha=0.25)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"gompertzsurv.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 2 - Survival of gompertz distribution - compare parameters
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = function(x) {1-pgompertz(x,shape=0.25,rate=0.01)}, aes(colour=("A: 0.25,0.01"))) +
  stat_function(fun = function(x) {1-pgompertz(x,shape=0.50,rate=0.01)}, aes(colour=("B: 0.50,0.01"))) +
  stat_function(fun = function(x) {1-pgompertz(x,shape=0.75,rate=0.01)}, aes(colour=("C: 0.75,0.01"))) +
  stat_function(fun = function(x) {1-pgompertz(x,shape=1.00,rate=0.01)}, aes(colour=("D: 1.00,0.01"))) +
  scale_x_continuous(name="T", breaks=seq(0,20,by=4)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  labs(colour="Dose: alpha, theta") +
  ggtitle("Gompertz S(t)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.8,0.8))

png(paste0(figpath,"gompertzsurv_varyalpha.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 2 - compare survival experiences between exponential and weibull
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {1-pweibull(x,shape=1.6,scale=5.74)}, aes(colour=("Weibull (lambda=5.74, beta=1.6)"))) +
  stat_function(fun = function(x) {1-pexp(x,rate=1/5.77)}, aes(colour=("Exponential (lambda=5.77)"))) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  labs(colour="Distribution") +
  ggtitle("Survival for motorist reaction times") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.65,0.90))

png(paste0(figpath,"motorist_surv_weib_exp.png"))
p  
dev.off()

1-pweibull(10,shape=1.6,scale=5.74)
1-pweibull(20,shape=1.6,scale=5.74)
1-pexp(10,rate=1/5.77)
1-pexp(20,rate=1/5.77)


#-------------------------------------------------------------------------------
#Set 2 - compare survival experiences between groups (Weibull)
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 40)), aes(x)) +
  stat_function(fun = function(x) {1-pweibull(x,shape=2.64,scale=18.28)}, aes(colour=("Males"))) +
  stat_function(fun = function(x) {1-pweibull(x,shape=2.52,scale=20.85)}, aes(colour=("Females"))) +
  scale_x_continuous(name="T", breaks=seq(0,40,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  labs(colour="Distribution") +
  ggtitle("Survival for age at first drink of alcohol") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.90,0.90))

png(paste0(figpath,"drink_surv_weib.png"))
p  
dev.off()

1-pweibull(20,shape=2.64,scale=18.28)
1-pweibull(20,shape=2.52,scale=20.85)


#-------------------------------------------------------------------------------
#Set 2 - weibull hazard and survival for motorist reaction time
#-------------------------------------------------------------------------------

#survival
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {1-pweibull(x,shape=1.6,scale=5.74)}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  #labs(colour="Distribution") +
  ggtitle("Weibull survival function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_surv_weib.png"))
p  
dev.off()

#hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {dweibull(x,shape=1.6,scale=5.74)/(1-pweibull(x,shape=1.6,scale=5.74))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="h(t)",breaks=seq(0,0.8,by=0.1)) +
  #labs(colour="Distribution") +
  ggtitle("Weibull hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_haz_weib.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 2 - exponential hazard and survival for motorist reaction time
#-------------------------------------------------------------------------------

#survival
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {1-pexp(x,rate=1/5.77)}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  #labs(colour="Distribution") +
  ggtitle("Exponential survival function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_surv_exp.png"))
p  
dev.off()

#hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {dexp(x,rate=1/5.77)/(1-pexp(x,rate=1/5.77))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="h(t)", breaks=seq(0,0.5,by=.1)) +
  #labs(colour="Distribution") +
  ggtitle("Exponential hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_haz_exp.png"))
p  
dev.off()



#-------------------------------------------------------------------------------
#Set 2 - logistic hazard and survival for age at first drink
#-------------------------------------------------------------------------------

#survival
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {1-plogis(x,location=16.74,scale=2.8)}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  #labs(colour="Distribution") +
  ggtitle("Logistic survival function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"drink_surv_log.png"))
p  
dev.off()

#hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {dlogis(x,location=16.74,scale=2.8)/(1-plogis(x,location=16.74,scale=2.8))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="h(t)",breaks=seq(0,0.5,by=0.1)) +
  #labs(colour="Distribution") +
  ggtitle("Logistic hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))



png(paste0(figpath,"drink_haz_log.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#Set 2 - exponential cumulative hazard for motorist reaction time
#-------------------------------------------------------------------------------

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pexp(x,rate=1/5.77))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  ggtitle("Exponential cumulative hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_cum_haz_exp.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#Set 2 - weibull cumulative hazard for motorist reaction time
#-------------------------------------------------------------------------------

#survival
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pweibull(x,shape=1.6,scale=5.74))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  ggtitle("Weibull cumulative hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"motorist_cum_haz_weib.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#Set 2 - cumulative hazard for age at first drink
#-------------------------------------------------------------------------------

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {-log(1-plogis(x,location=16.74,scale=2.8))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  ggtitle("Logistic cumulative hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"drink_cum_haz_log.png"))
p  
dev.off()

#hazard
p <- ggplot(data.frame(x = c(0, 30)), aes(x)) +
  stat_function(fun = function(x) {dlogis(x,location=16.74,scale=2.8)/(1-plogis(x,location=16.74,scale=2.8))}) +
  scale_x_continuous(name="T", breaks=seq(0,30,by=5)) +
  scale_y_continuous(name="H(t)",breaks=seq(0,0.5,by=0.1)) +
  #labs(colour="Distribution") +
  ggtitle("Logistic hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"drink_haz_log.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#--------------------------------------------- ----------------------------------
#Set 3 - Survival of exponential distribution motorist reaction time with mean
#-------------------------------------------------------------------------------

p <- ggplot(data.frame(x = c(0, 40)), aes(x)) +
  stat_function(fun = function(x) {1-pexp(x,rate=1/13.5)}) + 
  scale_x_continuous(name="T", breaks=seq(0,40,by=5)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  ggtitle("Exponential S(t) (lambda=13.5)") +
  geom_vline(aes(xintercept=13.5,colour="Mean"),size=1.1) +
  labs(colour="") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.90,0.90))

png(paste0(figpath,"expsurv_mean.png"))
p  
dev.off()


#--------------------------------------------- ----------------------------------
#Set 3 - parametric densities for veteran lung cancer
#-------------------------------------------------------------------------------


p <- ggplot(veteran,aes(x=time)) +
  geom_histogram(aes(y=..density..,x=time),binwidth=50,center=25) +
  scale_x_continuous(name="Time until death from lung cancer (days)", breaks=seq(-300,1000,by=100)) +
  stat_function(fun=dexp,args=list(rate=1/130.2),aes(colour = "Exponential (lambda=130.2)"),size=1.5) +
  stat_function(fun=dnorm,args=list(mean=130.7,sd=162.2),aes(colour = "Normal (mean=130.7, sd=162.2)"),size=1.5) +
  labs(colour="Distribution") +
  ggtitle("Lung cancer example") +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        legend.position = c(0.70,0.90))
  
png(paste0(figpath,"veteran_parametric_densities.png"))
p  
dev.off()



#--------------------------------------------- ----------------------------------
#Set 3 - MLE  - likelihood curve
#-------------------------------------------------------------------------------


dat<-c(5.3, 4.8, 0.4, 2.3,0.4)
dat
sum(dat)


p <- ggplot(data.frame(x = c(0, 10)), aes(x)) +
  stat_function(fun = function(x) {(1/x^5)*exp(-sum(dat)/x)},size=1.5,color=cols[1]) +
  ggtitle("Exponential likelihood function") +
  scale_x_continuous(name="lambda", breaks=seq(0.0,10,by=2)) +
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))
  

png(paste0(figpath,"exp_likelihood.png"))
p  
dev.off()


#--------------------------------------------- ----------------------------------
#Set 4 - KM estimates
#-------------------------------------------------------------------------------
time <- c(1.41,1.41,2.76,3.56,4.18,4.71,13.18)
censor <- c(1,0,0,1,1,0,1)
motorist_surv <- Surv(time,censor)
KM_obj <- survfit(motorist_surv~1, conf.type="none")
summary(KM_obj)
plot(KM_obj, xlab="Seconds", ylab="Survival Probability", main="KM Curve")

png(paste0(figpath,"KM_curve_samp_motorist.png"))
plot(KM_obj, xlab="Seconds", ylab="Survival Probability", main="KM Curve",cex.axis=1.5, cex.lab=1.5)
dev.off()


#all events censored
time <- c(1.41,1.41,2.76,3.56,4.18,4.71,13.18)
censor <- c(0,0,0,0,0,0,0)
motorist_surv <- Surv(time,censor)
KM_obj <- survfit(motorist_surv~1, conf.type="none")
summary(KM_obj)
plot(KM_obj, xlab="Seconds", ylab="Survival Probability", main="KM Curve")

#max time censored
time <- c(1.41,1.41,2.76,3.56,4.18,4.71,13.18)
censor <- c(1,0,0,1,1,0,0)
motorist_surv <- Surv(time,censor)
KM_obj <- survfit(motorist_surv~1, conf.type="none")
summary(KM_obj)
plot(KM_obj, xlab="Seconds", ylab="Survival Probability", main="KM Curve")

#--------------------------------------------- ----------------------------------
#Set 5 - KM estimates and CI
#-------------------------------------------------------------------------------

time <- c(1.41, 1.41, 2.76, 3.56, 4.18, 4.71, 13.18)
censor <- c(1, 0, 0, 1, 1, 0, 1)
motorist_surv <- Surv(time, censor)
KM_obj <- survfit(motorist_surv ~ 1, conf.type = "plain")
summary(KM_obj)

png(paste0(figpath,"KM_curve_samp_motorist_CI.png"))
plot(KM_obj, xlab = "Seconds", ylab = "Survival Probability", main = "KM Curve",cex.axis=1.5, cex.lab=1.5)
dev.off()



# Alternative
library(survminer)
motorist <- data.frame(time = c(1.41, 1.41, 2.76, 3.56, 4.18, 4.71, 13.18),
                       censor = c(1, 0, 0, 1, 1, 0, 1))

fit <- survfit(Surv(time, censor) ~ 1, data =  motorist, conf.type = "plain")

png(paste0(figpath,"KM_curve_samp_motorist_gg.png"))
ggsurvplot(fit, data = motorist, conf.int = TRUE, risk.table = TRUE)
dev.off()


#log-log CI
time <- c(1.41, 1.41, 2.76, 3.56, 4.18, 4.71, 13.18)
censor <- c(1, 0, 0, 1, 1, 0, 1)
motorist_surv <- Surv(time, censor)
KM_obj <- survfit(motorist_surv ~ 1, conf.type = "log-log")
summary(KM_obj)

png(paste0(figpath,"KM_curve_samp_motorist_CIloglog.png"))
plot(KM_obj, xlab = "Seconds", ylab = "Survival Probability", main = "KM Curve (CI based on log-log)",cex.axis=1.5, cex.lab=1.5)
dev.off()

KM_obj1 <- survfit(Surv(seconds, censor) ~ 1, data =  motorists, conf.type = "plain")
KM_obj2 <- survfit(Surv(seconds, censor) ~ 1, data =  motorists, conf.type = "log-log")


png(paste0(figpath,"KM_St_2CI.png"))
plot(KM_obj1, xlab="Seconds", ylab="Survival Probability",
     main="",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5, col="blue",lty=1)
lines(KM_obj2, lty=2, col="red")
legend("topright",c("plain","log-log"),col=c("blue","red"),cex=1.5,lty=c(2,3))
dev.off()

#--------------------------------------------- ----------------------------------
#Set 6 - hazard and cum haz
#-------------------------------------------------------------------------------


#hazard
time <- c(1.41, 1.41, 2.76, 3.56, 4.18, 4.71, 13.18)
censor <- c(1, 0, 0, 1, 1, 0, 1)
KM_obj <- survfit(Surv(time, censor) ~ 1)

png(paste0(figpath,"KM_haz.png"))
plot_haz(KM_obj)
dev.off()

# hazard all motorists
KM_obj <- survfit(Surv(seconds, censor) ~ 1, data =  motorists)

png(paste0(figpath,"KM_haz_allmotorists.png"))
plot_haz(KM_obj)
dev.off()

png(paste0(figpath,"KM_chaz_allmotorists.png"))
plot_chaz(KM_obj)
dev.off()


# Nelson-Aalen estimator of S(t)
time <- c(1.41, 1.41, 2.76, 3.56, 4.18, 4.71, 13.18)
censor <- c(1, 0, 0, 1, 1, 0, 1)
KM_obj_na <- survfit(Surv(time, censor) ~ 1, type = "fh", conf.type = "none")
KM_obj_km <- survfit(Surv(time, censor) ~ 1, conf.type = "none")

plot(KM_obj_na, xlab="Seconds", ylab="Survival Probability",
     main="")
lines(KM_obj_km, lty=2)
legend("topright",c("Nelson-Aalen","Kaplan-Meier"),lty=1:2)

png(paste0(figpath,"KM_St_na_km.png"))
plot(KM_obj_na, xlab="Seconds", ylab="Survival Probability",
     main="",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(KM_obj_km, lty=2)
legend("topright",c("Nelson-Aalen","Kaplan-Meier"),lty=1:2,cex=1.5)
dev.off()

#--------------------------------------------- ----------------------------------
#Set 7 - comparing groups
#-------------------------------------------------------------------------------

# age at first drink, entire data set ----

KM_obj <- survfit(Surv(Age, Censor) ~ Gender, data = drink)
png(paste0(figpath,"KM_drink_gender.png"))
ggsurvplot(KM_obj, data = drink, risk.table = TRUE, title = "Age at first drink")
dev.off()

# age at first drink, subset ----

drinksub <- data.frame(time   = c(43, 15, 19, 14, 18, 16, 14,
                                  18, 15, 17, 16, 40, 24, 16),
                       censor = c( 0,  1,  1, 1,   0,  1,  1,
                                   1,  1,  1, 1,   0,  0,  1),
                       gender = c("m","m","m","m","m","m","m",
                                  "f","f","f","f","f","f","f"))
drinksub
KM_obj <- survfit(Surv(time, censor) ~ gender, data = drinksub)
summary(KM_obj)


png(paste0(figpath,"KM_drinksub_gender.png"))
ggsurvplot(KM_obj, data = drinksub, risk.table = TRUE, title = "Age at first drink (subset)")
dev.off()

#ggsave(filename = paste0(figpath,"KM_drinksub_gender.png"), plot = p)

survdiff(Surv(time, censor) ~ gender, data = drinksub)

# VALCGS example 4 groups ----

# focus on treatment group 1 only
veteran1 <- veteran[veteran$trt == 1, ]

KM_obj <- survfit(Surv(time, status) ~ celltype, data = veteran1)

png(paste0(figpath,"KM_valcsg_r.png"))
ggsurvplot(KM_obj, data = veteran1, risk.table = TRUE, title = "VALCSG study, trt=1")
dev.off()

survdiff(Surv(time, status) ~ celltype, data = veteran1)

#--------------------------------------------- ----------------------------------
#Set 8 - CR model one categorical predictor
#-------------------------------------------------------------------------------

graduate$Gender <- factor(graduate$Gender, labels = c("males","females")) 

KM_obj <- survfit(Surv(Years, Censor) ~ Gender, data = graduate)

png(paste0(figpath,"KM_grad_surv.png"))
ggsurvplot(KM_obj, data = graduate, risk.table = TRUE, title = "KM curves")
dev.off()

source('C:/Users/spileggi/Google Drive/STAT 417/plot.haz.R')




#Plot Estimates of Cumulative Hazard for Males and Females
na.grad.m <- survfit(Surv(Years,Censor)~1, 
                     data=graduate[graduate$Gender==0,],type="fh")

na.grad.f <- survfit(Surv(Years,Censor)~1, 
                     data=graduate[graduate$Gender==1,],type="fh")

png(paste0(figpath,"KM_cumhaz_surv.png"))
plot(na.grad.m$time,-log(na.grad.m$surv),xlab="Years to Graduate",
     ylab = "Cumulative Hazard", 
     main = "N-A Estimator of H(t)",
     type = "s",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(na.grad.f$time,-log(na.grad.f$surv),lty=2,type="s")
legend("topleft",c("Males","Females"),lty=1:2, cex = 1.5)
dev.off()


#Plot Estimates of Log Cumulative Hazard
png(paste0(figpath,"KM_logcumhaz_surv.png"))
plot(na.grad.m$time,log(-log(na.grad.m$surv)),
     xlab = "Years to Graduate",
     ylab = "Log Cumulative Hazard", 
     main = "Log of N-A Estimator of H(t)",
     type = "s",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(na.grad.f$time,log(-log(na.grad.f$surv)),lty=2,type="s")
legend("topleft",c("Males","Females"),lty=1:2, cex = 1.5)
dev.off()




#####################################################################
#  Illustrating proportionality
#  1-6-10
####################################
#Assumed Weibull distribution with beta=1.6, lambda=5.74
#Assumed regression function:
time <- seq(0,40,length=100)

ht <- (1.6*time^(.6))/(5.74^1.6)
St <- exp(-(time/5.74)^1.6)
Ht <- (time/5.74)^1.6

htx1 <- ht*exp(.5*0) #x1=0
htx2 <- ht*exp(.5*1) #x1=1

Htx1 <- Ht*exp(.5*0) #x1=0
Htx2 <- Ht*exp(.5*1) #x1=1


#Plots of hazards and log hazards
#par(mfrow=c(1,2))
png(paste0(figpath,"prop_haz_groups.png"))
plot(time,htx2,
     type = "l",
     lty = 2,
     xlab = "Time",
     ylab = "h(t)",
     main = "Hazard functions",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5
     )
lines(time,htx1,lty=1)
legend("topleft",c("Group 1","Group 2"),lty=1:2, cex=1.5)
dev.off()

png(paste0(figpath,"prop_loghaz_groups.png"))
plot(time,log(htx2),
     type = "l",
     lty = 2,
     xlab = "Time",
     ylab = "log[h(t)]",
     main = "Log hazard functions",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(time,log(htx1),lty=1)
legend("bottomright",c("Group 1","Group 2"),lty=1:2, cex=1.5)
dev.off()




# CR model graduate -----

head(graduate)

CR_grad <- coxph(Surv(Years, Censor) ~ as.factor(Gender), data = graduate)
summary(CR_grad)

# CR model lung cancer -----

head(veteran)

CR_lung <- coxph(Surv(time, status) ~ karno, data = veteran)
summary(CR_lung)

#--------------------------------------------- ----------------------------------
#Set 10 - CR model multiple predictors predictor
#-------------------------------------------------------------------------------
CR_mod1 <- coxph(Surv(time, status) ~ karno + trt, data = veteran)

summary(CR_mod1)

CR_mod1$var

CR_mod1$loglik

CR_mod2 <- coxph(Surv(time, status) ~ celltype, data = veteran)
summary(CR_mod2)

CR_mod3 <- coxph(Surv(time, status) ~ as.factor(celltype), data = veteran)
summary(CR_mod3)

table(veteran$celltype)


#--------------------------------------------- ----------------------------------
#Set 11 - CR model multiple predictors predictor, more
#-------------------------------------------------------------------------------

# create blank plot ----

png(paste0(figpath,"blank_interaction.png"))
plot(c(0,100), c(0,100), 
     type = "n", 
     xlab = "Karnofsky score", 
     ylab = "log hazard", 
     yaxt = "n",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
legend("topright",c("Standard", "Test"), lty = 1:2, cex = 1.5)
dev.off()


CR_mod1 <- coxph(Surv(time, status) ~ karno + trt + karno:trt, data = veteran)
summary(CR_mod1)


CR_mod2 <- coxph(Surv(time, status) ~ karno*trt, data = veteran)
summary(CR_mod2)


CR_mod2$coefficients

exp(10*CR_mod2$coefficients[1]+10*CR_mod2$coefficients[3])
exp(10*CR_mod2$coefficients[1])

# nested models ----

CR_full <- coxph(Surv(time, status) ~ karno + trt + celltype + age + 
                                      karno:age + karno:celltype,
                 data = veteran)

CR_red <- coxph(Surv(time, status) ~ karno + trt + celltype + age,  
                  data = veteran)

summary(CR_full)
summary(CR_red)

CR_full$loglik
CR_red$loglik

pchisq(q = 8.945, df = 4, lower.tail = F)

# non-nested models ----

CR_mod1 <- coxph(Surv(time, status) ~ karno + celltype,
                 data = veteran)

CR_mod2 <- coxph(Surv(time, status) ~ trt + celltype + age,  
                data = veteran)

CR_mod1$loglik
CR_mod2$loglik


# predictor adjusted survival curves ----

# unadjusted survival
KM_obj <- survfit(Surv(time, status) ~ 1, 
                  conf.type = "none",
                  data = veteran)

# Cox regression model with Karnofsky score
CR_mod <- coxph(Surv(time, status) ~ karno, data = veteran)

# data frame that contains new values for prediction
pred_max_karno <- data.frame(karno = 90, veteran)
pred_min_karno <- data.frame(karno = 10, veteran)

# adjusted survival estimates based on results from Cox model
adj_surv_max <- survfit(CR_mod, newdata = pred_max_karno)
adj_surv_min <- survfit(CR_mod, newdata = pred_min_karno)

plot(KM_obj,
     xlab = "Days",
     ylab = "Est. survival prob.")
lines(adj_surv_min,lty = 2)
lines(adj_surv_max, lty = 3)
legend("topright",c("Unadjusted", "Karno = 10","Karno = 90"),lty = 1:3)

png(paste0(figpath,"karno_adj.png"))
plot(KM_obj,
     xlab = "Days",
     ylab = "Est. survival prob.",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(adj_surv_min,lty = 2)
lines(adj_surv_max, lty = 3)
legend("topright",c("Unadjusted", "Karno = 10","Karno = 90"),lty = 1:3, , cex = 1.5)
dev.off()



# Martingale residual -----

CR_mod <- coxph(Surv(time, status) ~ karno + trt + age, data = veteran)
veteran$mart <- residuals(CR_mod, type = "martingale")

png(paste0(figpath,"martingale_residuals.png"))
par(mfrow=c(1,3),pty="s")
plot(x = veteran$karno,
     y = veteran$mart,
     xlab = "Karnofsky score",
     ylab = "Martingale residual",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(veteran$karno, veteran$mart), col = 3)

plot(x = veteran$trt,
     y = veteran$mart,
     xlab = "Treatment",
     ylab = "Martingale residual",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(veteran$trt, veteran$mart), col = 3)

plot(x = veteran$age,
     y = veteran$mart,
     xlab = "Age",
     ylab = "Martingale residual",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(veteran$age, veteran$mart), col = 3)
dev.off()

# Deviance residual -----

CR_mod <- coxph(Surv(time, status) ~ karno + trt + age, data = veteran)
veteran$deviance <- residuals(CR_mod, type = "deviance")

png(paste0(figpath,"deviance_residuals.png"))
plot(x = 1:nrow(veteran), 
     y = veteran$deviance, 
     xlab = "Subject index",
     ylab = "Deviance residuals",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
abline(h = 0)
dev.off()

veteran[abs(veteran$deviance) > 2.5, c(1:8,10)]



# Schoenfeld residual -----
CR_mod <- coxph(Surv(time, status) ~ karno + trt + age, data = veteran)
schoen <- residuals(CR_mod, type = "schoenfeld")

complete_times <- sort(veteran$time[veteran$status!=0])

png(paste0(figpath,"schoen_residuals.png"))

par(mfrow = c(1,3), pty = "s")

plot(x = complete_times, 
     y = schoen[,1],
     xlab = "Complete times",
     ylab = "Schoenfeld residuals",
     main = "Karnofsky score",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(complete_times, schoen[,1]), col = 3)

plot(x = complete_times, 
     y = schoen[,2],
     xlab = "Complete times",
     ylab = "Schoenfeld residuals",
     main = "Treatment",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(complete_times, schoen[,2]), col = 3)

plot(x = complete_times, 
     y = schoen[,3],
     xlab = "Complete times",
     ylab = "Schoenfeld residuals",
     main = "Age",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)
lines(lowess(complete_times, schoen[,3]), col = 3)
dev.off()




# Score residual -----

CR_mod <- coxph(Surv(time, status) ~ karno + trt + age, data = veteran)
score <- residuals(CR_mod, type = "score")

veteran[score[,1] > 100 | 
          score[,1] < -50 | 
          abs(score[,2]) > 2 | 
          abs(score[,3]) > 40, 1:8]

png(paste0(figpath,"score_residuals.png"))

par(mfrow = c(1,3), pty = "s")

plot(x = 1:nrow(veteran), 
     y = score[,1], 
     xlab = "Subject index",
     ylab = "Score residuals",
     main = "Karnofsky score",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)

plot(x = 1:nrow(veteran), 
     y = score[,2], 
     xlab = "Subject index",
     ylab = "Score residuals",
     main = "Treatment",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)

plot(x = 1:nrow(veteran), 
     y = score[,3], 
     xlab = "Subject index",
     ylab = "Score residuals",
     main = "Age",
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)

dev.off()

# PH assumption formal test ----

CR_mod <- coxph(Surv(time, status) ~ karno + trt + age, data = veteran)
cox.zph(CR_mod, transform = "log")

# stratified Cox PH mdoel ----


CR_mod_stratified <- coxph(Surv(time, status) ~ karno + strata(trt) + age, data = veteran)
summary(CR_mod_stratified)


# Compute median age and Karnofsky scores for each stratum:
# Standard Treatment
median(veteran[veteran$trt==1,]$age)
median(veteran[veteran$trt==1,]$karno)

# Test Treatment
median(veteran[veteran$trt==2,]$age)
median(veteran[veteran$trt==2,]$karno)

# Plot the estimated predictor-adjusted survival curves:
pred_vals <- data.frame(karno = 60, age = 62, data = veteran)

fit <- survfit(CR_mod_stratified, newdata = pred_vals)

png(paste0(figpath,"stratified_cox.png"))
plot(fit[1]$time, 
     fit[1]$surv[,1], 
     type = "s",
     lty = 1, 
     xlab = "Days until death",
     ylab = "Est. surv. prob.",
     ylim = c(0, 1),
     cex.lab = 1.5,
     cex.axis = 1.5,
     cex.main = 1.5)

lines(fit[2]$time, 
      fit[2]$surv[,1],
      type = "s",
      lty = 2)


text(325, 0.12,"o", cex = 10, col = "yellow")

legend("topright",
       c("Standard treatment", "Test treatment"),
       lty = 1:2,
       cex = 1.5)
dev.off()
