datapath<-"C:/Users/spileggi/Google Drive/STAT 417/Jeffs stuff/Data Sets/"
figpath<-"C:/Users/spileggi/Google Drive/STAT 417/Slides/Figures/"


library(ggplot2)
library(flexsurv)
library(gridExtra)

drink<-read.table(paste0(datapath,"Firstdrink.txt"),header=T)
motorists<-read.table(paste0(datapath,"Aggressive.txt"),header=T)
lung<-read.table(paste0(datapath,"Lung.txt"),header=T)
rearrest<-read.table(paste0(datapath,"Rearrest.txt"),header=T)


#-------------------------------------------------------------------------------
# HW 2 - survival, hazard, and cumulative hazard for age at graduation
# DOES NOT MATCH MINITAB OUTPUT
#-------------------------------------------------------------------------------

#survival
p <- ggplot(data.frame(x = c(0, 16)), aes(x)) +
  stat_function(fun = function(x) {1-pllogis(x,shape=1.6069,scale=1/0.184191)}, aes(colour=("Males"))) +
  stat_function(fun = function(x) {1-pllogis(x,shape=1.5386,scale=1/0.171426)}, aes(colour=("Females"))) +
  scale_x_continuous(name="T", breaks=seq(0,16,by=4)) +
  scale_y_continuous(name="S(t)",breaks=seq(0,1,by=0.2)) +
  labs(colour="Distribution") +
  ggtitle("Log logistic survival function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

p

#hazard
p <- ggplot(data.frame(x = c(0, 16)), aes(x)) +
  stat_function(fun = function(x) {dllogis(x,shape=1.6069,scale=1/0.184191)/(1-pllogis(x,shape=1.6069,scale=1/0.184191))}, aes(colour=("Males"))) +
  stat_function(fun = function(x) {dllogis(x,shape=1.5386,scale=1/0.171426)/(1-pllogis(x,shape=1.5386,scale=1/0.171426))}, aes(colour=("Females"))) +
  scale_x_continuous(name="T", breaks=seq(0,16,by=4)) +
  scale_y_continuous(name="H(t)",breaks=seq(0,0.5,by=0.1)) +
  labs(colour="Distribution") +
  ggtitle("Log logistic hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

p

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 16)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pllogis(x,shape=1.6069,scale=1/0.184191))}, aes(colour=("Males"))) +
  stat_function(fun = function(x) {-log(1-pllogis(x,shape=1.5386,scale=1/0.171426))}, aes(colour=("Females"))) +
  scale_x_continuous(name="T", breaks=seq(0,16,by=4)) +
  scale_y_continuous(name="H(t)") +
  labs(colour="Distribution") +
  ggtitle("Log logistic cumulative hazard function") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

p  





#-------------------------------------------------------------------------------
#Set 2 - hazard and cumulative hazard for arbitrary example - 
#CHANGE gamma parameter
#-------------------------------------------------------------------------------
#hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
      stat_function(fun = function(x) {dgamma(x,shape=2,scale=1)/(1-pgamma(x,shape=2,scale=1))}) +
      scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
      scale_y_continuous(name="h(t)") +
      #labs(colour="Distribution") +
      #ggtitle("Gamma (lambda=1,beta=2)") +
      #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
      theme(axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(2)))

png(paste0(figpath,"haz1.png"))
p  
dev.off()

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pgamma(x,shape=2,scale=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Gamma (lambda=1,beta=2)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"cumhaz1.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {dgamma(x,shape=1,scale=1)/(1-pgamma(x,shape=1,scale=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="h(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Gamma (lambda=1,beta=1)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"haz2.png"))
p  
dev.off()

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pgamma(x,shape=1,scale=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Gamma (lambda=1,beta=1)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"cumhaz2.png"))
p  
dev.off()


#-------------------------------------------------------------------------------
#hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {dgamma(x,shape=0.5,scale=1)/(1-pgamma(x,shape=0.5,scale=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="h(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Gamma (lambda=1,beta=0.5)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"haz3.png"))
p  
dev.off()

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {-log(1-pgamma(x,shape=0.5,scale=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Gamma (lambda=1,beta=0.5)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"cumhaz3.png"))
p  
dev.off()

#-------------------------------------------------------------------------------
#hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {dlnorm(x,meanlog=0,sdlog=1)/(1-plnorm(x,meanlog=0,sdlog=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="h(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Lognormal (mu=0,sigma=1)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"haz4.png"))
p  
dev.off()

#cumulative hazard
p <- ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = function(x) {-log(1-plnorm(x,meanlog=0,sdlog=1))}) +
  scale_x_continuous(name="T", breaks=seq(0,5,by=1)) +
  scale_y_continuous(name="H(t)") +
  #labs(colour="Distribution") +
  #ggtitle("Lognormal (mu=0,sigma=1)") +
  #geom_vline(xintercept=16,color=cols[1],size=1.5) + 
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)))

png(paste0(figpath,"cumhaz4.png"))
p  
dev.off()

