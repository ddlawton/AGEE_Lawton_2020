rm(list=ls())
dat<-read.csv("file:///C:/Users/ddlaw/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/ibutton Data/Bottom 3 temp and humidity.csv")
setwd("C:\\Users\\ddlaw\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\figures")
library(ggplot2)
library(scales)
library(reshape)
library(plyr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(PMCMR)
library(ggpubr)


dat$Date.Time<-as.POSIXct(dat$Date.Time, format="%m/%d/%Y %H:%M")
levels(dat$position)
flevels<-c("Treestand","Edge","20 meters","Grassland")
dat$position <- factor(dat$position,levels=flevels)
dat<-na.omit(dat)
dat$hour<-as.POSIXlt(dat$Date.Time)$hour

qqnorm(dat$Temp)

ggplot(data=dat,aes(x=Temp))+geom_histogram()


mod<-aov(dat$Temp~dat$depth*dat$position)
summary(mod)
TukeyHSD(mod)
str(dat$depth)
ggplot(dat,aes(x=hour,y=Temp,color=depth))+geom_point()+geom_smooth(method='gam',
                                                                    formula=y~s(x))
?bam
?gam
geom_his
mod<-gam(Temp~s(position,bs="re")+depth+s(hour,by=depth,k=23),family=scat(),data=dat,select=TRUE, method="REML")
summary(mod)

plot(mod)

b <- getViz(mod)

check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

dat$hour <- as.integer(dat$hour)

pdat <- with(dat, expand.grid(position = levels(position),
                              depth = levels(depth),
                              hour  = seq(min(hour),max(hour), length = 50)))

pdat <- transform(pdat, pred = predict(mod, newdata = pdat, type = "response"))
str(dat)

ggplot(pdat,aes(x=hour,y=pred,color=position))+geom_point()+geom_smooth(method="gam",formula=y~s(x))

ggplot(pdat,aes(x=hour,y=pred,color=depth))+geom_point()+geom_smooth(method="gam",formula=y~s(x))+
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"))+
  theme_pubclean()+ylab("")+xlab("Time (hour)")+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=18),
        legend.title=element_blank())


ggsave("Position_temp_2016.png",plot=last_plot(),scale=1,width=5,height=5,units="in",dpi=300)






mod2<-bam(Temp~s(position,bs="re")+
            depth+s(hour,by=depth,k=23),data=dat,
          family=scat(link="log"))
summary(mod2)



str(dat)
dat$hour <- as.integer(dat$hour)
pdat2 <- with(dat, expand.grid(position = levels(position),
                               depth = levels(depth),
                               hour  = seq(min(hour),max(hour), length = 50)))
str(dat)
pdat2 <- transform(pdat2, pred = predict(mod, newdata = pdat, type = "response"))
str(dat)

ggplot(pdat2,aes(x=hour,y=pred,color=depth))+geom_point()+geom_smooth(method="gam",formula=y~s(x))+ylab("Predictive temperatures")

