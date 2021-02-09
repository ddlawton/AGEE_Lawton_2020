rm(list=ls())
dat<-read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/ibutton Data/Bottom 3 temp and humidity.csv")
library(ggplot2)
library(scales)
library(reshape)
library(plyr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(PMCMR)


dat$Date.Time
dat$Date.Time<-as.POSIXct(dat$Date.Time, format="%m/%d/%Y %H:%M")
dat<-na.omit(dat)
dat$hour<-as.POSIXlt(dat$Date.Time)$hour

qqnorm(dat$Temp)

mod<-gam(Temp~s(hour,by=position)+
           position+depth+s(hour,by=depth),data=dat,
         family=scat(link="log"))

