#forming substrate and stratum proportions etc
rm(list=ls())
setwd("C:/Users/ddlaw/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
point<-read.csv("point intercept.csv")
field.1<-subset(point,point$Paddock==1)
F1T1<-subset(field.1,field.1$Transect==1)
F1T2<-subset(field.1,field.1$Transect==2)
F1T3<-subset(field.1,field.1$Transect==3)
F1T4<-subset(field.1,field.1$Transect==4)
field.2<-subset(point,point$Paddock==2)
F2T5<-subset(field.1,field.2$Transect==5)
F2T6<-subset(field.1,field.2$Transect==6)
F2T7<-subset(field.1,field.2$Transect==7)
F2T8<-subset(field.1,field.2$Transect==8)
field.3<-subset(point,point$Paddock==3)
F3T9<-subset(field.1,field.1$Transect==1)
F3T10<-subset(field.1,field.1$Transect==2)
F3T11<-subset(field.1,field.1$Transect==3)
F3T12<-subset(field.1,field.1$Transect==4)

#proportions

