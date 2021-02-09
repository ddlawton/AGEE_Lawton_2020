##########################################################################################
##########################################################################################
#                      Woody Plant Nutrient                                              #
##########################################################################################
##########################################################################################
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Plant Nutrient Gradient")
nut<-read.csv("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\2017\\Plant Nutrient Gradient\\Plant_nutrient_2016_2017_Sept52018.csv")
nut$Year <- as.factor(nut$Year)

library(ggplot2)
library("tidyverse")
library('plyr')
library(ggpubr)
P<-ggplot(nut,aes(x=Carb..,y=Protein..,colour=factor(Location),shape=factor(Field)))

P2<-P+geom_point(size=8)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)", colour="Location",shape="Location")
P4<-P3+scale_color_manual(name="Location",labels=c("Treestand","Edge","20 Meters","grassland"),values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"))
P5<-P4+scale_shape_manual(name="Field",labels=c("Foremans","Backaero","Bottom 3"),values=c(15,16,17))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))
P6+
  coord_fixed(ratio = 1, xlim = c(0,.30), ylim = c(0,.30),expand = c(0, 0))+
  scale_x_continuous(breaks=c(0,.1,.2,.3))+
  scale_y_continuous(breaks=c(0,.1,.2,.3))+
  theme(
    axis.text=element_text(size=13, colour="black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key = element_rect(fill="transparent", colour=NA),
    legend.text=element_text(size=10),
    plot.title = element_text(vjust=10)
    
  )
ggsave("PNG17.png", bg="transparent", width=50, height=50,units="cm")

plyr::count(nut$Species,vars="Location")
ggplot(nut,aes(x=Species,y=Protein..))+geom_col()
mod <- aov(nut$Protein..~nut$Species)
TukeyHSD(mod)
#
y <- cbind(nut$Carb..,nut$Protein..)
mod <- manova(y~nut$Location+nut$Field+nut$Year+nut$Functional.Group)
summary(mod)

P<-ggplot(nut,aes(x=Carb..,y=Protein..,shape=factor(Location)))

P2<-P+geom_point(size=8)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)", colour="Year",shape="Location")
P4<-P3+scale_color_manual(name="Year",labels=c("2016","2017"),values=c("#a6cee3","#1f78b4"))
P5<-P4+scale_shape_manual(name="Field",labels=c("Treestand","Edge","20 Meters","grassland"),values=c(49,50,51,52))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))
P6+
  coord_fixed(ratio = 1, xlim = c(0,.30), ylim = c(0,.30),expand = c(0, 0))+
  scale_x_continuous(breaks=c(0,.1,.2,.3))+
  scale_y_continuous(breaks=c(0,.1,.2,.3))+
  theme(
    axis.text=element_text(size=13, colour="black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key = element_rect(fill="transparent", colour=NA),
    legend.text=element_text(size=10),
    plot.title = element_text(vjust=10)
    
  )
summary(mod)
ggplot(nut,aes=)
