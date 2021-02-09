##########################################################################################
##########################################################################################
#                      Point Intercept Method                                            #
##########################################################################################
##########################################################################################
library("tidyverse")
library(plyr)
library(dplyr)
#reading in data
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Plant Sampling")
plant<-read.csv("ASU_Douglas Lawton_Pont incercept2016.csv",stringsAsFactors = FALSE)

?read.csv

#ground cover
Ground.cover<-ddply(plant,c("ï..Year","Paddock","Transect"),function(plant)count(plant$Groundcover))
Ground.cover$pro<-Ground.cover$freq/50


write.csv(Ground.cover,'1617ground_cover.csv')



#Species Dominance
Species.1<-ddply(plant,c("ï..Year","Paddock","Transect"),function(plant)count(plant$Species.1))
Species.2<-ddply(plant,c("ï..Year","Paddock","Transect"),function(plant)count(plant$Species.2))
Species.3<-ddply(plant,c("ï..Year","Paddock","Transect"),function(plant)count(plant$Species.3))
head(Species.3)
?rbind
Species<-rbind.fill(Species.1,Species.2,Species.3)


transect<-ddply(Species,c("ï..Year","Transect"),function(Species)length(Species$x))
?ddply

Species




Species$x[Species$x == ""] <- "none"

ddply(dd, c("dim1","dim2"), function(df)c(mean(df$v1),mean(df$v2),mean(df$v3),sd(df$v1),sd(df$v2),sd(df$v3)))
