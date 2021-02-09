#substrate and stratum 
install.packages(ggplot2, dependencies = TRUE)
library(ggplot2)
rm(list=ls())
getwd()
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
substrate<-read.csv("APL Density x Ground cover.csv")
sub2<-subset(substrate,substrate$replicate==1)
plot(sub2)
foremans<-subset(sub2,sub2$field==1)
bottom3<-subset(sub2,sub2$field==2)
backaero<-subset(sub2,sub2$field==3)

#plotting Bareground
par(mfrow=c(1,2))
plot(foremans$Bareground,ylim=c(0,0.40), type="o", ylab="Proportion Bareground",xlab="transect",axes=FALSE, ann=TRUE)
  axis(1,las=1,labels=c("Treestand","Edge","Twenty Meters","Grassland"),at=c(1,2,3,4))
  axis(2)
  title("Bareground Proportion for each transect")
  lines(bottom3$Bareground, type="o",col="#009E73")
  lines(backaero$Bareground, type="o",col="#e79f00")
  legend("topleft",legend=c("Field 1","Field 2","Field 3"),col=c("#000000", "#009E73", "#e79f00"), lty=1, cex=2/3,bty="n")

#plotting Litter
plot(foremans$litter,ylim=c(0,1), type="o", ylab="Proportion Litter",xlab="transect",axes=FALSE, ann=TRUE)
  axis(1,las=1,labels=c("Treestand","Edge","Twenty Meters","Grassland"),at=c(1,2,3,4))
  axis(2)
  title("Litter Proportion for each transect")
  lines(bottom3$litter, type="o",col="#009E73")
  lines(backaero$litter, type="o",col="#e79f00")
  legend("bottomleft",legend=c("Field 1","Field 2","Field 3"),col=c("#000000", "#009E73", "#e79f00"), lty=1, cex=2/3,bty="n")
  