#bargraph for plant host choice test
rm(list=ls())
library(ggplot2)
library(reshape2)
setwd("C:/Users/douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/plant Host Test")
results<-read.csv("hostchoiceresults.csv")

bp<-ggplot(results,aes(x=variable,y=value,fill=result))+geom_col()
bp2<-bp+labs(x="", y="Number of trials", colour="Result")+scale_y_continuous(limit=c(0,18),breaks=c(0,6,12,18))+
  theme(
    axis.text=element_text(size=13, colour="black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key = element_rect(fill="transparent", colour=NA),
    legend.text=element_text(size=10),
    plot.title = element_text(vjust=10))
    



barplot(as.matrix(results), main="Plant Host Choice Test", ylab = "Numbers", beside= TRUE, col=c("#000000", "#009E73", "#e79f00"),
        par(xpd=TRUE), legend("topright",c("Wins", "Losses","Ties"),fill = c("#000000", "#009E73", "#e79f00")))
?legend