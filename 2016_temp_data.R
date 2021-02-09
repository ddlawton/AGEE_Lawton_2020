#ibutton data
library(ggplot2)
library(scales)
library(reshape)
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/ibutton Data")
temp<-read.csv("Bottom 3 temp and humidity.csv")
temp$time<-as.POSIXct(temp$ï..time, format="%m/%d/%y %H:%M")
temp<-na.omit(temp)
grassland<-subset(temp,temp$location=="Grassland")
edge<-subset(temp,temp$location=="Edge")
twenty<-subset(temp,temp$location=="20 meters")
treestand<-subset(temp,temp$location=="Treestand")
#grassland
g<-ggplot(data=grassland,aes(x=time,y=Temp,group=as.factor(position),
                        color=as.factor(position)))
g2<-g+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))
#edge
e<-ggplot(data=edge,aes(x=time,y=Temp,group=as.factor(position),
                             color=as.factor(position)))
e2<-e+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))
#20 meters
tw<-ggplot(data=twenty,aes(x=time,y=Temp,group=as.factor(position),
                              color=as.factor(position)))
tw2<-tw+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))
#treestand
t<-ggplot(data=treestand,aes(x=time,y=Temp,group=as.factor(position),
                             color=as.factor(position)))
t2<-t+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



multiplot(g2,e2,tw2,t2, cols=2)


aboveground<-subset(temp,temp$position=="above ground")
belowground<-subset(temp,temp$position=="below ground")
ground<-subset(temp,temp$position=="ground")
#above ground
ag<-ggplot(data=aboveground,aes(x=time,y=Temp,group=as.factor(location),
                                color=as.factor(location)))
ag2<-ag+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Above Ground")
#below ground
bg<-ggplot(data=belowground,aes(x=time,y=Temp,group=as.factor(location),
                        color=as.factor(location)))
bg2<-bg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Below Ground")
#ground
g<-ggplot(data=ground,aes(x=time,y=Temp,group=as.factor(location),
                           color=as.factor(location)))
g2<-g+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Ground")

multiplot(ag2,bg2,g2, cols=2)

##Humidity
#above ground
hag<-ggplot(data=aboveground,aes(x=time,y=Humdity,group=as.factor(location),
                                color=as.factor(location)))
hag2<-hag+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Above Ground",y="Humidity")
#below ground
hbg<-ggplot(data=belowground,aes(x=time,y=Humdity,group=as.factor(location),
                                color=as.factor(location)))
hbg2<-hbg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Below Ground",y="Humidity")
#ground
hg<-ggplot(data=ground,aes(x=time,y=Humdity,group=as.factor(location),
                          color=as.factor(location)))
hg2<-hg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Ground",y="Humidity")

multiplot(hag2,hbg2,hg2, cols=2)

#taking out negative values
#abovegoround
aboveground2<-subset(aboveground,aboveground$Humdity>=0)

hag<-ggplot(data=aboveground2,aes(x=time,y=Humdity,group=as.factor(location),
                                 color=as.factor(location)))
hag2<-hag+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("4 days"))+labs(title="Above Ground",y="Humidity")

#below ground
belowground2<-subset(belowground,belowground$Humdity>=0)
hbg<-ggplot(data=belowground2,aes(x=time,y=Humdity,group=as.factor(location),
                                 color=as.factor(location)))
hbg2<-hbg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("4 days"))+labs(title="Below Ground",y="Humidity")

#ground
ground2<-subset(ground,belowground$Humdity>0)
hg12<-ggplot(data=ground2,aes(x=time,y=Humdity,group=as.factor(location),
                           color=as.factor(location)))
hg2<-hg12+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("4 days"))+labs(title="Ground",y="Humidity")

multiplot(hag2,hbg2,hg2, cols=2)