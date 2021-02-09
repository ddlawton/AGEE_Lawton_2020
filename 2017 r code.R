#2017 temp data
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/iButton Data")
temp<-read.csv("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/iButton Data/temp.csv")
temp$ï..Date.Time<-as.POSIXct(temp$ï..Date.Time, format="%m/%d/%Y %H:%M")

?as.POSIXct
temp<-na.omit(temp)
library(ggplot2)
library(scales)
library(reshape)
library(plyr)
library(dplyr)
temp$hour<-as.POSIXlt(temp$ï..Date.Time)$hour
temp1<-group_by(temp,position,hour,location)
weather_sum<-dplyr::summarize(temp1,mn.t=mean(Temperature),mn.h=mean(Humidity))
weather_sum$hour=as.integer(temp1_sum$hour)


########
#formatting for GLMM
#######
#Day equals 6am-8pm
#Night equals 9pm - 5am
####
#2017 data

day <-  filter(weather_sum, hour >= 6, hour <= 20)
day2<- ddply(day, .(position,location),summarize, Temp = mean(mn.t), Humid = mean(mn.h))
time <- rep("day",length(1:12))
day2<- cbind(time,day2)


night <-  filter(weather_sum, hour < 6 | hour > 20)
night2<- ddply(night, .(position,location),summarize, Temp = mean(mn.t), Humid = mean(mn.h) )
time <- rep("night",length(1:12))
night2<- cbind(time,night2)

weather_final_2017 <- rbind(day2,night2)
year <- rep("2017",length(1:24))
weather_final_2017<- cbind(year,weather_final_2017)


#2016 data

temp<-read.csv("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/ibutton Data/bottom 3 temp and humidity.csv")
temp$Date.Time<-as.POSIXct(temp$Date.Time, format="%m/%d/%Y %H:%M")
temp<-na.omit(temp)


temp$hour<-as.POSIXlt(temp$Date.Time)$hour
temp1<-group_by(temp,position,hour,location,depth)
weather_sum<-dplyr::summarize(temp1,mn.t=mean(Temp),mn.h=mean(Humdity))
weather_sum$hour=as.integer(weather_sum$hour)

day <-  filter(weather_sum, hour >= 6, hour <= 20)
day2<- ddply(day, .(position,location,depth),summarize, Temp = mean(mn.t), Humid = mean(mn.h))
time <- rep("day",length(1:11))
day2<- cbind(time,day2)


night <-  filter(weather_sum, hour < 6 | hour > 20)
night2<- ddply(night, .(position,location,depth),summarize, Temp = mean(mn.t), Humid = mean(mn.h) )
time <- rep("night",length(1:11))
night2<- cbind(time,night2)

weather_final_2016 <- rbind(day2,night2)
year <- rep("2016",length(1:22))
weather_final_2016<- cbind(year,weather_final_2016)





#combining both years together

weather_1617 <- rbind.fill(weather_final_2016,weather_final_2017)
write.csv(weather_1617,"weather_1617.csv")


# Multiple plot function
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



#2017 Data
t<-ggplot(temp1_sum,aes(x=hour,y=mn,group=position,color=position))
t2<-t+geom_smooth(size=2)+scale_x_continuous(breaks=seq(0,24,by=2),expand=c(0,0))
t3<-t2+labs(x="Hour", y="Average Daily Temperature (C)", title="2017 Ground Temperature")
t4<-t3+theme(
  axis.text=element_text(colour="black",size=30),title=element_text(size=30),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=35))
  


 ggplot(temp1_sum, aes(x=hour,y=mn, colour=position)) +
  stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", aes(fill=type), alpha=0.3) +
  theme_bw()


humid1_sum<-summarize(temp1,n=n(),mn=mean(Humidity),sd=sd(Humidity))
humid1_sum$se<-(humid1_sum$sd/(sqrt(humid1_sum$n)))
h<-ggplot(humid1_sum,aes(x=hour,y=mn,group=position,color=position))
h2<-h+geom_smooth(size=2)+scale_x_continuous(breaks=seq(0,24,by=2),expand=c(0,0))
h3<-h2+labs(x="Hour", y="Average Daily Relative Humidity", title="2017 Ground Humidity")
h4<-h3+theme(
  axis.text=element_text(colour="black",size=30),title=element_text(size=30),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=35))

multiplot(t3,h3, cols=2)




#2016 Data
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/iButton Data")
temp_16<-read.csv("Bottom 3 temp and humidity.csv")
temp_16$time<-as.POSIXct(temp_16$time, format="%m/%d/%y %H:%M")
temp_16<-na.omit(temp_16)
temp_16$hour<-as.POSIXlt(temp_16$time)$hour
head(temp_16)
temp_16_grouped<-group_by(temp_16,location,position,hour)

above<-subset(temp_16_grouped_sum,temp_16_grouped_sum$position=="above ground")
ground<-subset(temp_16_grouped_sum,temp_16_grouped_sum$position=="ground")
below<-subset(temp_16_grouped_sum,temp_16_grouped_sum$position=="below ground")


#above
a<-ggplot(above,aes(x=hour,y=mn,group=location,color=location))
a.5<-a+geom_smooth(size=2)+labs(x="Hour", y="Average Daily Temperature (C)", title="2016 Above Ground Temperature")
a1<-a.5+theme(
  axis.text=element_text(colour="black",size=30),title=element_text(size=30),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=35))
#ground
g<-ggplot(ground,aes(x=hour,y=mn,group=location,color=location))
g1<-g+geom_smooth(size=2)+labs(x="Hour", y="Average Daily Temperature (C)", title="2016 Ground Temperature")+theme(
  axis.text=element_text(colour="black",size=30),title=element_text(size=30),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=35))
#below ground
b<-ggplot(below,aes(x=hour,y=mn,group=location,color=location))
b1<-b+geom_smooth(size=2)+labs(x="Hour", y="Average Daily Temperature (C)", title="2016 Below Ground Temperature")+theme(
  axis.text=element_text(colour="black",size=30),title=element_text(size=30),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=35))

multiplot(a1,g1,b1, cols=2)



##########
#humidity#
##########



humid_16_grouped_sum<-summarize(temp_16_grouped,n=n(),mn=mean(Humdity),sd=sd(Humdity))



above_h<-subset(humid_16_grouped_sum,humid_16_grouped_sum$position=="above ground")
ground_h<-subset(humid_16_grouped_sum,humid_16_grouped_sum$position=="ground")
below_h<-subset(humid_16_grouped_sum,humid_16_grouped_sum$position=="below ground")


#above
a_h<-ggplot(above_h,aes(x=hour,y=mn,group=location,color=location))
a1_h<-a_h+geom_smooth(size=2)+labs(x="Hour", y="Average Daily Relative Humidity", title="2016 Above Ground Humidity")+theme(
  axis.text=element_text(colour="black",size=25),title=element_text(size=25),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=25))
#ground
g_h<-ggplot(ground_h,aes(x=hour,y=mn,group=location,color=location))
g1_h<-g_h+geom_boxplot(size=2)+labs(x="Hour", y="Average Daily Relative Humidity", title="2016 Ground Humidity")+theme(
  axis.text=element_text(colour="black",size=25),title=element_text(size=25),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=25))
#below ground
b_h<-ggplot(below_h,aes(x=hour,y=mn,group=location,color=location))
b1_h<-b_h+geom_smooth(size=2)+labs(x="Hour", y="Average Daily Relative Humidity", title="2016 Below Ground Humidity")+theme(
  axis.text=element_text(colour="black",size=25),title=element_text(size=25),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=25))

multiplot(a1_h,g1_h,b1_h, cols=2)


qqnorm(log((temp_16$Temp-((mean(temp_16$Temp))))))
qqline(log(temp_16$Temp))



kruskal.test(temp_16$Temp~temp_16$position)

PT = pairwise.wilcox.test(temp_16$Temp, 
                          temp_16$position, 
                          p.adjust.method="none")


kruskal.test(temp_16$Temp~temp_16$location)

PT_1= pairwise.wilcox.test(temp_16$Temp, 
                          temp_16$location, 
                          p.adjust.method="none")

























































#grassland
ag<-ggplot(data=grassland,aes(x=?..Date.Time,y=Temperature,group=as.factor(location),
                                color=as.factor(location)))
ag2<-ag+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Grassland", x="Date")
#treestand
bg<-ggplot(data=treestand,aes(x=?..Date.Time,y=Temperature,group=as.factor(location),
                                color=as.factor(location)))
bg2<-bg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Tree Stand", x="Date")
#Edge
g<-ggplot(data=Edge,aes(x=?..Date.Time,y=Temperature,group=as.factor(location),
                          color=as.factor(location)))
g2<-g+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Edge", x="Date")
#twenty
a<-ggplot(data=twenty,aes(x=?..Date.Time,y=Temperature,group=as.factor(location),
                        color=as.factor(location)))
a2<-a+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="20 meters", x="Date")


multiplot(ag2,bg2,g2,a2, cols=2)

#####humidity###
#grassland
ag<-ggplot(data=grassland,aes(x=?..Date.Time,y=Humidity,group=as.factor(location),
                              color=as.factor/(location)))
ag23<-ag+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Grassland", x="Date")
#treestand
bg<-ggplot(data=treestand,aes(x=?..Date.Time,y=Humidity,group=as.factor(location),
                              color=as.factor(location)))
bg23<-bg+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Tree Stand", x="Date")
#Edge
g<-ggplot(data=Edge,aes(x=?..Date.Time,y=Humidity,group=as.factor(location),
                        color=as.factor(location)))
g23<-g+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="Edge", x="Date")
#twenty
a<-ggplot(data=twenty,aes(x=?..Date.Time,y=Humidity,group=as.factor(location),
                          color=as.factor(location)))
a23<-a+geom_point()+geom_smooth()+scale_x_datetime(breaks = date_breaks("2 days"))+labs(title="20 meters", x="Date")


multiplot(ag23,bg23,g23,a23, cols=2)



###########
Sigificance
##########
install.packages("nortest")
library("nortest")
#Testing assumptions
x.bar<-mean(temp$Temperature)
ra.res<-abs(temp$Temperature-x.bar)
qqnorm(ra.res)
qqline(ra.res)
shapiro.test(ra.res[0:5000]) #not normal, logging residuals

temp$Temperature.Log<-log10(temp$Temperature)
x.bar1<-mean(temp$Temperature.Log)
ra.res1<-abs(temp$Temperature.Log-x.bar1)
qqnorm(ra.res1)
qqline(ra.res1)
shapiro.test(ra.res1)
#even after transformation, the null hypothesis was reject therefore, I will use a Kruskall Wallis Test

kruskal.test(temp$Temperature~temp$position)
library(PMCMR)  
posthoc.kruskal.nemenyi.test(temp$Temperature~temp$position)

n<-c("treestand","edge","20 meters", "grassland")

temp$position<-factor(temp$position,levels=c("treestand","edge","20 meters", "grassland"))
levels(temp$position)


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df<-data_summary(temp, varname="Temperature",groupnames="position")



b<-ggplot(df, aes(x=as.factor(position), y=Temperature))
b2<-b+geom_bar(stat="identity",fill="dark green")+labs(x="", y="Temperature (c)", title="Ground temperature")
b3<-b2+geom_errorbar(aes(ymin=Temperature-sd, ymax=Temperature+sd), width=.2,
              position=position_dodge(.9)) 
b4<-b3+theme(
  axis.text=element_text(colour="black",size=50),title=element_text(size=50),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=50)
  
)
ggsave("2017TEMP.png", bg="transparent", width=50, height=50,units="cm")


#########
#humidity
####################


df1<-data_summary(temp, varname="Humidity",groupnames="position")



b<-ggplot(df1, aes(x=as.factor(position), y=Humidity))
b2<-b+geom_bar(stat="identity",fill="dark green")+labs(x="", y="Relative Humidity", title="Ground Humidity")
b3<-b2+geom_errorbar(aes(ymin=Humidity-sd, ymax=Humidity+sd), width=.2,
                     position=position_dodge(.9)) 
b4<-b3+theme(
  axis.text=element_text(colour="black",size=50),title=element_text(size=50),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=50)
  
)
ggsave("2017HUMID.png", bg="transparent", width=50, height=50,units="cm")
