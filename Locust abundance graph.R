################################################
###############Tree-Grassland Study#############
#############Douglas Lawton#####################
#################2017###########################
################################################
#library
library(ggplot2)
#reading in csv
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Locust Abundance")
loc<-read.csv("Lab Sorting.csv")
df<-loc[,c("Trial","Sample","Field","Transect","location","Total.APL","Total.OG")]
trans<-read.csv("Transect Walking.csv")
df2<-trans[,c("total")]

#Calculating APL RA
df$sort<-(df$Total.APL)/(df$Total.OG)
df$walk<-df2
df$ra<-df$sort*df$walk
df$location<-factor(df$location)
df3<-as.data.frame(df)
################################################
###############Graph making#####################
################################################

#subsetting df3 for each transect and sample.
field_1<-subset(df3,df3$Field=="1")
S1<-subset(field_1,field_1$Sample=="1")
S4<-subset(field_1,field_1$Sample=="4")
field_2<-subset(df3,df3$Field=="2")
S2<-subset(field_2,field_2$Sample=="2")
S5<-subset(field_2,field_2$Sample=="5")
field_3<-subset(df3,df3$Field=="3")
S3<-subset(field_3,field_3$Sample=="3")
S6<-subset(field_3,field_3$Sample=="6")
Forest<-subset(df3,df3$location=="Forest")
Edge<-subset(df3,df3$location=="Edge")
twenty_meters<-subset(df3,df3$location=="20 meters")
Grassland<-subset(df3,df3$location=="Grassland")
forest_average<-mean(Forest$ra)
Edge_average<-mean(Edge$ra)
twentym_average<-mean(twenty_meters$ra)
grassland_average<-mean(Grassland$ra)
averages<-c(forest_average,Edge_average,twentym_average,grassland_average)

n<-c("Forest","Edge","20 meters", "Grassland")
average1<-data.frame(factor(n),averages,row.names=1)

average1$levels<-as.factor(n)
levels(average1$levels)
average1$levels<-factor(average1$levels,levels=c("Forest","Edge","20 meters","Grassland"))
levels(average1$levels)


levels(df3$location)
df3$location<-factor(df3$location,levels=c("Forest","Edge","20 meters","Grassland"))
df3$replane<-paste(df3$Sample, df3$Field,sep="_")

#plotting
t<-ggplot(df3,aes(x=as.factor(location),y=ra,group=replane,colour=as.factor(Field)))
t2<-t+geom_line(size=8)+geom_point(size=15)
t3<-t2+scale_color_manual(values=c("#D55E00","gray55","#CC79A7"),
                          labels=c("Foremans","Bottom 3","Back Aero"))
t4<-t3+geom_col(data=average1, aes(x=levels,y=averages,group=levels), 
                fill="transparent", color= "black",size=2)
t5<-t4+labs(x="Transect Location", y="Locust Relative Abundance",
             title="Locust abundance on the transect gradient \n average shown with bars 2017",colour="Field")
t6<-t5+ theme(
  axis.text=element_text(size=50, colour="black"),
  title=element_text(size=50),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent",colour=NA),
  legend.text=element_text(size=50),
  plot.title = element_text(vjust=0)
)
ggsave("LA.png", bg="transparent", width=50, height=50,units="cm")



################################################
###############Running Stats####################
################################################

#Testing for Normality
library(car)
par(mfrow=c(1,1))
x.bar<-mean(df3$ra)
ra.res<-abs(df3$ra-x.bar)
qqnorm(ra.res)
qqline(ra.res)
shapiro.test(ra.res) #not normal, logging residuals

#testing equal variance
leveneTest(df3$ra~factor(df3$location)) #not equal


#since normality was not met, I will log transform and test again
df3$ra.log<-log10(df3$ra+1)
x.bar1<-mean(df3$ra.log)
ra.res1<-abs(df3$ra.log-x.bar1)
qqnorm(ra.res1)
qqline(ra.res1)
shapiro.test(ra.res1)
#even after transformation, the null hypothesis was reject therefore, I will use a Kruskall Wallis Test

#Firstly, finding differences between fields. Dependent: RA of APL, Independent: Field
kruskal.test(df3$ra~df3$Field) #Failed to Reject Null @ chi-squared = 2.4168, df = 2, p-value = 0.2987

#since fields do not differ sigificantly, now comparing location (treestand-grassland)
kruskal.test(df3$ra~df3$location) #Reject the null @ chi-squared = 14.985, df = 3, p-value = 0.001829

#since sigificantly different, using nemenyi test to find the differences
library(PMCMR)  
posthoc.kruskal.nemenyi.test(df3$ra~df3$location)

#plotting the results
b<-ggplot(df3,aes(x=factor(location),y=ra))+geom_boxplot(fill="dark green",colour="black",size=2,outlier.size = 10)
b2<-b+scale_x_discrete(name="Location")+
  scale_y_continuous(name="Locust Abundance",breaks=c(1,2,3,4,5,6),limits=c(0,6))
b3<-b2+ggtitle("2017 Locust abundance by location. \nSigificant shown with letters.")
b4<-b3+annotate("text",x=c(1,2,3,4),y=c(1,1.25,1.25,3.5),label=c("a","ab","abc","c"),size=25)
b5<-b4+  theme(
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
ggsave("2017LA.png", bg="transparent", width=50, height=50,units="cm")


