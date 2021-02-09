################################################
###############Tree-Grassland Study#############
#############Douglas Lawton#####################
#################2017###########################
################################################
#library
library(ggplot2)
library(car)
library(ggpubr)
#reading in csv
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Locust Abundance")
df3<-read.csv("1617_LA.csv")
df3$location<-factor(df3$location,levels=c("Forest","Edge","20 meters","Grassland"))


################################################
###############Running Stats####################
################################################

#Testing for Normality

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
df3$Year <- as.factor(df3$Year)
df3$Field <- as.factor(df3$Field)

compare_means(ra ~ location,  data = df3)
my_comparisons <- list(c("Forest","Edge"),c("Forest","20 meters"),c("Forest","Grassland"),c("Edge","20 meters"),c("Edge","Grassland"),c("20 meters","Grassland"))
ggplot(df3,aes(x=location,y=ra))+geom_boxplot()+theme_pubclean()+ylab("Relative abundance")+xlab("Location")+geom_jitter(height=0)+
  annotate("text",x=1,y=4.5,label="A")+annotate("text",x=2,y=4.5,label="A,B")+
  annotate("text",x=3,y=4.5,label="B,C")+annotate("text",x=4,y=4.5,label="C")

subset(df3,df3$location=="20 meters")
?stat_compare_means
##





################################################
###############Graph making#####################
################################################

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

df3$replane<-paste(df3$Sample, df3$Field,sep="_")

#plotting
t<-ggplot(df3,aes(x=as.factor(location),y=ra,group=replane,colour=as.factor(Field)))

t2<-t+geom_line()+geom_point()

t3<-t2+scale_color_manual(values=c("#D55E00","gray55","#CC79A7"),
                          labels=c("Foremans","Bottom 3","Back Aero"))

t4<-t3+geom_col(data=average1, aes(x=levels,y=averages,group=levels), 
                fill="transparent", color= "black",size=1)

t5<-t4+labs(x="Transect Location", y="Locust Relative Abundance",
            title="Locust abundance on the transect gradient \n average shown with bars",
            colour="Field")

t6<-t5+theme(
  axis.text=element_text(size=10, colour="black"),
  title=element_text(size=10),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent",colour=NA),
  legend.text=element_text(size=10),
  plot.title = element_text(vjust=0),
  axis.line=element_line(colour="black",size=1,linetype="solid")
)






