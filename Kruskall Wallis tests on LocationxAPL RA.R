#ANOVA on Locust Relative Abundance 
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Locust Abundance")
LA<-read.csv("Locust.Abundance.csv")

#separating locust abundance by transect
LA.T<-subset(LA,LA$Location==1) #Treestand
LA.E<-subset(LA,LA$Location==2) #edge
LA.2<-subset(LA,LA$Location==3) #Twenty meters
LA.G<-subset(LA,LA$Location==4) #Grassland

#Testing for Normality at each location
library(car)
par(mfrow=c(1,1))
qqnorm(LA$Abundance)
qqline(LA$Abundance)

#testing equal variance
leveneTest(LA$Abundance~factor(LA$Location))

#since normality was not met, I will log transform and test again
#since 7 of my 36 variables were zero, I will log transforms the residuals
x.bar<-mean(LA$Abundance)
LA$Residuals<-abs(LA$Abundance-x.bar)
LA$Log_Residuals<-log(LA$Residuals)
par(mfrow=c(1,1))
qqnorm(LA$Residuals)  
qqline(LA$Residuals)  
shapiro.test(LA$Log_Residuals)
#even after transformation, the null hypothesis was reject therefore, I will use a Kruskall Wallis Test

#Firstly, finding differences between fields. Dependent: RA of APL, Independent: Field
kruskal.test(LA$Abundance~LA$Field) #Fail to Reject Null @ chi-squared = 5.4732, df = 2, p-value = 0.06479

#since fields do not differ sigificantly, now comparing location (treestand-grassland)
kruskal.test(LA$Abundance~LA$Location) #Reject the null @ chi-squared = 17.377, df = 3, p-value = 0.0005912

#since sigificantly different, using nemenyi test to find the differences
library(PMCMR)  
posthoc.kruskal.nemenyi.test(LA$Abundance~LA$Location)

#plotting the results
boxplot(LA$Abundance~LA$Location, xlab="Transect Location", ylim=c(0,5.5), names=c("Treestand","Edge","20 meters","Grassland"), ylab="Locust Abundace",font.main=3, cex.main=1.2, font.lab=3, col="darkgreen",main="Locust Abundance on the treestand/grassland gradient with sigificance reported via letters")
text(1,.5,"A")
text(2,2.5,"A,B")
text(3,3.5,"B")
text(4,5.3,"B")
library(ggplot2)
?
b<-ggplot(LA,aes(x=factor(Location),y=LA$Abundance))+geom_boxplot(fill="dark green",colour="black",size=2,outlier.size = 10)
b2<-b+scale_x_discrete(breaks=c("1","2","3","4"),labels=c("Treestand","Edge","20 meters","Grassland"),name="Location")+
  scale_y_continuous(name="Locust Abundance",breaks=c(1,2,3,4,5,6),limits=c(0,6))
b3<-b2+ggtitle("2016 Locust abundance by location. \nSigificant shown with letters.")
b4<-b3+annotate("text",x=c(1,2,3,4),y=c(1,3,4,6),label=c("a","ab","b","b"),size=25)
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
ggsave("2016LA.png", bg="transparent", width=50, height=50,units="cm")
