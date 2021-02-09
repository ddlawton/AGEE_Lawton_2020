rm(list=ls())
getwd()
setwd("C:/Users/Douglas/Desktop")
LA <- read.csv("locust abundance.csv")
#subsetting LA for each transect and sample.
field_1<-subset(LA,LA$Field=="1")
S4<-subset(field_1,field_1$Sample=="4")
S7<-subset(field_1,field_1$Sample=="7")
S9<-subset(field_1,field_1$Sample=="9")
field_2<-subset(LA,LA$Field=="2")
S2<-subset(field_2,field_2$Sample=="2")
S5<-subset(field_2,field_2$Sample=="5")
S8<-subset(field_2,field_2$Sample=="8")
field_3<-subset(LA,LA$Field=="3")
S1<-subset(field_3,field_3$Sample=="1")
S3<-subset(field_3,field_3$Sample=="3")
S6<-subset(field_3,field_3$Sample=="6")
Forest<-subset(LA,LA$Transect==1)
Edge<-subset(LA,LA$Transect==2)
twenty_meters<-subset(LA,LA$Transect==3)
Grassland<-subset(LA,LA$Transect==4)
forest_average<-mean(Forest$Locust.abundance)
Edge_average<-mean(Edge$Locust.abundance)
twentym_average<-mean(twenty_meters$Locust.abundance)
grassland_average<-mean(Grassland$Locust.abundance)
averages<-c(forest_average,Edge_average,twentym_average,grassland_average)


n<-c("Forest","Edge","20 Meters", "Grassland")
average1<-data.frame(factor(n),averages,row.names=1)


average1$levels<-as.factor(n)
levels(average1$levels)
average1$levels<-factor(average1$levels,levels=c("Forest","Edge","20 Meters","Grassland"))
levels(average1$levels)
#Creating a linegraph of the locust abundance
library(ggplot2)
p<-ggplot(average1, aes(x=levels,y=averages))
p2<-p+geom_col(fill="transparent", color= "black",size=2)+
  geom_point(data=S1, aes(x=Transect,y= Locust.abundance, color="#D55E00"), size=15) +geom_line(data=S1, aes(x=Transect,y= Locust.abundance,color="#D55E00"), size=8)+
  geom_point(data=S2, aes(x=Transect,y= Locust.abundance, color="#D55E00"), size=15)+geom_line(data=S2, aes(x=Transect,y= Locust.abundance, color="#D55E00"), size=8)+
  geom_point(data=S3, aes(x=Transect,y= Locust.abundance, color="#D55E00"), size=15)+geom_line(data=S3, aes(x=Transect,y= Locust.abundance, color="#D55E00"), size=8)+
  geom_point(data=S4, aes(x=Transect,y= Locust.abundance, color="gray55"), size=15)+geom_line(data=S4, aes(x=Transect,y= Locust.abundance, color="gray55"), size=8)+
  geom_point(data=S5, aes(x=Transect,y= Locust.abundance, color="gray55"), size=15)+geom_line(data=S5, aes(x=Transect,y= Locust.abundance, color="gray55"), size=8)+
  geom_point(data=S6, aes(x=Transect,y= Locust.abundance, color="gray55"), size=15)+geom_line(data=S6, aes(x=Transect,y= Locust.abundance, color="gray55"), size=8)+
  geom_point(data=S7, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=15)+geom_line(data=S7, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=8)+
  geom_point(data=S8, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=15)+geom_line(data=S8, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=8)+
  geom_point(data=S9, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=15)+geom_line(data=S9, aes(x=Transect,y= Locust.abundance, color="#CC79A7"), size=8)+
  labs(x="Transect Location", y="Locust Relative Abundance",title="Locust abundance on the transect gradient \n average shown with bars", colour="Fields")

p3<-p2+scale_color_manual(values=c("#D55E00","gray55","#CC79A7"),labels=c("Foremans","Bottom 3","Back Aero"))
p4<-p3+scale_shape_manual(name="Fields",labels=c("Foremans","Bottom 3","Back Aero"),values=c(15,16,17))+
  theme(
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


barplot


barplot(averages,ylim=c(0,8),col=NA,ylab="Relative Abundance",xlab="Transect",names=c("Treestand","Edge","20 meters","Grassland"),main="Locust relative abundance by field on the treestand/grassland gradient.  \n Averages shown with bars.",cex=2/3,cex.lab=2/3, cex.axis=2/3, cex.main=2/3, cex.sub=2/3)
lines(S4$Locust.abundance~S4$Transect,col="#000000",type="o",par(lwd=3))/
lines(S7$Locust.abundance~S7$Transect,col="#000000",type="o")
lines(S9$Locust.abundance~S9$Transect,col="#000000",type="o")
lines(S2$Locust.abundance~S2$Transect,col="#009E73",type="o")
lines(S5$Locust.abundance~S5$Transect,col="#009E73",type="o")
lines(S8$Locust.abundance~S8$Transect,col="#009E73",type="o")
lines(S1$Locust.abundance~S1$Transect,col="#e79f00",type="o")
lines(S3$Locust.abundance~S3$Transect,col="#e79f00",type="o")
lines(S6$Locust.abundance~S6$Transect,col="#e79f00",type="o")
legend("topright",legend=c("Field 1","Field 2","Field 3"),col=c("#000000", "#009E73", "#e79f00"), lty=1, cex=2/3,bty="n")
?par
#creating a boxplot of the locust abundance

boxplot(Forest$Locust.abundance,Edge$Locust.abundance,twenty_meters$Locust.abundance, Grassland$Locust.abundance, xlab="Transect Location", ylim=c(0,5.5), names=c("Treestand","Edge","20 meters","Grassland"), ylab="Locust Abundace",font.main=3, cex.main=1.2, font.lab=3, col="darkgreen",main="Locust Abundance on the treestand/grassland gradient with sigificance reported via letters")
text(1,.5,"A,B,C")
text(2,2.5,"A,D")
text(3,3.5,"B")
text(4,5.3,"C,D")


p1<-p+geom_col(fill="transparent", color= "black",size=2)+
  geom_point(data=S1, aes(x=Transect,y= Locust.abundance ), size=15)+geom_line(data=S1, aes(x=Transect,y= Locust.abundance), size=8)+
  geom_point(data=S2, aes(x=Transect,y= Locust.abundance), size=15)+geom_line(data=S2, aes(x=Transect,y= Locust.abundance), size=8)+
  scale_colour_brewer(palette="Set1")
