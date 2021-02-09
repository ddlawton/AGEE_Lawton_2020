#Plant Nutrient Gradients
rm(list=ls())
setwd("C:/Users/ddlaw/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2016/Plant  Nutrients Gradient")
nut<-read.csv("Nutrient.csv")
head(nut)
Grad<-subset(nut,nut$Experiment==1)
#plotting carb and protein
#plotting in ggplot2
library(ggplot2)
Grad$carb..
library(dplyr)
Grad2<-
  tapply(Grad$carb..,Grad$Transect,mean)
head(Grad)
#Nutrient Gradient
P<-ggplot(Grad,aes(prot..,carb..))

P2<-P+geom_point(aes(colour=factor(Location),shape=factor(Field)),size=8)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)", colour="Location",shape="Location")
P4<-P3+scale_color_manual(name="Location",labels=c("Treestand","Edge","20 Meters","grassland"),values=c("#a6cee3","#1f78b4","#b2df8a","#33a02c"))
P5<-P4+scale_shape_manual(name="Field",labels=c("Foremans","Bottom 3","Backaero"),values=c(15,16,17))
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
ggsave("PNG12.png", bg="transparent", width=50, height=50,units="cm")






#forb v grass
P<-ggplot(Grad,aes(prot..,carb..))

P2<-P+geom_point(aes(colour=factor(Functional.Group),shape=factor(Functional.Group)),size=3)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)",title="Fig2. Forbs and grasses nutrients", colour="Functional.Group",shape="Functional.Group")
P4<-P3+scale_color_discrete(name="Functional Group",labels=c("Forb","Grass"))
P5<-P4+scale_shape_discrete(name="Functional Group",labels=c("Forb","Grass"))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))+annotate("text", x=.20, y=.195, angle=45, label="1 carbohydrate:1 protein")
P6+annotate("text", x=.10, y=.195, angle=62, label="2 carbohydrate:1 protein")

#Enteropogon only
library(dplyr)
Grad1<-Grad%>%
  select(Species, carb..,prot..,Location)%>%
  filter(Species == "Enteropogon")

P<-ggplot(Grad1,aes(prot..,carb..))

P2<-P+geom_point(aes(colour=factor(Location),shape=factor(Location)),size=3)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)",title="Fig 3. Enteropogon nutrient gradient", colour="Location",shape="Location")
P4<-P3+scale_color_discrete(name="Location",labels=c("Tree Stand","Edge","twenty Meters","Grassland"))
P5<-P4+scale_shape_discrete(name="Location",labels=c("Tree Stand","Edge","twenty Meters","Grassland"))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))+annotate("text", x=.20, y=.195, angle=48, label="1 carbohydrate:1 protein")
P6+annotate("text", x=.10, y=.195, angle=62, label="2 carbohydrate:1 protein")

#plot by field
P<-ggplot(Grad,aes(prot..,carb..))

P2<-P+geom_point(aes(colour=factor(Field),shape=factor(Field)),size=3)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)",title="Fig 1. Plant Nutrient gradient (forbs and grasses combined)", colour="Field",shape="Field")
P4<-P3+scale_color_discrete(name="Field",labels=c("Foremans","Bottom 3","Back Aero"))
P5<-P4+scale_shape_discrete(name="Location",labels=c("Foremans","Bottom 3","Back Aero"))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))+annotate("text", x=.20, y=.195, angle=45, label="1 carbohydrate:1 protein")
P6+annotate("text", x=.10, y=.195, angle=62, label="2 carbohydrate:1 protein")

#Plotting Bottom 3
bot3<-subset(Grad,Grad$Field==2)
P<-ggplot(bot3,aes(prot..,carb..))

P2<-P+geom_point(aes(colour=factor(Location),shape=factor(Location)),size=3)+xlim(.0,.25)+ylim(0,.25)+coord_equal(ratio = 1)
P3<-P2+labs(x="Protein (Proportion by mass)", y="Carbohydrate (Proportion by mass)",title="Fig 1. Plant Nutrient gradient (forbs and grasses combined)", colour="Location",shape="Location")
P4<-P3+scale_color_discrete(name="Location",labels=c("Tree Stand","Edge","twenty Meters","Grassland"))
P5<-P4+scale_shape_discrete(name="Location",labels=c("Tree Stand","Edge","twenty Meters","Grassland"))
P6<-P5+geom_abline(aes(intercept=0, slope=1))+geom_abline(aes(intercept=0, slope=2))+annotate("text", x=.20, y=.195, angle=45, label="1 carbohydrate:1 protein")
P6+annotate("text", x=.10, y=.195, angle=62, label="2 carbohydrate:1 protein")

ave.carb<-tapply(Grad$carb.ave,Grad$Location,mean)
ave.prot<-tapply(Grad$prot.ave,Grad$Location,mean)

totalave<-cbind(ave.prot,ave.carb) 
test<-tapply(Grad$ratio,Grad$Location,mean)
sd<-tapply(Grad$ratio,Grad$Location,sd)


library(ggplot2)
qplot(Grad$Location,Grad$ratio)+geom_errorbar(aes(x=x, ymin=y-sd, ymax=y+sd))

plot(test)

library(dplyr)
test1 = Grad%>%
  group_by(Field,Location)%>%
  summarise_each(funs(mean),carb.ave,prot.ave)
plot(test1)
Ave.F1<-subset(test1,Field==1)
Ave.F2<-subset(test1,Field==2)
Ave.F3<-subset(test1,Field==3)
plot(carb.ave~prot.ave,data=Ave.F1,xlim=c(0,2000),ylim=c(0,5000))
points(carb.ave~prot.ave,data=Ave.F2,col="#009E73")
points(carb.ave~prot.ave,data=Ave.F3,col="#e79f00")

par(mfrow=c(1,2))
plot(carb.ave~prot.ave,data=Ave.F1,xlim=c(0,2000),ylim=c(0,5000))
points(carb.ave~prot.ave,data=Ave.F2,col="#009E73")
points(carb.ave~prot.ave,data=Ave.F3,col="#e79f00")
plot(tree$prot.ave,tree$carb.ave, ylim=c(0,6000),xlim=c(0,6000),xlab="Protein (ug)",ylab="Carb (ug)", col="#000000" )
points(edge$carb.ave,edge$prot.ave, pch=0, col="#009E73")
points(twenty$carb.ave,twenty$prot.ave, pch=2,col="#e79f00")
points(grass$carb.ave,grass$prot.ave, pch= 5,col="red")
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
points#checking for normality
library(car)
par(mfrow=c(1,1))
#carb
qqnorm(Grad$carb.ave)
qqline(Grad$carb.ave)
shapiro.test(Grad$carb.ave)
#carb is not normal
carb.mean<-mean(Grad$carb.ave)
residuals<-abs(Grad$carb.ave-carb.mean)
qqnorm(residuals)
qqline(residuals)
shapiro.test(log(Grad$ratio))

Grad$log<-log(Grad$carb.ave)
qqnorm(Grad$log)
qqline(Grad$log)
shapiro.test(Grad$log)
#logged carb passed normality

#protein
qqnorm(Grad$prot.ave)
qqline(Grad$prot.ave)
shapiro.test(Grad$prot.ave)
#protein is normal

#running anova
#carb
m5<-lm(Grad$Location~Grad$log)
anova(m5)
#not sigificant

#protein
m6<-lm(Grad$Location~Grad$prot.ave)
anova(m6)
#not sigificant



#running manova
Y<-cbind(Grad$prot.ave,Grad$carb.ave)
fit<-manova(Y~Grad$Location)
summary.aov(fit, test="Pillai")

#running Ancova
m8<-aov(Grad$prot.ave~Grad$carb.ave+Grad$Location)
summary(m8)
#not sigificant


m2<-lm(Grad$Location~Grad$log+Grad$prot.ave+Grad$log*Grad$prot.ave)
anova(m2)
m3<-lm(Grad$Location~log(Grad$ratio))
anova(m3)

