#Intake experiement 
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Intake Experiment")
intake<-read.csv("standardITr.csv")

                 
#subsetting cycles and diet pairs
#cycle 1
cyc1<-subset(intake,intake$cycle=="1")

#cycle 2
cyc2<-subset(intake,intake$cycle=="2")

#testing for normality (using residuals)
#carb
qqnorm((intake$sum.carb)-mean(intake$sum.carb))
qqline((intake$sum.carb)-mean(intake$sum.carb))
shapiro.test((intake$sum.carb)-mean(intake$sum.carb))
#log transformation
intake$Sum.C.Log<-log(intake$sum.carb)
qqnorm(intake$Sum.C.Log)
qqline(intake$Sum.C.Log)
shapiro.test(intake$Sum.C.Log) #met normality
#protein
qqnorm((intake$sum.prot)-mean(intake$sum.prot))
qqline((intake$sum.prot)-mean(intake$sum.prot))
shapiro.test((intake$sum.prot)-mean(intake$sum.prot)) #met normality 
#log transformation and retrying
intake$Sum.p.Log<-log(intake$sum.prot)
qqnorm((intake$Sum.p.Log)-mean(intake$Sum.p.Log))
qqline((intake$Sum.p.Log)-mean(intake$Sum.p.Log))
shapiro.test(intake$Sum.p.Log) #failed normality

#cycle 1 Mancova
y<-cbind(cyc1$sum.carb, cyc1$sum.prot)
fit<-manova(y~cyc1$diet.pair..7p.35c.A.28p.14c...B.35p.7c.+cyc1$average.mass)
summary(fit, test="Pillai")


#cycle 2 Mancova
y1<-cbind(cyc2$sum.carb, cyc2$sum.prot)
fit1<-manova(y1~cyc2$diet.pair..7p.35c.A.28p.14c...B.35p.7c.+cyc2$average.mass)
summary(fit1, test="Pillai")


summary(lm(cycle1$Carb~cycle1$initial.mass+cycle1$Pair))
