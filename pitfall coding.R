##############################################################
####                   PITFALL                        ########
####                      Sampling                     #######
##############################################################
rm(list=ls())
#libraries
library(ggplot2)
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggpubr)
library(lme4)
library(mgcv)
library(rcompanion)
library(FSA)

setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/figures")
pit<-read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Pitfall/Pitfall sorting.csv")
pit[is.na(pit)] <- 0

flevels <- c("Tree stand","Edge","20 Meters","Grassland")
pit$Location <- factor(pit$Location,levels=flevels)


other_preds <-
  pit%>%
  select(-ends_with("o")) %>%
  select(-c(all.Ants..Formicidae..p, Bull.Ants.p, Meat.Ants.p, other.ants.p))%>%
  gather(.,family,amount,5:14)%>%
  group_by(Field,Location,Transect,Sample)%>%
  summarize_at(vars(amount),sum)


ant_preds <-
  pit%>%
  select(-ends_with("o")) %>%
  select(c(Field,Location,Transect,Sample,Meat.Ants.p))%>%
  group_by(Field,Location,Transect,Sample)%>%
  summarize_at(vars(Meat.Ants.p),sum)


other<-ggplot(other_preds,aes(x=Location,y=amount))+geom_boxplot(outlier.size=-1)+
  geom_jitter(width=.3,height=0,size=2)+
  scale_y_continuous(limits=c(0,15),breaks=c(0,5,10,15))+
  theme_pubclean()+theme(axis.text=element_text(size=15), 
                         axis.title=element_text(size=16))+
  xlab("")+ylab("Invertebrate predator abundance")


ggsave("other.predators.png",plot=other,scale=1,width=5,height=5,units="in",dpi=300)

qqnorm(log(other_preds$amount+1))
qqline(log(other_preds$amount+1))
scheirerRayHare(amount~Location*Field,data=other_preds)

summary(ants_mod)

ants<-ggplot(ant_preds,aes(x=Location,y=Meat.Ants.p))+geom_boxplot(outlier.size=-1)+
  geom_jitter(width=.3,height=0,size=2)+
  scale_y_continuous(limits=c(0,900),breaks=c(0,300,600,900))+
  theme_pubclean()+theme(axis.text=element_text(size=15), 
                         axis.title=element_text(size=16))+
  xlab("")+ylab("Ant abundance")


ggsave("ant.predators.png",plot=ants,scale=1,width=5,height=5,units="in",dpi=300)

qqnorm(log(ant_preds$Meat.Ants.p+1))
qqline(log(ant_preds$Meat.Ants.p+1))
scheirerRayHare(Meat.Ants.p~Location*Field,data=ant_preds)

dunnTest(Meat.Ants.p~Field,data=ant_preds,method="bh")



pit_total <-
  pit4 %>%
  dplyr::group_by(Field,Location,Transect,Sample) %>%
  summarize_at(vars(amount),sum)


other <- ggplot(pit_total,aes(x=Location,y=amount))+
  geom_boxplot(outlier.size = -1)+
  geom_jitter(width=.3)+
  ylab("Other predator density")+
  xlab("")+
  theme_pubclean()



pit_total_ants <-
  pit %>%
  group_by(Field,Location,Transect,Sample)%>%
  select(Meat.Ants.p)%>%
  summarize_at(vars(amount),sum)

as.int
pit_total_ants$Field <- as.factor(pit_total_ants$Field)
flevels <- c("Tree stand","Edge","20 Meters","Grassland")
pit_total_ants$Location <-factor(
  pit_total_ants$Location,
  levels=flevels)
?as.factor

qqnorm(pit_total_ants$total)
mod<-kruskal.test(total~Location,data=pit_total_ants)
summary(mod)
ants<- ggplot(pit_total_ants,aes(x=Location,y=total))+
  geom_boxplot(outlier.size = -1)+
  geom_jitter(width=.3)+
  ylab("Meat ant Density")+
  xlab("")+
  theme_pubclean()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())



multiplot(ants,other)



mod1 <- gam(total~Location+(1|Field),family=poisson(),dat=pit_total)
summary(mod1)

?glmer
?glmmTMB
pit2$Field <- as.factor(pit2$Field)

ggplot(pit_total,aes(x=Location,y=total,color=Field))+geom_boxplot(outlier.size = -1)+geom_point(position=position_jitterdodge(jitter.width=.3,jitter.height = 0))



mod<-kruskal.test(total~Location,dat=pit_total)





mod2<-glmmTMB(data=subset(pit2,pit2$family=="Meat.Ants.p"), amount~Location+(1|Field),family=nbinom1())
summary(mod2)
sim<-simulateResiduals(mod,n=250)
plot(sim)
testOverdispersion(sim)
testZeroInflation(sim)

flevels <- c("Tree stand","Edge","20 Meters","Grassland")
pit2$Location <- factor(pit2$Location , levels=flevels)
ggplot(subset(pit2,pit2$family=="Meat.Ants.p"),aes(x=Location,y=amount))+geom_boxplot()+theme_pubclean()+ylab("Amount")+geom_jitter(width=.30,height=.0)



pit2$family
df1 <- pit %>%
  group_by(Field,Location,Transect) %>%
  summarise_all(funs(mean))

mytable <- xtabs(~Field+Location+Sample, data=pit)


pit$Location <- factor(pit$Location, levels=c("Tree stand", "Edge", "20 Meters","Grassland"))

ggplot(pit2,aes(x=Location,y=amount))+geom_boxplot()+geom_jitter(width=.30,height=.0)

?geom_jitter

test<-aov(pit$Meat.Ants.p~pit$Location)
summary(test)
TukeyHSD(test)




test<-pit %>%
  
  #gathering pitfall
  
  gather(family,amount,5:29)

df1 <- test %>%
  group_by(Field,Location,Transect,Sample,family) %>%
  summarise_all(funs(mean))


mod<-lm(amount~Location+as.factor(Field),data=df1)
summary(mod)
?summarise_all 

#removing zeros and leaving just what was found in each sample


df5<- test2 %>%
  group_by(Field,Location,Transect,family) %>%
  summarise_all(funs(mean))

grepl(pattern = "(.p)$", df5$family)

predators<-subset(df1,substr(family,nchar(family)-1,nchar(family)) %in% (".p"))
predators$Field = as.factor(predators$Field)
test4<-subset(predators, select = -c(all.Ants..Formicidae..p,Bull.Ants.p,Meat.Ants.p,other.ants.p))

summary(lm(data=subset(predators,predators$family=="all.Ants..Formicidae..p"), amount~Location+as.factor(Field)))
?glmmTMB



library(glmmTMB)
library(DHARMa)
predators$log <-log(predators$amount+5)
mod<-glmmTMB(data=predators, log~Location+Field)
summary(mod)
sim<-simulateResiduals(mod,n=250)
testResiduals(sim)
?subset

ggplot(test2,aes(x=Location,y=amount))+geom_boxplot()

mod <- aov(data=predators, amount~Location)
summary(mod)

test3<-spread(test2,family,amount)

test4<-subset(test3, select = -c(all.Ants..Formicidae..p,Bull.Ants.p,Meat.Ants.p,other.ants.p))

test5<-gather(test4,family,amount,4:29)
test6<-test5[!rowSums(test5[-c(1:5)] == 'NA') >= 1,]

final<-na.omit(test5)
predators<-subset(final,substr(family,nchar(family)-1,nchar(family)) %in% (".p"))


count(predators$family)

ggplot(predators,aes(x=Location,y=amount,fill=family))+geom_boxplot()

write.csv(final, "predator_abundance.csv")

ggplot(predators,aes(x=Location,y=amount, fill=family))+geom_boxplot()+ scale_fill_discrete(name = "Family",
                                                                                            labels= c("Centipedes","	Opiliones","Lamingtoniidae","Miturgidae","Other Coleoptera","Other Araneae","Pisauridae","Lycosidae"))                                                                                                              
summary(predators)                                                                                                                     

summary(aov(data=predators,amount~Location))      


mod <- lm(data=predators, amount~Location)
summary(mod)






library(vegan)
matrix<- as.matrix.data.frame()
vegdist()

