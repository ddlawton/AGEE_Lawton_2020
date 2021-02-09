rm(list=ls())
library(jmv)
dat <- read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Plant Nutrient Gradient/Plant_nutrient_2016_2017_Sept52018.csv")
head(dat)
dat$Field <- as.factor(dat$Field)
dat$Year <- as.factor(dat$Year)
ggplot(dat,aes(x=as.factor(Location),y=prop.protein))+geom_boxplot()
grass <-
  dat%>%
  filter(Functional.Group=="Grass")
dat$Functional.Group

c<-cbind(grass$Carb..,grass$Protein..)
mod<-manova(c~Field+Location*Species*Year,data=grass)
str(dat)


summary(aov(mod))
?manova
x<- c("Carb..","Protein..")
y<- c("Field","Species","Location","Year")
z<- c("tot.macro")
mod2 <- mancova(data=dat,
                deps=x,
                factors=y)
summary.ma(mod2)
?mancova
