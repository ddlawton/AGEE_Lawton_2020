rm(list=ls())
setwd("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\2017\\Plant Sampling\\figures and updates")
dat<- read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Plant Sampling/point_intercept_bothyears.csv")
head(dat)

#libraries
library("ggplot2")
library("tidyverse")
library(plyr)
library(reshape2)
library(scales)
library(mgcv)
library(mgcViz)
library(DHARMa)
library(ggpubr)
#cleaning up data
str(dat)
dat$Year<-as.factor(dat$Year)
dat$Transect<-as.factor(dat$Transect)
dat$Distance<-as.factor(dat$Distance)

# gathering and counting the number of hits for each ground cover variable and plant species

ground_cover<-
  dat %>%
    group_by(Year,Paddock,Transect,Groundcover) %>%
      dplyr::summarise(length(Groundcover))



ground_coversm <-
ground_cover %>%
  spread(Groundcover,'length(Groundcover)')%>%
  mutate(year.transect=paste(Year,Transect,sep="."))%>%
  group_by(year.transect)%>%
  select(-c(Year,Paddock,Transect))

Species.1<-
  dat %>%
  group_by(Year,Paddock,Transect,Species.1,Functional.Group.1) %>%
  dplyr::summarise(length(Species.1),length(Functional.Group.1))


Species.1sm <-
  Species.1 %>%
  spread(Species.1,'length(Species.1)')%>%
  spread(Functional.Group.1,'length(Functional.Group.1)')%>%
  mutate(year.transect=paste(Year,Transect,sep="."))%>%
  group_by(year.transect)%>%
  select(-c(Year,Paddock,Transect))

Species.2<-
  dat %>%
  group_by(Year,Paddock,Transect,Species.2,Functional.Group.2) %>%
  dplyr::summarise(length(Species.2),length(Functional.Group.2))



Species.2sm <-
  Species.2 %>%
  spread(Species.2,'length(Species.2)')%>%
  spread(Functional.Group.2,'length(Functional.Group.2)')%>%
  mutate(year.transect=paste(Year,Transect,sep="."))%>%
  group_by(year.transect)%>%
  select(-c(Year,Paddock,Transect))


Species.3<-
  dat %>%
  group_by(Year,Paddock,Transect,Species.3,Functional.Group.3) %>%
  dplyr::summarise(length(Species.3),length(Functional.Group.3))

Species.3sm <-
  Species.3 %>%
  spread(Species.3,'length(Species.3)')%>%
  spread(Functional.Group.3,'length(Functional.Group.3)')%>%
  mutate(year.transect=paste(Year,Transect,sep="."))%>%
  group_by(year.transect)%>%
  select(-c(Year,Paddock,Transect))

melted_data <- rbind(melt(Species.1sm, id.vars=ncol(Species.1sm)), 
                     melt(Species.2sm, id.vars=ncol(Species.2sm)), 
                     melt(Species.3sm, id.vars=ncol(Species.3sm)))

#Merging like column names together

merged_plants<-
  melted_data %>%
  mutate(value = ifelse(is.na(value), 0, value))%>%
  group_by(year.transect,variable) %>% 
  dplyr::summarise(value=sum(value)) %>%
  spread(variable,value)
  

#adding locust abundance
locust <- read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/Across Years GLMM/1617_LA.csv")
locust <-
  locust %>%
    select(Year,Transect,ra,Field,location)%>%
    group_by(Year,Transect,Field,location) %>%
    dplyr::summarise(mean.ra=mean(ra),se.ra=((sd(ra))/(sqrt(length(ra)))))%>%
    mutate(year.transect=paste(Year,Transect,sep="."))


#combining all data together
all.dat <- 
  list(locust, ground_coversm, merged_plants) %>%
    reduce(left_join, by = "year.transect") %>%
    replace(is.na(.), 0)

#summing up all variables if needed ...
##all.dat$ground_cover_sum <- rowSums(all.dat[7:11])
##all.dat$species_sum <- rowSums(all.dat[12:(ncol(all.dat)-11)])


# Looking at simple correlations
library(GGally)
ggpairs(all.dat[7:(ncol(all.dat)-6)])
cors <- as.data.frame(cor(all.dat[7:(ncol(all.dat)-6)]))
corrplot::corrplot(cor(as.matrix(as.data.frame(all.dat[7:(ncol(all.dat)-6)]))))





library(FactoMineR)
library(factoextra)


pca <- PCA(all.dat[7:(ncol(all.dat)-6)])
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 20))

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_contrib(pca, choice = "var", axes = 1, top = 30)

fviz_pca_ind(pca,
             label = "none", # hide individual labels
             habillage = all.dat$location, # color by groups
             addEllipses = TRUE # Concentration ellipses
)


all.dat$location <-factor(all.dat$location,levels(all.dat$location)[c(3,2,1,4)])

ggplot(all.dat,aes(x=location,y=mean.ra))+geom_point()

### Model Construction for both years

mod<-glm((mean.ra+1) ~ location + B + L + grass + shrub + tree + as.factor(Field), data=all.dat, family=inverse.gaussian(link=log))
library(DHARMa)
summary(mod)
res<-simulateResiduals(mod, n=500)
testDispersion(res)
testUniformity(res)
plot(res)

step(mod,direction = 'both')
mod2<-glm((mean.ra + 1) ~ location + as.factor(Field), data=all.dat,family = inverse.gaussian(link = inverse))
summary(mod2)
res2<-simulateResiduals(mod, n=500)
plot(res2)
testDispersion(res2)
testUniformity(res2)

mod3<-glm((mean.ra+1) ~ B + L + grass + shrub + tree, data=all.dat, family=inverse.gaussian(link=inverse))
summary(mod3)
res3<-simulateResiduals(mod3, n=500)
testDispersion(res3)
testUniformity(res3)
plot(res3)

step(mod3)


mod4<-glm((mean.ra + 1) ~ grass + tree, family = inverse.gaussian(link = inverse),data=all.dat)
summary(mod4)
res4<-simulateResiduals(mod4, n=500)
testDispersion(res4)
testUniformity(res4)
plot(res4)

library(mgcv)
library(mgcViz)
all.dat$Field<- as.factor(all.dat$Field)

gam1<-gam((mean.ra+1) ~ (location) + s(B,k=3) + s(L,k=3) + s(grass,k=3) + 
    s(shrub,k=3) + s(tree,k=3) + (Field),
    data=all.dat, family=gaussian())
gam.check(gam1)
summary(gam1)


nrow(all.dat)
ggplot(all.dat,aes(x=location,y=mean.ra))+geom_point()


# Model construction for 2016

ggplot(y16,aes(x=location, y=mean.ra))+geom_point()

y16 <- subset(all.dat,all.dat$Year=="2016")

mod_16<-glm((mean.ra+1) ~ location + B + L + grass + shrub + tree + as.factor(Field), data=y16, family=quasipoisson(link=log))
summary(mod_16)
plot(mod_16)
res<-simulateResiduals(mod_16, n=500)
testDispersion(res)
testUniformity(res)
plot(res)
step(mod_16)

mod_16_2 <- glm((mean.ra+1) ~ location + B + L + grass + shrub + tree, data=y16, family=inverse.gaussian(link=log))
library(DHARMa)
summary(mod)
res<-simulateResiduals(mod, n=500)
testDispersion(res)
testUniformity(res)
plot(res)
step(mod_16)

#model construction for 2017

y17 <- subset(all.dat, all.dat$Year=="2017")


y17$Field <- gsub("1","foreman",y17$Field )
y17$Field <- gsub("2","bottom 3",y17$Field )
y17$Field <- gsub("3","backaero",y17$Field )
y17$Field <- gsub("bottom backaero","bottom 3",y17$Field )
y17$location <- gsub("Forest","Treestand",y17$location)
y17$location <- gsub("20 meters","Twenty Meters",y17$location)

y17 <-
  y17 %>%
    group_by(Field,location) %>%
    mutate(field.location=paste(Field,location,sep="."))

########
#formatting temp for GLM
#######
#Day equals 6am-8pm
#Night equals 9pm - 5am
####

temp <- read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/iButton Data/temp.csv")
temp$ï..Date.Time<-as.POSIXct(temp$ï..Date.Time, format="%m/%d/%Y %H:%M")
temp<-na.omit(temp)
temp$hour<-as.POSIXlt(temp$ï..Date.Time)$hour

weather_sum <- temp %>%
  group_by(ï..Date.Time,position,location,hour) %>%
  dplyr::summarize(mn.t=mean(Temperature))

weather_sum$position <- gsub("edge","Edge",weather_sum$position )
weather_sum$position <- gsub("grassland","Grassland",weather_sum$position )
weather_sum$position <- gsub("treestand","Treestand",weather_sum$position )

day <-
  weather_sum %>%
  filter(hour >= 6, hour <= 20) %>%
  group_by(position, location) %>%
  summarise(Day_Temp = mean(mn.t)) %>%
  mutate(field.location=paste(location,position,sep="."))%>%
  ungroup() %>%
  select(c(Day_Temp,field.location))


night <-
  weather_sum %>%
  filter(hour < 6 | hour > 20) %>%
  group_by(position, location) %>%
  summarise(Night_Temp = mean(mn.t)) %>%
  mutate(field.location=paste(location,position,sep="."))%>%
  ungroup() %>%
  select(c(Night_Temp,field.location))



##############################################################
####                   PITFALL                        ########
####                      Sampling                     #######
##############################################################

pit<-read.csv("file:///C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/2017/Pitfall/Pitfall sorting.csv")
pit[is.na(pit)] <- 0


pit$Location <- gsub("Tree stand","Treestand",pit$Location )
pit$Location <- gsub("20 Meters","Twenty Meters",pit$Location )
pit$Location <- gsub("Edge","Edge",pit$Location )
pit$Location <- gsub("Grassland","Grassland",pit$Location )


pit$Field <- gsub("1","foreman",pit$Field )
pit$Field <- gsub("2","bottom 3",pit$Field )
pit$Field <- gsub("3","backaero",pit$Field )
pit$Field <- gsub("bottom backaero","bottom 3",pit$Field )

pit <-
  pit %>%
  select( -c(all.Ants..Formicidae..p,Bull.Ants.p,Meat.Ants.p,other.ants.p)) %>%
  gather(family,amount,5:29) %>%
  subset(substr(family,nchar(family)-1,nchar(family)) %in% (".p")) %>%
  group_by(Field,Location,family) %>%
  dplyr::summarise(average = mean(amount)) %>%
  spread(family,average) %>%
  mutate(field.location=paste(Field,Location,sep=".")) %>%
  ungroup() %>%
  mutate(predator.sum = rowSums(.[3:10])) %>%
  select(-c(Field,Location))


all.dat.17 <- 
  y17 %>%
  left_join(day,by= "field.location") %>%
  left_join(night,by= "field.location") %>%
  left_join(pit,by= "field.location") %>%
  replace(is.na(.), 0)


?glm
mod17_1 <- glm((mean.ra+1) ~location + B + grass + tree + Day_Temp + predator.sum + 
                 as.factor(Field), dat = all.dat.17, family=Gamma(link="log"))
res <- simulateResiduals(mod17_1, n=1000)
plot(res)
summary(mod17_1)
step(mod17_1)

mod17_2<-glm((mean.ra + 1) ~ B + L + grass + Day_Temp + predator.sum + 
               as.factor(Field), dat = all.dat.17, family=inverse.gaussian(link=log))
res <- simulateResiduals(mod17_2, n=500)
plot(res)

mod17_1_gam <- gam((mean.ra+1) ~location + B + grass + tree + Day_Temp + predator.sum + 
                     as.factor(Field), dat = all.dat.17, family=Gamma(link="log"))

summary(mod17_1_gam)
gam.check(mod17_1_gam)


lm_mod <- lm((mean.ra+1) ~location + B + grass + tree + Day_Temp + predator.sum + 
                     as.factor(Field), dat = all.dat.17)

summary(lm_mod)
ggplot(all.dat.17,aes(x=predator.sum,y=mean.ra))+geom_point()+geom_smooth()

res<-simulateResiduals(lm_mod,n=1000)
plot(res)

summary(mod17_2)
step(mod17_2)