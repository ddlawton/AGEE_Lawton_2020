#attemping MANCOVA/PCA on Plant sampling and LA
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
data<-read.csv("MANOVA.csv")
log.data<-log(data[,4:15])
dat.pca<-prcomp(data,center=TRUE,scale=TRUE)
dat.pca
print(dat.pca)
plot(dat.pca)
library(ggfortify)
autoplot(prcomp(cbind(log(data$litter),data$Bareground,data$CorseWoddyDebris,data$Crypto,data$Dung,data$ground,data$shrub,data$mid.storey,data$sub.canopy,data$canpoy,data$isc,data$locust),center=TRUE,scale=TRUE),loadings = TRUE,  loadings.label = TRUE)
#attemping MANOVA
rm(list=ls())
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
data<-read.csv("MANOVA.csv")
Y<-cbind(data$litter,data$Bareground,data$CorseWoddyDebris,data$Crypto,data$Dung,data$ground,data$shrub,data$mid.storey,data$sub.canopy,data$canpoy,data$isc)
fit<-manova(Y~data$locust)
summary.aov(fit)

#trying out some fantastic GLMM

#logging ISC
data$Lisc<-log(data$isc)
#resetting Transect #s
data$Transect[data$Transect==5]<-1
data$Transect[data$Transect==6]<-2
data$Transect[data$Transect==7]<-3
data$Transect[data$Transect==8]<-4
data$Transect[data$Transect==9]<-1
data$Transect[data$Transect==10]<-2
data$Transect[data$Transect==11]<-3
data$Transect[data$Transect==12]<-4

#setting up Randoms
data$ffield<-factor(data$field) #try to add random of a sample
#making model
test1<-lmer(log(locust)~isc+Bareground+CorseWoddyDebris+Crytpo+Dung+ground+shrub+mid.storey+sub.canopy+canpoy+(1|ffield)+isc,data=data,family=poisson)
?glmer
summary(test1)
library(MASS)
PQL.test<-glmmPQL(log(locust)~offset(Lisc)+Bareground+CorseWoddyDebris+Crytpo+Dung+ground+shrub+mid.storey+sub.canopy+canpoy,random=~1|ffield,family=binomial,data=data)

DE.PQL<-glmmPQL(Ecervi.01 ~ CLength * fSex,
                random = ~ 1 | fFarm, family = binomial, data = DeerEcervi)
summary(DE.PQL)