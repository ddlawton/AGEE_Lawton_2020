rm(list=ls())
library(mgcv)
library(mgcViz)
library(DHARMa)
library(lme4)
library(ggplot2)
library(nlme)
library(tidyverse)
library(visreg)
library(voxel)
library(rgl)
library(ggpubr)
library(PMCMR)
library(FSA)
library(rcompanion)

setwd("C:\\Users\\ddlaw\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\figures")
dat <- read.csv("file:///C:/Users/ddlaw/Dropbox (ASU)/Research/Treestand-Grassland Study/Data/Across Years GLMM/Combined_dat.csv")
dat$binned_litter <- dat$litter+dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo
dat$adjusted_BG <- 1-dat$binned_litter
dat$field <- as.factor(dat$field)
dat$Location <- as.factor(dat$Location)
cor.test(dat$Plant.Carb,dat$Plant.Protein)
dat$prop.protein <- dat$Plant.Protein / (dat$Plant.Carb+dat$Plant.Protein)
dat$Year <- factor(dat$Year)
str(dat)
mean(dat$litter/(dat$litter+dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo))
std <- function(x) sd(x)/sqrt(length(x))
std(dat$litter/(dat$litter+dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo))

ggplot(dat,aes(x=Location,y=locust))+geom_boxplot()
mod<-aov(locust~Location+field,data=dat)

?scheirerRayHare
mod<-aov(locust~Location*field*Year,data=dat)
summary(mod)
TukeyHSD(mod)
posthoc.kruskal.dunn.test(locust~Location+field+Year,data=dat,method="bh")



mean(dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo/(dat$litter+dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo))
std <- function(x) sd(x)/sqrt(length(x))
std(dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo/(dat$litter+dat$Dung+dat$CoarseWoddyDebris+dat$Crytpo))


library(PMCMR)
posthoc.kruskal.dunn.test(dat$locust,dat$Location)
ggplot(dat,aes(y=locust,x=Location))+geom_boxplot(outlier.size=-1)+
  geom_jitter(width=.2)+
  theme_pubclean()+ylab("Locust abundance")+xlab("")+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=18),
        legend.title=element_blank())+ 
  scale_x_discrete(labels=c("1" = "Tree stand", "2" = "Edge",
                            "3" = "20 Meters","4"="Open grassland"))



ggsave("LA.png",plot=last_plot(),scale=1,width=6,height=6,units="in",dpi=300)


#Overall model

ggplot(dat,aes(y=locust,x=adjusted_BG,color=field))+geom_point()+
  geom_smooth()
ggplot(dat,aes(y=locust,x=as.integer(Location)))+geom_point()+geom_smooth(method="gam")
ggplot(dat,aes(y=locust,x=Plant.Carb))+geom_point()+geom_smooth()
ggplot(dat,aes(y=locust,x=Plant.Protein,color=field))+geom_point()+geom_smooth(method="gam")

#linear model...field needs to be held as random effect

mod <- lm(locust~adjusted_BG+Plant.Protein+Location+field, data=dat)
summary(mod)
?glmer

#linear mixed model
lmer_mod <- lmer(locust~adjusted_BG+Plant.Protein+Location+(1|field), data=dat)
summary(lmer_mod)
anova(lmer_mod)
n<-simulateResiduals(lmer_mod,n=1000)
plot(n)

lme_mod <- lme(locust~adjusted_BG+Plant.Protein+Location, random=~1|field, data=dat)
summary(lme_mod)
anova(lme_mod)
n<-simulateResiduals(lme_mod,n=1000)
plot(n)


#Generalized Linear Mixed Model

glmer_mod <- glmer((locust+1)~adjusted_BG+Plant.Protein+Location+(1|field), 
                   data=dat,family=Gamma(link = "log"))
overdisp_fun(glmer_mod)
plot(glmer_mod)
summary(glmer_mod)
n1<-simulateResiduals(glmer_mod,n=1000)
plot(n1)
drop1(glmer_mod)
dat$field <-as.factor(dat$field)
dat$Year <-as.factor(dat$Year)
str(dat$field)
?gam

#Generalized Additive Modeling (final model)
par(mar=c(2,2,2,2))
par(mar=c(1,1,0,0))
gam_model <- gam((locust+1)~s(adjusted_BG)+(prop.protein)+
                   (Location)+
                   Year+s(field,bs="re"), 
                 data=dat,family=scat(link="log"),
                 select=TRUE, method="REML")

summary(gam_model)
b <- getViz(gam_model)
o <- qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
runif


ggsave("LA.bareground.png",plot=last_plot(),scale=1,width=5,height=5,units="in",dpi=300)


ggplot(pdat,aes(x=prop.protein,y=pred))+
  geom_point(size=1)+
  geom_smooth(method="gam",formula=y~x)+
  ylab("Predictive abundace")+xlab("Plant Protein proportion")+
  scale_y_continuous(limits=c(0,12),breaks=c(0,4,8,12))+
  scale_x_continuous(limits=c(.0,1),breaks=c(0,.25,.50,.75,1))+
  theme_pubclean()+theme(axis.text=element_text(size=18), axis.title=element_text(size=18),
                         legend.text=element_text(size=10),
                         legend.title=element_blank())+ guides(color=guide_legend(override.aes=list(fill="white")))

ggsave("LA.plantprotein.png",plot=last_plot(),scale=1,width=5,height=5,units="in",dpi=300)





plot.gam(gam_model,shift=TRUE,select=1)
ylab("s(bareground)")
xlab="bareground")

?plot.gam


vis.gam(gam_model),view=c("Location","adjusted_BG"))
plotRGL(sm(b, 1), fix = c("z" = 0), residuals = TRUE)
?plot.gam
dat$predict <- predict(gam_model,type="response")
ggplot(dat,aes(x=adjusted_BG,y=predict))+geom_point()+geom_smooth(method="gam")
?plot
?plot.gam

summary(gam_model)
b <- getViz(gam_model)
o <- qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
o 
?plot.mgamViz()
print(plot(b, allTerms = T), pages = 1)
?plot.gam

o <- plot( sm(b, 2) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()+ylim(-2,2)


check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
o <- plot( sm(b, 2) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic() + ylim(-.5,.5)

pl <- plot(b, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL)
print(pl,pages=1)


gg <- plotGAM(gam_model,smooth.cov="adjusted_BG",
              groupCovs = NULL,rawOrFitted = "raw",
              plotCI = T)
?plotGAM
gg+ylab("s(bareground %)")+xlab("bareground %")
?plotGAM

# The output from visreg is a list of the same length as the number of 'x' variables,
#   so we use ldply to pick the objects we want from the each list part and make a dataframe: 
smooths <- ldply(plotdata, function(part)   
  data.frame(Variable = part$meta$x, 
             x=part$fit[[part$meta$x]], 
             smooth=part$fit$visregFit, 
             lower=part$fit$visregLwr, 
             upper=part$fit$visregUpr))

# The ggplot:
ggplot(smooths, aes(x, smooth)) + geom_line() +
  geom_line(aes(y=lower), linetype="dashed") + 
  geom_line(aes(y=upper), linetype="dashed") + 
  facet_grid(. ~ Variable, scales = "free_x")





#2017 model
dat_2017 <-
  dat %>%
  filter(Year==2017)%>%
  select(Year,field,Location,locust,adjusted_BG,Plant.Protein,
         Predator.Abundance,ground_day_temp,ground_night_temp,prop.protein)

ggplot(dat_2017,aes(y=locust,x=adjusted_BG))+geom_point()+
  geom_smooth(method="gam",formula=y ~ s(x,bs="cs"))
ggplot(dat_2017,aes(y=locust,x=as.integer(Location)))+geom_point()+geom_smooth(method="gam")
ggplot(dat_2017,aes(y=locust,x=Plant.Carb))+geom_point()+
  geom_smooth(method="gam",formula=y ~ s(x,bs="cs"))
ggplot(dat_2017,aes(y=locust,x=Plant.Protein))+geom_point()+
  geom_smooth(method="gam",formula=y ~ s(x,bs="cs"))
ggplot(dat_2017,aes(y=locust,x=Predator.Abundance,color=field))+geom_point()+
  geom_smooth(method="gam")
ggplot(dat_2017,aes(y=locust,x=field))+geom_point()+geom_smooth(method="gam")

linear_model <- lm(locust~adjusted_BG+Plant.Protein+
                     Predator.Abundance+Location+field,dat=dat_2017)
summary(linear_model)
dat$prop.protein










gam_model_2017 <- gam((locust+1)~s(adjusted_BG, by=Location)+s(Predator.Abundance, by=Location) +
                        Location + (prop.protein) + s(ground_day_temp, by=Location),select=TRUE,method="REML",
                      dat=dat_2017,family=Gamma())

summary(gam_model_2017)
?gam
plot(gam_model_2017,allTerms = T,shift=TRUE,pages=1)

b <- getViz(gam_model_2017)

check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))



gam_model_2017_2 <- gam((locust)~s(adjusted_BG)+s(Plant.Protein)+(Predator.Abundance)+
                          Location,select=TRUE,method="REML",
                        dat=dat_2017,family=scat())


summary(gam_model_2017_2)
plot.gam(gam_model_2017_2,shift=TRUE,select=1:4)
plot()
b <- getViz(gam_model_2017_2)
o <- qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
o 
print(plot(b, allTerms = T,shift=TRUE), pages = 1)
check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))


glmm_2017_model <- glmer((locust+1)~(adjusted_BG)+(Plant.Protein)+(Predator.Abundance)+
                           Location+(1|field),
                         dat=dat_2017,family=Gamma(link = "identity"))

?glmer
plot(glmm_2017_model)
summary(glmm_2017_model)
drop1(glmm_2017_model)
n<-simulateResiduals(glmm_2017_model,n=10000)
plot(n)


dfs <- dat_2017
dfs[4:9] <- scale(dfs[4:9])
m1_sc <- update(GLMM_model_2017,data=dfs)

summary(m1_sc)
plot(m1_sc)
n<-simulateResiduals(m1_sc,n=1000)
plot(n)
testDispersion(n)
?glmer

summary(m1_sc)
b <- getViz(GLMM_model_2017)
o <- qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
o 
print(plot(b, allTerms = T), pages = 1)
check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

?gam

n<-simulateResiduals(GLMM_model_2017,n=1000)
plot(n)
testDispersion(n)
testResiduals(n)
summary(GLMM_model_2017)

#2016 model
dat_2016 <-
  dat %>%
  filter(Year==2016)

ggplot(dat_2016,aes(y=locust,x=adjusted_BG))+geom_point()+geom_smooth(method="gam")
ggplot(dat_2016,aes(y=locust,x=as.integer(Location)))+geom_point()+geom_smooth(method="gam")
ggplot(dat_2016,aes(y=locust,x=Plant.Carb))+geom_point()+geom_smooth(method="gam")
ggplot(dat_2016,aes(y=locust,x=Plant.Protein))+geom_point()+geom_smooth(method="gam")
ggplot(dat_2016,aes(y=locust,x=field))+geom_point()+geom_smooth(method="gam")

linear_model <- lm(locust~adjusted_BG+Plant.Protein+
                     Location+field,dat=dat_2016)
summary(linear_model)

GLMM_model_2016 <- glmer((locust+1)~(adjusted_BG)+(Plant.Protein)+
                           Location+(1|field),dat=dat_2016,
                         family=gaussian(link="log"))
n<-simulateResiduals(GLMM_model_2016,n=1000)
plot(n)
testDispersion(n)
testResiduals(n)
summary(GLMM_model_2016)


gam_model <- gam((locust+1)~(adjusted_BG)+(Plant.Protein)+
                   Location+s(field,bs="re"), 
                 data=dat_2016,family=scat(link="log"),
                 select=TRUE,method="REML")
plot.gam(gam_model,pages=1)
summary(gam_model)
b <- getViz(gam_model)
o <- qq(b, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
o 
print(plot(b, allTerms = T), pages = 1)
check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))



gam_model_PN <- gam((Plant.Protein)~ Location, 
                    data=dat_2016,family=Gamma(),
                    select=TRUE,method="REML")
summary(gam_model_PN)
d <- getViz(gam_model_PN)
o <- qq(d, rep = 10, method = "simul1", CI = "normal", showReps = TRUE, 
        a.replin = list(alpha = 0.1), discrete = TRUE)
o 
print(plot(d, allTerms = T), pages = 1)
check(d,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
mod<-lm(Plant.Protein~adjusted_BG+
          Location+field,data=dat)
summary(mod)
ggplot(dat,aes(y=Plant.Protein,x=as.integer(Location),color=field))+
  geom_point()+geom_smooth(method="lm")
lme_mod_pn <- lmer((Plant.Protein)~ Location+(1|field),data=dat)
summary(lme_mod_pn)       
