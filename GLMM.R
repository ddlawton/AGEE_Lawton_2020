#Libraries
rm(list=ls())
library(ggplot2)
library(tidyverse)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(lme4)
library(nlme)
library(arm)
library(car)
library(lmPerm)
library(DataExplorer)
library(glmm)
library(glmmTMB)
library(DHARMa)
library(sjstats)
library(mgcv)
library(mgcViz)
library(itsadug)
library(corrplot)
dat<-read.csv("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\Across Years GLMM\\Combined_dat.csv")
dat$Location <- as.factor(dat$Location)
dat$field <- as.factor(dat$field)
str(dat)
head(dat)
par(mfrow=c(1,1))
#creating correlation matrices
overall <- dat[6:17]
y2016 <- dat %>%
  filter(Year==2016) %>%
  select(6:17)
y2017 <- dat %>%
  filter(Year==2017) %>%
  select(6:11,18:21,"Predator.Abundance",-"Crytpo")

mod1<-cor(overall)
corrplot(mod1,title="overall")

mod2<-cor(y2016)
corrplot(mod2,title="2016")

mod3<-cor(y2017)
corrplot(mod3,title="2017")

?corrplot
?select
ggplot(dat, aes(x=Location,y=locust))+geom_point()+geom_smooth()
mod <- glm((locust+1)~litter + Bareground + Location + field,
           family=gaussian(),
           start=c(1,1,1,1,1,1,1,1),
           data=dat)

?glm
summary(mod)
res<-simulateResiduals(mod,n=500)
plot(res)

mod$coefficients 
plot(mod)
sim<-simulateResiduals(mod,n=500)
plot(sim)
#subsetting years

#both years
#PCA Anlysis determined only litter bareground and dung were important variables.

both_years <- subset(dat, select=c("field","Location","litter","Bareground","Dung","locust"))


m1<-glmmTMB(data=both_years,locust~litter + Bareground + Dung + Location + (1|field),family=nbinom2())
m1<-glmmTMB(data=both_years,locust~litter + Bareground + Dung + Location + (1|field),family=nbinom2())
m1<-glmmTMB(data=both_years,locust~litter + Bareground + Dung + Location + (1|field),family=nbinom2())
m1<-glmmTMB(data=both_years,locust~litter + Bareground + Dung + Location + (1|field),family=nbinom2())

sim<-simulateResiduals(m1,n=250)
plot(sim)
summary(m1)
testDispersion(sim)
testZeroInflation(sim)
test

sjstats::r2(mod)

ggplot(both_years, aes(x=Location,y=Bareground))+geom_point()


# fixed effects variance (manually - not sure how to get
# predict.glmmTMB to do this)
linear.predictor <- model.matrix(m1) %*% fixef(m1)$cond
fixed.var <- var(linear.predictor)
# sum variance of all random effects ***excluding OLRE***
all.ranef.var <- unlist(VarCorr(m1)$cond)
ranef.var <- all.ranef.var[!names(all.ranef.var) %in% "obs"]
# OLRE (additive overdispersion variance)
olre.var <- all.ranef.var["obs"]

# now the observation-level variance
# (distinct from the observation-level random effect [OLRE])
# here this is the variance added by the Poisson distribution
# (aka distribution-specific variance)
 # first we need to estimate lambda
 # the Interface paper recommends calculating lambda as
 # exp(beta0 + total.re.var/2)   -- eqn. 5.8
# beta0 is the intercept from an intercept-only refit of the model
(beta0 <- fixef(update(m1, ~ . - mined))$cond)
lambda <- exp(beta0 + (ranef.var + olre.var)/2)

 # observation-level variance (Table 1)
ol.var <- trigamma(lambda)

 # total variance
total.var <- fixed.var + ranef.var + olre.var + ol.var

# marginal R2glmm
 fixed.var / total.var
# conditional R2glmm
(fixed.var + ranef.var) / total.var


?gam
 litter + Bareground + Dung + Location + (1|field)
 
gam1<-gam(locust~s(litter) + Bareground + Dung + Location + s( field,bs="re"),
    data=dat,family=scat())
summary(gam1)
gam.check(gam1)
b <- getViz(gam1)

o <- plot( sm(b, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
print(plot(b, allTerms = T), pages = 1) # Calls print.plotGam()

qq(b, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))

check(b)
plot(gam1)
?glmmTMB
plot_smooth(gam1,view="litter")
plot_parametric(gam1)

mod <- glmer(locust~litter + Bareground + Dung + Location + (1 | field), 
             mustart=pmax(both_years$locust,1),
             data = both_years, family = gaussian(link = "log"), nAGQ = 100)
summary(mod)
overdisp_fun(mod)
drop1(mod)
demo(glm.vr)

summary(mod)

?glmmTMB


lm_bothyears<-lm(locust~litter + Bareground + Dung + Location + field,
          data=both_years)
summary(lm_bothyears)
m <- simulateResiduals(lm_bothyears,n=5000)
plot(m)

#2016
#PCA Anlysis determined only litter bareground and dung were important variables.
dat2<- subset(dat, dat$Year=="2016")
year16 <- subset(dat2, select=c("field","Location","litter","Bareground","CoarseWoddyDebris","Crytpo", "Dung","locust"))

m1<-glmmTMB(data=year16,locust~ Location + litter + (1|field), ziformula = ~1, family=nbinom2())
fixef(m1)
summary(m1)
sim<-simulateResiduals(m1,n=1000)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)

ggplot(year16,aes(x=litter,y=locust,color=field))+geom_smooth(method="gam")+geom_point()

lm_2016<-lm(locust~litter + Bareground + Dung + Location + field , data=year16)
summary(lm_2016)

mod <- glm((locust+1)~litter + Bareground + Dung + Location +field, 
             mustart=pmax(year16$locust,1),
             data = year16, family = Gamma())


summary(mod)
drop1(mod)
overdisp_fun(mod)

m<-simulateResiduals(mod,n=1000)
plot(m)
?glmer
ggplot(year16,aes(x=Bareground,y=locust))+geom_smooth(method="gam")+geom_point()

gam2<-gam(locust~litter + Bareground + Dung + Location + s( field,bs="re"),
          data=year16,family=scat())
summary(gam2)
gam.check(gam2)
b1 <- getViz(gam2)

o <- plot( sm(b1, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
print(plot(b1, allTerms = T), pages = 1) # Calls print.plotGam()

qq(b1, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))

check(b1)




#2017
#PCA Anlysis determined only litter, bareground, ground day humidity,ground day temp, ground night humidity, 
#and predator abundance being important variables

dat3<- subset(dat, dat$Year=="2017")
year17 <- subset(dat3, select=c("field","Location","litter","Bareground", "ground_day_temp","CoarseWoddyDebris", 
                                "Predator.Abundance",
                               "locust"))
create_report(year17[1:12])


lm_2017<-lm(locust~(litter) + (Bareground) + (ground_day_temp) + 
                     + (Predator.Abundance) + Location + field, data=year17)
summary(lm_2017)
n<-simulateResiduals(lm_2017,n=5000)
plot(n)
plot(lm_2017)
fixef(lm_2017)
plogis(-34)
str(year17)
ggplot(year17,aes(x=Location,y=locust))+geom_point()

sim<-simulateResiduals(lm_2017,n=250)
plot(sim)
?glmmTMB
 summary(lm_2017)



mod <- glmer(locust~litter + Bareground + ground_day_temp + 
               Predator.Abundance + Location + (1 | field), 
             mustart=pmax(year17$locust,1),
             data = year17, family = scat())
summary(mod)
overdisp_fun(mod)


gam3<-gam(locust~s(litter) + Location +(ground_day_temp) + 
            + (Predator.Abundance)+
            s(field,bs="re"),
          data=year17,family=scat())
summary(gam3)
gam.check(gam3)
b2 <- getViz(gam3)

o <- plot( sm(b2, 1) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
print(plot(b2, allTerms = T), pages = 1) # Calls print.plotGam()

qq(b2, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2))

check(b2)

#running both years model
lm<-lm(locust ~ litter, data= dat)
residuals <- resid(lm)
dat$Location <- as.factor(dat$Location)
dat$field <- as.factor(dat$field)
dat$Year <- as.factor(dat$Year)

qqnorm(log(residuals+2))
qqline(log(residuals+2))
shapiro.test(log(residuals+2))

par(mfrow=c(1,1))

qqp(dat$locust)

qqp(dat$locust, "lnorm")

nbinom <- fitdistr((dat$locust+1), "Laplace–Gauss")
qqp(dat$locust, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr((dat$locust+1), "Poisson")
qqp(dat$locust, "pois", poisson$estimate)

gamma <- fitdistr((dat$locust+1), "gamma")
qqp(dat$locust, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

?fitdistr


#data is not normal
length(dat$locust)
factorial(60)


#GLMM
mod_glmer1<-lmer(locust~Bareground + CoarseWoddyDebris + Crytpo + Dung + Location +(1|field) + (1|Year),family=gaussian(),data=dat)
summary(mod_glmer1)
sim<-simulateResiduals(mod_glmer1,n=500)
plot(sim)


?lmp
mod <- lmp(locust~litter+ Bareground + CoarseWoddyDebris + Crytpo + Dung + Location,data=dat, perm="Prob", nCycle = 4000000)
summary(mod)
?lmp

fstat<-summary(lmp(locust~Bareground + CoarseWoddyDebris + Crytpo + Dung + Location,data=dat, perm="Exact"))$fstat[1]




summary(mod)

mod <- lm(locust~litter+ Bareground + CoarseWoddyDebris + Crytpo + Dung + Location,data=dat, nCycle = 4000)
summary(mod)$fstat[1]

nreps<-20000
fsim <-numeric(nreps)
plot(density(fsim))

for(i in 1:nreps){
  fsim[i] <- summary(lm(locust~Bareground + CoarseWoddyDebris + Crytpo + Dung + Location,data=dat))$fstat[1]
}

anova(summary(mod)

