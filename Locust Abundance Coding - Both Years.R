##########################################################################################
##########################################################################################
#                           Locust Abundance                                             #
##########################################################################################
##########################################################################################

#libraries
library("ggplot2")
library(dunn.test)
library(tidyverse)
library(ggpubr)
library(PMCMR)
dat <- read.csv("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\2017\\Locust Abundance\\1617_LA.csv")
str(dat)
dat$Year <-as.factor(dat$Year)
dat$Field <-as.factor(dat$Field)
dat$Transect <-as.factor(dat$Transect)

dat %>%
  group_by(location)%>%
  summarise(length(ra))
#testing normality
qqnorm(dat$ra)
qqline(dat$ra)
shapiro.test(dat$ra) #data is not normal

#running Kruskall Wallace
kruskal.test(dat$ra~dat$location) #sigificant
dunn.test(x=dat$ra,g=dat$location)# post hoc test
posthoc.kruskal.nemenyi.test(dat$ra~dat$location)
posthoc.
# Graphing
dat %>%
  mutate(location = fct_relevel(location, "Forest", "Edge", "20 meters", "Grassland")) %>%
  ggplot( aes(x=location, y=ra)) +
  geom_boxplot(outlier.size = -10)+
  geom_jitter(width=.2,height = 0)+
  theme_pubclean()+
  geom_text(x=1,y=4.5, label="A")+
  geom_text(x=2,y=4.5, label="B")+
  geom_text(x=3,y=4.5, label="B,C")+
  geom_text(x=4,y=4.5, label="C")+
  scale_x_discrete(labels=c("Forest"="Tree Stand", "Edge"="Edge","20 meters"="20 meters","Grassland"="Grassland"))+
  ylab("Relative Abundance")+xlab("")


