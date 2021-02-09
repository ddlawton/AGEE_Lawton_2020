rm(list=ls())
getwd()
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
substrate<-read.csv("APL Density x Ground cover.csv")

#first test for normality using Kolmogorov-Smirnov test
ks.test(substrate$locust, "pnorm",mean=mean(substrate$locust),sd=sqrt(var(substrate$locust)))
#Locust abundance is sigificantly different (D: 0.2456, p-value: 0.02601) from the normal, therefore we will use the Spearman's rank order correlation

#locustxlitter
cor.test(substrate$litter,substrate$locust,method="s") #not sigificantly correlated (rs=0.1390,p-value: 0.3476)
#locustxBareground
cor.test(substrate$locust,substrate$Bareground,method="s") #not sigificantly correlated (rs=0.1390,p-value: 0.4187)
#locustxCWD
cor.test(substrate$locust,substrate$CorseWoddyDebris,method="s") #sigificantly correlated (rs=-0.3715,p-value: 0.02567)
#locustxCrytpo
cor.test(substrate$locust,substrate$Crytpo,method="s") #not sigificantly correlated (rs=0.0370,p-value: 0.8303)
#locustxDung
cor.test(substrate$locust,substrate$Dung,method="s") #not sigificantly correlated (rs=-0.1065,p-value: 0.5361)
#locustxground
cor.test(substrate$locust,substrate$ground,method="s") # sigificantly correlated (rs=0.5971593,p-value: 0.0001206)
#locustxshrub
cor.test(substrate$locust,substrate$shrub,method="s") #not sigificantly correlated (rs=-0.2357,p-value: 0.1663)
#locustxmid story
cor.test(substrate$locust,substrate$mid.storey,method="s") # sigificantly correlated (rs=-0.412267,p-value: 0.01247)
#locustxsub.canopy
cor.test(substrate$locust,substrate$sub.canopy,method="s") #sigificantly correlated (rs=-0.6245646,p-value: .00004675)
#locustxcanpoy
cor.test(substrate$locust,substrate$canpoy,method="s") #sigificantly correlated (rs=-0.4693,p-value: .003883)
#locustxisc
cor.test(substrate$locust,substrate$isc,method="s") #sigificantly correlated (rs=0.5142,p-value: .001335)



