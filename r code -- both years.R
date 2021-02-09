##########################################################################################
##########################################################################################
#                      Ant and Locust Abundance                                          #
##########################################################################################
##########################################################################################
setwd("C:/Users/Douglas/Dropbox (ASU)/Research/Treestand-Grassland Study/antz")
ant<-read.csv("ant hills.csv")
#testing assumptions of normality adn equal variances
#Testing for Normality
library(car)
x.bar<-mean(ant$nymph.total)
ra.res<-abs(ant$nymph.total-x.bar)
qqnorm(ra.res)
qqline(ra.res)
shapiro.test(ra.res) #not normal, logging residuals

ant$nymph.total.log<-log10(ant$nymph.total+1)
x.bar1<-mean(ant$nymph.total.log)
ra.res1<-abs(ant$nymph.total.log-x.bar1)
qqnorm(ra.res1)
qqline(ra.res1)
shapiro.test(ra.res1)
#even after transformation, the null hypothesis was reject therefore, I will use a Kruskall Wallis Test

kruskal.test(ant$nymph.total~ant$date) #not sigificant (X^2: 0.68536, P: 0.7099)
library(PMCMR)  
posthoc.kruskal.nemenyi.test(ant$nymph.total~ant$Direction)
#box plots

library(ggplot2)
b<-ggplot(ant, aes(x=Distance, y=nymph.total, group=Distance))
b2<-b+geom_boxplot(fill="dark green",colour="black",size=2,outlier.size = 10)+labs(x="Distance from ant hill (m)", y="Total Nymphs",
                                             title="Total nymphs collected near ant hills")

b3<-b2+theme(
  axis.text=element_text(colour="black",size=50),title=element_text(size=50),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.background = element_rect(fill="transparent", colour=NA),
  legend.key = element_rect(fill="transparent", colour=NA),
  axis.line.x=element_line(colour="black",size=2),
  axis.line.y=element_line(colour="black",size=2),
  legend.text=element_text(size=50)
  
)
ggsave("2017ANTS.png", bg="transparent", width=50, height=50,units="cm")



#looking at each recorder individually
#Marion
ant.m<-subset(ant,ant$Recorder=="Marion")
kruskal.test(ant.m$nymph.total~ant.m$Distance) #not sigificant x^2 0.36097 p 0.8349
#Douglas
ant.d<-subset(ant,ant$Recorder=="Douglas")
kruskal.test(ant.d$nymph.total~ant.d$Distance) #not sigificant x^2 0.29435 p 0.8631
#box plotting it out
r<-ggplot(ant, aes(x=date, y=nymph.total, group=Directio))+geom_boxplot()


