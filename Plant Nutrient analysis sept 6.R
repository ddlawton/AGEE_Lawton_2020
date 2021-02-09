########################################
#          Woody Plant Nutrients       #
########################################

rm(list=ls())
library('ggplot2')
library('ggpubr')
setwd("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\figures")

#functions
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


dat <- read.csv("C:\\Users\\Douglas\\Dropbox (ASU)\\Research\\Treestand-Grassland Study\\Data\\2017\\Plant Nutrient Gradient\\Plant_nutrient_2016_2017_Sept52018.csv")
str(dat)
dat$Year <- as.factor(dat$Year)
dat$Field <- as.factor(dat$Field)
dat$Transect <- as.factor(dat$Transect)
grass<-subset(dat,dat$Functional.Group=="Grass")


ggplot(grass, aes(grass$Species,y=""))+geom_col(stat=count)

#testing carb and protein correlation
cor(dat$Carb..,dat$Protein..)
ggplot(dat,aes(x=Carb..,y=Protein..))+geom_point()+geom_smooth(method="lm")

#test normality 
mod <- lm(dat$Carb..~dat$Location)
qqnorm(log(resid(mod)+5))
qqline(log(resid(mod)+5))
shapiro.test(log(resid(mod)+5))

mod2 <- lm(dat$Protein..~dat$Location)
qqnorm(log(resid(mod2)+5))
qqline(log(resid(mod2)+5))
shapiro.test(log(resid(mod2)+5))

#Running Manova
x <- cbind(dat$Carb..,dat$Protein..) #those dependents 
mod<-manova(x ~ dat$Year+dat$Field+dat$Location+dat$Species)
summary(mod)
summary.aov(mod)


carb.uni <- aov(Carb..~Year+Species,data=dat)
summary(carb.uni) 
TukeyHSD(carb.uni)

Protein.uni <- aov(Protein..~Year+Species,data=dat)
summary(Protein.uni) 
TukeyHSD(Protein.uni)


#plotting The results

#Year Differnce
g <- ggplot(dat,aes(x=Protein..,y=Carb.., shape=Year))+geom_point(size=5)+ 
  coord_fixed(xlim=c(0,.4),ylim=c(0,.4))+geom_abline(slope=1)+
  geom_abline(slope=2)+scale_shape_manual(values=c(15,0))+
  xlab("Proportion protein (dry mass)")+ ylab("")+theme_pubr(legend="right")+
  coord_fixed(xlim=c(0,.4),ylim=c(0,.4),expand = c(0, 0))+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18),
        legend.text=element_text(size=18))

ggsave("plant.nut.year.png",plot=g,scale=1,width=6,height=6,units="in",dpi=300)

#Transect Differnce
t <- ggplot(dat,aes(x=Protein..,y=Carb.., shape=Location))+geom_point(size=5)+ 
  coord_fixed(xlim=c(0,.4),ylim=c(0,.4),
     expand = c(0, 0))+geom_abline(slope=1)+geom_abline(slope=2)+
  scale_shape_manual(values=c(15,0,8,13))+
    xlab("Proportion protein (dry mass)")+ylab("Proportion carbohydrate (dry mass)")+
  theme_pubr(legend="right")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18),
        legend.text=element_text(size=18))

ggsave("plant.nut.transect.png",plot=t,scale=1,width=6,height=6,units="in",dpi=300)

