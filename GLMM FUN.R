#GLMM
#outliers
rm(list=ls())
setwd("C:/Users/ddlaw/Dropbox (ASU)/Research/Landscape Ecology Research/Data/Plant Sampling")
data<-read.csv("MANOVA.csv")
data$fLocation<-factor(data$Location)
data$ffield<-factor(data$field)
op<-par(mfrow=c(6,2), mar=c(1.5,1.5,1.5,1.5))
dotchart(data$locust, main="Abundance", group=data$flocation)
dotchart(data$litter, main="litter", group=data$flocation)
dotchart(data$Bareground, main="Bareground", group=data$flocation)
dotchart(data$CorseWoddyDebris, main="Corse Woody Debris", group=data$flocation)
dotchart(log10(data$Crytpo), main="Crytpogram", group=data$flocation)
dotchart(data$Dung, main="Dung", group=data$flocation)
dotchart(data$ground, main="Ground", group=data$flocation)
dotchart(data$shrub, main="Shrub", group=data$flocation)
dotchart(data$mid.storey, main="Mid Story", group=data$flocation)
dotchart(data$sub.canopy, main="Sub Canopy", group=data$flocation)
dotchart(data$canpoy, main="Canopy", group=data$flocation)
dotchart(data$isc, main="In Sky Clear", group=data$flocation)
par(op)
#collinearity
z<-c(data$locust,data$litter,data$Bareground,data$CorseWoddyDebris,data$Crytpo,data$Dung,data$ground,data$shrub,data$mid.storey,data$sub.canopy,data$canpoy,data$isc)
colnames(z)<-c("abundance","Litter","Bareground","Corse Woody Debris","Crytpogram","Dung","Ground","Shrub","Mid Story","Subcanopy","Canopy","In sky clear")
pairs(z),lower.panel=panel.smooth,upper.panel=panel.car(),diag.panel = panel.hist))

M1<-lm(formula=locust~litter+Bareground+CorseWoddyDebris+Crytpo+Dung+ground+shrub+mid.storey+sub.canopy+canpoy+isc+fLocation+ffield,data=data)
summary(M1)
drop1(M1,test="F")
anova(M1)
step(M1)
M2<-lm(formula=locust~litter+Bareground+ground+shrub+mid.storey+sub.canopy+canpoy+isc+fLocation+ffield,data=data)
summary(M2)
drop1(M2,test="F")
anova(M2)
step(M2)
M3<-lm(formula=locust~litter+Bareground+shrub+mid.storey+fLocation+ffield,data=data)
summary(M3)
drop1(M3,test="F")
anova(M3)
step(M3)
M4<-lm(formula=locust~litter+Bareground+isc+fLocation+ffield,data=data)
summary(M4)
drop1(M4,test="F")
anova(M4)
step(M4)

data$locust.1<-data$locust+1
library(lme4)
GLMM<-glmer(locust.1~litter+Bareground+ground+shrub+mid.storey+sub.canopy+isc+fLocation+(1|ffield), data=data,family=Gamma(link=log))
,start=list(locust.1=1,litter=1,Bareground=1,ground=1,shrub=1,mid.storey=1,sub.canopy=1,isc=1,fLocation=1)
?glmer
help(family)
summary(GLMM)
drop1(GLMM)
anova(GLMM)

library(MASS)
test1<-glmmPQL(locust.1~litter+Bareground+ground+shrub+mid.storey+sub.canopy+isc+fLocation,random=~1|ffield,family=Gamma(link=lo),data=data)
summary(test1)

library(usdm)
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}

corvif(data[,5:16])
df=data.frame(data[,5:16])
corvif(df)
