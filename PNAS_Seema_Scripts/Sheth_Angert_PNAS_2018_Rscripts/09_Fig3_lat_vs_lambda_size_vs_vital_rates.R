#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Perform model selection of fixed effects of latitude and elevation on lambda
############# Plot size distribution and size vs. each vital rate for each population
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# Install GLMMADMB package following instructions here: http://glmmadmb.r-forge.r-project.org/
# install.packages("R2admb")
# install.packages("glmmADMB", repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),type="source")

# require packages
require(lme4)
require(glmmADMB)
require(MuMIn)
require(RColorBrewer)
require(ggplot2)
require(plotrix)
require(plyr)
require(dplyr)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. run data_prep.R script to clean up data ###
#*******************************************************************************

source("R_scripts/data_prep.R")

# Variables are: 

# Site: population
# ID: unique identifier for each individual
# Region: latitudinal region that population is nested within
# Latitude: latitude of population
# Longitude: longitude of population
# Elevation: elevation of population
# Class: stage class (juvenile, adult, or NA) of plant at time = t 
# Fec1: Total number of fruits per individual   
# logSize: total stem length of the individual
# ClassNext: stage class (juvenile, adult, dead, or NA) of plant at time = t+1 
# logSizeNext: same as "logSize" above, for t+1
# Surv: survival (1) or not (0) of individuals between time = t and time = t+1
# Year: annual transition of the long-term data at time = t (2010-2013)
# Fec0: Probability of flowering (1 if Class=="A" for adult, 0 if Class=="J" for juvenile)
# RegionRank: ordinal rank of regions from south to north
# SeedCt: mean seed count, rounded to the nearest integer, for each site

#*******************************************************************************
#### 2. Read in vital rate parameters, lambdas, and bootstrapped lambdas for each population ###
#*******************************************************************************

# read in .csv file of vital rate model coefficients
params=read.csv("R_output/vital_rate_coefficients.csv")

# Read in .csv file of lambda estimates for each site
site.lambda=read.csv("R_output/site.lambda.csv")

# Read in .csv file of bootstrapped lambdas for each site; note that these bootstrapped lambdas were created by randomly resampling individuals from each site with replacement, where the size of the sample equals the number of unique individuals present at each site across all years (2010-2013)
boot.lambda=read.csv("R_output/Mcard_demog_INDIV_BOOTSTRAP_lambda_2010-2013.csv") %>% rename(Site=siteID)

# Set up color vector
lat_cols=colorRampPalette(brewer.pal(11,"Spectral"))
data$Col=lat_cols(32)[as.numeric(cut(data$Latitude,breaks = 32))]
site.cols=subset(data,select=c("Site","Col")) %>% unique()

#*******************************************************************************
#### 3. Make Fig. 3a, latitude vs. lambda ###
#*******************************************************************************

#*******************************************************************************
### 3A.Model selection for effects of latitude and elevation on lambda ###
#*******************************************************************************

# Full model with quadratic terms for lat, elev, and lat x elev interaction
m1=lm(lambda~poly(Latitude,2)+poly(Elevation,2)+Latitude:Elevation,data=site.lambda)
summary(m1)

# Full model w/out lat x elev interaction
m2=lm(lambda~poly(Latitude,2)+poly(Elevation,2),data=site.lambda)
summary(m2)

# Full model w/out lat x elev interaction and without quadratic term for elev
m3=lm(lambda~poly(Latitude,2)+Elevation,data=site.lambda)
summary(m3)

# Full model w/out lat x elev interaction and without elev
m4=lm(lambda~poly(Latitude,2),data=site.lambda)
summary(m4)

# Full model w/out lat x elev interaction and without elev and without quadratic term for lat
m5=lm(lambda~Latitude,data=site.lambda)
summary(m5)

# Full model w/out lat x elev interaction and without quadratic term for lat
m6=lm(lambda~Latitude+poly(Elevation,2),data=site.lambda)
summary(m6)

# Full model w/out lat x elev interaction and without lat
m7=lm(lambda~poly(Elevation,2),data=site.lambda)
summary(m7)

# Full model w/out quadratic terms for lat & elev
m8=lm(lambda~Latitude*Elevation,data=site.lambda)
summary(m8)

# Full model w/out quadratic terms for lat & elev and without lat x elev interaction
m9=lm(lambda~Latitude+Elevation,data=site.lambda)
summary(m9)

# Full model w/ only elev 
m10=lm(lambda~Elevation,data=site.lambda)
summary(m10)

# Model selection
AICc(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10) # preferred model is m5

# set up vectors to plot model predictions (for m5 model)
x=seq(min(site.lambda$Latitude),max(site.lambda$Latitude),by=0.01)
lat=data.frame(Latitude=x)
y=coef(m5)[1]+coef(m5)[2]*x

# obtain confidence intervals for slope
CI <- predict(m5, newdata=data.frame(Latitude=x), interval="confidence")
CI <- as.data.frame(CI) # Coerce the matrix to a dataframe, so we can access the column using the $ operator.

# Create polygon to plot confidence intervals for slope
polygon.x <- c(lat$Latitude, rev(lat$Latitude))
polygon.y <- c(CI$lwr, rev(CI$upr))

#*******************************************************************************
### 3B. Obtain 95% bias-corrected confidence intervals for each lambda estimate based on bootstrapping ###
#*******************************************************************************

# Obtain 95% bias-corrected confidence intervals around lambda for each site
lambda.ci.bias=c()

for (i in 1:length(site.lambda$Site)) {
  boot.site=boot.lambda$lambda[boot.lambda$Site==site.lambda$Site[i]]
  obs.lambda=site.lambda$lambda[site.lambda$Site==site.lambda$Site[i]]
  z=qnorm(length(boot.site[boot.site<obs.lambda])/length(boot.site))
  lambda.ci.bias$Site=site.lambda$Site 
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  lambda.ci.bias$lower.ci[i]=quantile(boot.site,probs=lower.ci)
  lambda.ci.bias$upper.ci[i]=quantile(boot.site,probs=upper.ci) 
}

# convert to data frame
lambda.ci.bias=data.frame(lambda.ci.bias)

# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
site.lambda=join(site.lambda,lambda.ci.bias,by="Site")

#*******************************************************************************
### 3C. Plot Fig. 3a, latitude vs. lambda with confidence intervals ###
#*******************************************************************************

# write to .pdf
pdf("Figures/Fig3_size_vital_rates_lat_lambda.pdf",width=8.5,height=11)

# set plotting parameters
par(las=1,bty="l",mfrow=c(3,2),oma = c(5,2,1,3) + 0.1,mar = c(2,4,1,3) + 0.1,xpd=NA,cex.lab=2,cex.axis=1.7)

# plot latitude vs. lambda
plotCI(x=site.lambda$Latitude,y=site.lambda$lambda,li=site.lambda$lower.ci,ui=site.lambda$upper.ci,pch=21,pt.bg="black",xlab="",ylab="")
lines(x,rep(1,length(x)),lwd=2,lty=2)
lines(x,y,lwd=2)
polygon(x=polygon.x, y=polygon.y, col=adjustcolor("black", alpha.f=0.3), border=NA)

# Add title
title(xlab=expression(paste("Latitude (",degree~N,")")),cex.axis=1.7,cex.lab=2,line=3)
title(ylab=expression(paste(italic(lambda))),cex.lab=2,line=4)
# add panel letter
mtext("A",side=3,line=-1.2,adj=-.2)

#*******************************************************************************
#### 4. Plot Fig. 3b, latitude vs. size ###
#*******************************************************************************

# Plot latitude vs. size
boxplot(logSize~Latitude,data=data,col=rev(site.cols$Col),notch=TRUE,ylab="",xlab="",horizontal=TRUE,cex=0.8,xaxt="n",yaxt="n")

# add x-axis ticks
axis(side=1,labels=F) 

# add y-axis
axis(side=2,at=c(3,8,13,18,23,28),labels=c("30","25","20","15","10","5"),las=2,cex.axis=1.7)

# add y-axis title
title(ylab="Population",cex.lab=2,line=4)

# add panel letter
mtext("B",side=3,line=-1.2,adj=-.2)

#*******************************************************************************
#### 5. Plot Fig. 3c, size vs. survival ###
#*******************************************************************************

# plot size vs. site-specific survival
plot(data$logSize,data$Surv,pch=21,bg="grey",col=NA,xlab="",ylab="",xaxt="n")
	 	 
# plot site-specific lines  
for (i in 1:length(params$Site)) {
	data1=subset(data,Site==params$Site[i])
	params1=subset(params,Site==params$Site[i])
    xx=seq(min(data1$logSize[!is.na(data1$Surv)],na.rm=T),max(data1$logSize[!is.na(data1$Surv)],na.rm=T),by=.01) # logSizes at which to evaluate predictions 
 	yy=exp(params1$surv.int+params1$surv.slope*xx)
 	yy=yy/(1+yy)
    #lines(xx,yy,col=site.cols[i])}  
 	  lines(xx,yy,col=unique(data1$Col))}  

# add x-axis ticks
axis(side=1,labels=F) 

# add y-axis title
title(ylab="Prob (Survival)",cex.lab=2,line=4)

# add panel letter
mtext("C",side=3,line=-1.2,adj=-.2)
 	    
#*******************************************************************************
#### 6. Plot Fig. 3d, size vs. growth ###
#*******************************************************************************
  
# plot size vs. site-specific growth
plot(data$logSize,data$logSizeNext,pch=21,bg="grey",col=NA,xlab="",ylab="",xaxt="n")
	 	 
# plot site-specific lines  
for (i in 1:length(params$Site)) {
	data1=subset(data,Site==params$Site[i])
	params1=subset(data.frame(params),Site==params$Site[i])
    xx=seq(min(data1$logSize[!is.na(data1$logSizeNext)],na.rm=T),max(data1$logSize[!is.na(data1$logSizeNext)],na.rm=T),by=.01) # logSizes at which to evaluate predictions 
 	yy=params1$growth.int+params1$growth.slope*xx
    #lines(xx,yy,col=site.cols[i])}  
 	lines(xx,yy,col=unique(data1$Col))}  

# add x-axis ticks
axis(side=1,labels=F) 

# add y-axis title
title(ylab="ln (size at year = t+1)",cex.lab=2,line=4)

# add panel letter
mtext("D",side=3,line=-1.2,adj=-.2)
  
#*******************************************************************************
#### 7. Plot Fig. 3e, size vs.probability of flowering ###
#*******************************************************************************
  
# plot size vs. site-specific flowering probability
plot(data$logSize,data$Fec0,pch=21,bg="grey",col=NA,xlab="",ylab="")
	 	 
# plot site-specific lines  
for (i in 1:length(params$Site)) {
	data1=subset(data,Site==params$Site[i])
	params1=subset(data.frame(params),Site==params$Site[i])
    xx=seq(min(data1$logSize[!is.na(data1$Fec0)],na.rm=T),max(data1$logSize[!is.na(data1$Fec0)],na.rm=T),by=.01) # logSizes at which to evaluate predictions 
 	yy=exp(params1$flowering.int+params1$flowering.slope*xx)
 	yy=yy/(1+yy)
    #lines(xx,yy,col=site.cols[i])}  
 	lines(xx,yy,col=unique(data1$Col))}  

# Add title
title(xlab="ln (size at year = t)",cex=1.2,line=2.5)

# add x-axis ticks
axis(side=1,labels=F) 
 
# add y-axis title
title(ylab="Prob (Flowering)",cex.lab=2,line=4)

# add panel letter
mtext("E",side=3,line=-1.2,adj=-.2) 

#*******************************************************************************
#### 8. Plot Fig.3f, size vs. fruit # ###
#*******************************************************************************
  
# plot size vs. site-specific fruit #
plot(data$logSize,data$Fec1,pch=21,bg="grey",col=NA,xlab="",ylab="")
	 	 
# plot site-specific lines  
for (i in 1:length(params$Site)) {
	data1=subset(data,Site==params$Site[i])
	params1=subset(data.frame(params),Site==params$Site[i])
    xx=seq(min(data1$logSize[!is.na(data1$Fec1)],na.rm=T),max(data1$logSize[!is.na(data1$Fec1)],na.rm=T),by=.01) # logSizes at which to evaluate predictions 
 	yy=exp(params1$fruit.int+params1$fruit.slope*xx)
 	lines(xx,yy,col=unique(data1$Col))}  

# plot a box around data shown in inset
lines(x=c(1,9),y=c(0,0))
lines(x=c(1,9),y=c(800,800))
lines(x=c(1,1),y=c(0,800))
lines(x=c(9,9),y=c(0,800))

# Add title
title(xlab="ln (size at year = t)",cex=1.2,line=2.5)
title(ylab="Fruit #",cex=2,line=5)

# add panel letter
mtext("F",side=3,line=-1.2,adj=-.2)

# plot inset panel zooming into small sizes
par(fig=c(.6, .85, .1, .3), new = TRUE,cex.axis=1.5)
data_sub=subset(data,Fec1<=800&Site!="Little Jameson Creek")
site_sub=params$Site[params$Site!="Little Jameson Creek"]
plot(data_sub$logSize,data_sub$Fec1,pch=21,bg="grey",col=NA,xlab="",ylab="",xlim=c(1,9),ylim=c(0,800),bty="o")
for (i in 1:length(site_sub)) {
  data1=subset(data_sub,Site==site_sub[i])
  params1=subset(data.frame(params),Site==site_sub[i])
  xx=seq(min(data1$logSize[!is.na(data1$Fec1)],na.rm=T),max(data1$logSize[!is.na(data1$Fec1)],na.rm=T),by=.01) # logSizes at which to evaluate predictions 
  yy=exp(params1$fruit.int+params1$fruit.slope*xx)
   lines(xx,yy,col=unique(data1$Col))}  

# write to pdf
dev.off()
