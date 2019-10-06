#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Obtain vital rate contributions to variation in lambda and perform permutation test to test for demographic compensation
############# Based on method described in Villellas et al. 2015, Ecology Letters
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171121

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(plyr)
require(dplyr)
require(picante)
require(RColorBrewer)
require(MuMIn)

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
#### 2. Bring in vital rate coefficients and bootstrapped vital rate contributions ###
#*******************************************************************************

# read in .csv file of vital rate model coefficients
params=read.csv("R_output/vital_rate_coefficients.csv")

# read in .csv file of site info and lambda estimates
site.lambda=read.csv("R_output/site.lambda.csv")

# read in .csv file of bootstrapped vital rate contributions, produced by running "13_LTRE_demog_comp_bootstrap.R" script
LTRE.boot=read.csv("R_output/Mcard_demog_LTRE_BOOTSTRAP.csv")

#*******************************************************************************
### 3. Create reference IPM parameterized by coefficients averaged across all sites without any perturbations
#*******************************************************************************

#  Compute mean parameter estimates across all sites 
# Note that the "2" in the middle slot refers to 2nd dimension, columns, rather than rows
params_mean=apply(params[,2:14],2,mean) %>% t() %>% data.frame()
params_mean$seed.ct=round(params_mean$seed.ct,digits=0)

# Rename parameters and data for use in IPM script
params1=params_mean
data1=data

# Run script to create survival, growth, and fecundity functions and build reference IPM
source("R_scripts/integral_projection_model.R")

# obtain lambda estimate for reference matrix
(lambda.ref <- Re(eigen(K)$values[1]))

#*******************************************************************************
### 4. Calculate sensitivity of each vital rate coefficient in reference IPM (built from coefficients averaged across sites)
#*******************************************************************************

#*******************************************************************************
### 4A. Set up empty objects to be filled in loop and initiate for loop to loop through perturbing each of 13 coefficients one at a time
#*******************************************************************************

# set value for delta, amount that will be added to each coefficient
delta=0.01

# create empty vectors corresponding to which coefficient was perturbed, its corresponding lambda estimate, perturbed parameters, and sensitivity
sensParam=c()
lambda.perturbed=c()
sensitivity=c()
params_mean.perturbed=c()

# initiate loop to perturb g'th parameter  
for (g in 1:length(params_mean)) {

	# save parameters to a new object in which the gth element will be perturbed
	params_mean.perturbed=params_mean
    	
  # replace original coefficient g with coefficient + delta
  params_mean.perturbed[g]=params_mean[g] + delta
    	
  # store name of coefficient being perturbed
  sensParam[g] <- names(params_mean[g])
     	  
#*******************************************************************************
### 4B. Build IPMs using the gth perturbed coefficient and calculate sensitivities
#*******************************************************************************

# Rename parameters and data for use in IPM script
params1=params_mean.perturbed
data1=data
  
# Run script to create survival, growth, and fecundity functions and build reference IPM
source("R_scripts/integral_projection_model.R")
  
# obtain lambda estimate
(lambda.perturbed[g] <- Re(eigen(K)$values[1]))
    
# obtain estimate of sensitivity, calculated as (lam.perturbed-lambda)/delta, for the gth perturbed coefficient 
sensitivity[g]=(lambda.perturbed[g]-lambda.ref)/delta
	} # end loop across all coefficients of a given site
  
# make a data frame of sensitivities and their respective perturbed parameters
sensitivity.ref=data.frame(sensParam,sensitivity,lambda.perturbed)

# write sensitivities of reference matrix to .csv
write.csv(sensitivity.ref,"R_output/sensitivity_ref_matrix.csv",row.names=FALSE)

#*******************************************************************************
### 5. Calculate site-specific contributions of each vital rate parameter to differences in lambda
#*******************************************************************************

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(data$Site)

# Create empty objects to be filled in loop
LTRE=data.frame()

# For each site, calculate LTRE contribution of each vital rate parameter by looping through parameters of each site

for (i in 1:length(site)) {
	params1=subset(params,Site==site[i],select=-Site) # select parameters for site f and remove Site column
	params_diff=params1-params_mean # calculate difference between parameters of site f and mean parameters
	LTRE.site=params_diff*sensitivity.ref$sensitivity
	LTRE.site$Site=site[i]
	LTRE=rbind(LTRE,LTRE.site)
}

# Summarize contributions by vital rates

# survival contribution
LTRE$LTRE_surv=LTRE$surv.int+LTRE$surv.slope

# growth contribution
LTRE$LTRE_growth=LTRE$growth.int+LTRE$growth.slope+LTRE$growth.sd

# flowering probability contribution
LTRE$LTRE_flowering=LTRE$flowering.int+LTRE$flowering.slope

# fruit # contribution; could add seed count to this since it's not really site-specific
LTRE$LTRE_fruit=LTRE$fruit.int+LTRE$fruit.slope

# offspring size distribution contribution
LTRE$LTRE_recruits=LTRE$recruit.logSize.mean+LTRE$recruit.logSize.sd

# merge table with site info
LTRE=join(LTRE,site.lambda)

#*******************************************************************************
### 6. Plot LTRE vs. latitude and perform statistical tests of relationships
#*******************************************************************************

#*******************************************************************************
### 6A. # Latitude vs. vital rate contributions
#*******************************************************************************

# Write function to obtain 0.025 and 0.975 quantiles 
ci=function(x) {
  quantile(x,probs=c(0.025,0.975))
}

# Obtain 95% confidence intervals around LTRE contributions for each site (NOTE: NOT BIAS-CORRECTED!)
surv.ci=summarise(group_by(LTRE.boot,Site), surv.lower.ci = quantile(LTRE_surv,probs=0.025), surv.upper.ci=quantile(LTRE_surv,probs=0.975)) %>% data.frame() 
growth.ci=summarise(group_by(LTRE.boot,Site), growth.lower.ci = quantile(LTRE_growth,probs=0.025), growth.upper.ci=quantile(LTRE_growth,probs=0.975)) %>% data.frame() 
flowering.ci=summarise(group_by(LTRE.boot,Site), flowering.lower.ci = quantile(LTRE_flowering,probs=0.025), flowering.upper.ci=quantile(LTRE_flowering,probs=0.975)) %>% data.frame() 
fruit.ci=summarise(group_by(LTRE.boot,Site), fruit.lower.ci = quantile(LTRE_fruit,probs=0.025), fruit.upper.ci=quantile(LTRE_fruit,probs=0.975)) %>% data.frame() 
estab.ci=summarise(group_by(LTRE.boot,Site), estab.lower.ci = quantile(establishment.prob,probs=0.025), estab.upper.ci=quantile(establishment.prob,probs=0.975)) %>% data.frame() 
recruit.ci=summarise(group_by(LTRE.boot,Site), recruit.lower.ci = quantile(LTRE_recruits,probs=0.025), recruit.upper.ci=quantile(LTRE_recruits,probs=0.975)) %>% data.frame() 
LTRE_ci=data.frame(surv.ci,growth.ci[,2:3],flowering.ci[,2:3],fruit.ci[,2:3],estab.ci[,2:3],recruit.ci[,2:3])
# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
LTRE=join(LTRE,LTRE_ci,by="Site")

# Obtain 95% bias-corrected confidence intervals around survival contribution for each site
surv.ci.bias=c()

for (i in 1:length(site)) {
  boot.surv=LTRE.boot$LTRE_surv[LTRE.boot$Site==site[i]]
  obs.surv=LTRE$LTRE_surv[LTRE$Site==site[i]]
  z=qnorm(length(boot.surv[boot.surv<obs.surv])/length(boot.surv))
  surv.ci.bias$Site[i]=as.character(site[i]) 
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  surv.ci.bias$surv.lower.ci[i]=quantile(boot.surv,probs=lower.ci)
  surv.ci.bias$surv.upper.ci[i]=quantile(boot.surv,probs=upper.ci)
  }

surv.ci.bias=data.frame(surv.ci.bias)

# Obtain 95% bias-corrected confidence intervals around growth contribution for each site
growth.ci.bias=c()

for (i in 1:length(site)) {
  boot.growth=LTRE.boot$LTRE_growth[LTRE.boot$Site==site[i]]
  obs.growth=LTRE$LTRE_growth[LTRE$Site==site[i]]
  z=qnorm(length(boot.growth[boot.growth<obs.growth])/length(boot.growth))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  growth.ci.bias$growth.lower.ci[i]=quantile(boot.growth,probs=lower.ci)
  growth.ci.bias$growth.upper.ci[i]=quantile(boot.growth,probs=upper.ci)
}

growth.ci.bias=data.frame(growth.ci.bias)

# Obtain 95% bias-corrected confidence intervals around flowering contribution for each site
flowering.ci.bias=c()

for (i in 1:length(site)) {
  boot.flowering=LTRE.boot$LTRE_flowering[LTRE.boot$Site==site[i]]
  obs.flowering=LTRE$LTRE_flowering[LTRE$Site==site[i]]
  z=qnorm(length(boot.flowering[boot.flowering<obs.flowering])/length(boot.flowering))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  flowering.ci.bias$flowering.lower.ci[i]=quantile(boot.flowering,probs=lower.ci)
  flowering.ci.bias$flowering.upper.ci[i]=quantile(boot.flowering,probs=upper.ci)
}

flowering.ci.bias=data.frame(flowering.ci.bias)

# Obtain 95% bias-corrected confidence intervals around fruit # contribution for each site
fruit.ci.bias=c()

for (i in 1:length(site)) {
  boot.fruit=LTRE.boot$LTRE_fruit[LTRE.boot$Site==site[i]]
  obs.fruit=LTRE$LTRE_fruit[LTRE$Site==site[i]]
  z=qnorm(length(boot.fruit[boot.fruit<obs.fruit])/length(boot.fruit))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  fruit.ci.bias$fruit.lower.ci[i]=quantile(boot.fruit,probs=lower.ci)
  fruit.ci.bias$fruit.upper.ci[i]=quantile(boot.fruit,probs=upper.ci)
}

fruit.ci.bias=data.frame(fruit.ci.bias)

# Obtain 95% bias-corrected confidence intervals around establishment probability contribution for each site
estab.ci.bias=c()

for (i in 1:length(site)) {
  boot.estab=LTRE.boot$establishment.prob[LTRE.boot$Site==site[i]]
  obs.estab=LTRE$establishment.prob[LTRE$Site==site[i]]
  z=qnorm(length(boot.estab[boot.estab<obs.estab])/length(boot.estab))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  estab.ci.bias$estab.lower.ci[i]=quantile(boot.estab,probs=lower.ci)
  estab.ci.bias$estab.upper.ci[i]=quantile(boot.estab,probs=upper.ci)
}

estab.ci.bias=data.frame(estab.ci.bias)

# Obtain 95% bias-corrected confidence intervals around offspring size contribution for each site
recruit.ci.bias=c()

for (i in 1:length(site)) {
  boot.recruit=LTRE.boot$LTRE_recruits[LTRE.boot$Site==site[i]]
  obs.recruit=LTRE$LTRE_recruits[LTRE$Site==site[i]]
  z=qnorm(length(boot.recruit[boot.recruit<obs.recruit])/length(boot.recruit))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  recruit.ci.bias$recruit.lower.ci[i]=quantile(boot.recruit,probs=lower.ci)
  recruit.ci.bias$recruit.upper.ci[i]=quantile(boot.recruit,probs=upper.ci)
}

recruit.ci.bias=data.frame(recruit.ci.bias)

LTRE_ci=data.frame(surv.ci.bias,growth.ci.bias,flowering.ci.bias,fruit.ci.bias,estab.ci.bias,recruit.ci.bias)

# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
LTRE=join(LTRE,LTRE_ci,by="Site")

#*******************************************************************************
### 6B. # Latitude vs. vital rate contributions
#*******************************************************************************

# Create a vector of evenly spaced latitude 
lat=seq(min(data$Latitude),max(data$Latitude),length.out=length(site))

# Latitude vs. vital rate contributions
pdf("Figures/Fig4_latitude_LTRE.pdf")
par(las=1,bty="l",cex.lab=1.8,cex.axis=1.6,mfrow=c(2,3),oma = c(5,2,0,2) + 0.1,mar = c(0,4,3,3) + 0.1,xpd=NA,mgp=c(4.5,1,0))

# Latitude vs. survival
m1=lm(LTRE_surv~Latitude,data=LTRE)
m2=lm(LTRE_surv~poly(Latitude,2),data=LTRE)
AICc(m1,m2)
summary(m2)

plot(LTRE$Latitude,LTRE$LTRE_surv,pch=19,xlab="",ylab="Prob (Survival) contribution",ylim=c(min(LTRE$surv.lower.ci),max(LTRE$surv.upper.ci)),xaxt="n")
lines(lat,predict(m2,newdata=data.frame(Latitude=lat),type="response"))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$surv.lower.ci, y1=LTRE$surv.upper.ci, code=3, angle=90, length=0.02)
mtext("A",side=3,cex=1.5,line=1.1,adj=-0.55)

# Latitude vs. growth
m3=lm(LTRE_growth~Latitude,data=LTRE)
m4=lm(LTRE_growth~poly(Latitude,2),data=LTRE)
AICc(m3,m4)
summary(m3)

plot(LTRE$Latitude,LTRE$LTRE_growth,pch=19,xlab="",ylab="Growth contribution",ylim=c(min(LTRE$growth.lower.ci),max(LTRE$growth.upper.ci)),xaxt="n")
lines(lat,predict(m3,newdata=data.frame(Latitude=lat),type="response"))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$growth.lower.ci, y1=LTRE$growth.upper.ci, code=3, angle=90, length=0.02)
mtext("B",side=3,line=1.1,cex=1.5,adj=-0.55)

# Latitude vs. flowering
m5=lm(LTRE_flowering~Latitude,data=LTRE)
m6=lm(LTRE_flowering~poly(Latitude,2),data=LTRE)
AICc(m5,m6)
summary(m5)

plot(LTRE$Latitude,LTRE$LTRE_flowering,pch=19,xlab="",ylab="Prob (Flowering) contribution",ylim=c(min(LTRE$flowering.lower.ci),max(LTRE$flowering.upper.ci)),xaxt="n")
lines(lat,predict(m5,newdata=data.frame(Latitude=lat),type="response"))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$flowering.lower.ci, y1=LTRE$flowering.upper.ci, code=3, angle=90, length=0.02)
mtext("C",side=3,cex=1.5,line=1.1,adj=-0.55)

# Latitude vs. fruit #
m7=lm(LTRE_fruit~Latitude,data=LTRE)
m8=lm(LTRE_fruit~poly(Latitude,2),data=LTRE)
AICc(m7,m8)
summary(m8)

plot(LTRE$Latitude,LTRE$LTRE_fruit,pch=19,xlab="",ylab="Fruit # contribution",ylim=c(min(LTRE$fruit.lower.ci),max(LTRE$fruit.upper.ci)))
lines(lat,predict(m8,newdata=data.frame(Latitude=lat),type="response"))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$fruit.lower.ci, y1=LTRE$fruit.upper.ci, code=3, angle=90, length=0.02)
mtext("D",side=3,adj=-0.55,cex=1.5,line=1.1)

# Latitude vs. establishment probability
m11=lm(establishment.prob~Latitude,data=LTRE)
m12=lm(establishment.prob~poly(Latitude,2),data=LTRE)
AICc(m11,m12)
summary(m11)

plot(LTRE$Latitude,LTRE$establishment.prob,pch=19,xlab="",ylab="Prob (Recruitment) contribution",ylim=c(min(LTRE$estab.lower.ci),max(LTRE$estab.upper.ci)))
lines(lat,predict(m11,newdata=data.frame(Latitude=lat),type="response"))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$estab.lower.ci, y1=LTRE$estab.upper.ci, code=3, angle=90, length=0.02)
mtext("E",side=3,adj=-0.55,cex=1.5,line=1.1)

# Latitude vs. offspring size
m9=lm(LTRE_recruits~Latitude,data=LTRE)
m10=lm(LTRE_recruits~poly(Latitude,2),data=LTRE)
AICc(m9,m10)
summary(m9)

plot(LTRE$Latitude,LTRE$LTRE_recruits,pch=19,xlab="",ylab="Offspring size (mean & SD) contribution",ylim=c(min(LTRE$recruit.lower.ci),max(LTRE$recruit.upper.ci)))
arrows(x0=LTRE$Latitude, x1=LTRE$Latitude, y0=LTRE$recruit.lower.ci, y1=LTRE$recruit.upper.ci, code=3, angle=90, length=0.02)
mtext("F",side=3,adj=-0.55,cex=1.5,line=1.1)

# add common x-axis for all panels
title(xlab=expression(paste("Latitude (",degree~N,")")),outer=TRUE,line=4,cex.lab=1.8)  

dev.off()

#*******************************************************************************
### 7. Check LTRE by comparing sum of LTRE contributions across all parameters within each site to lambda difference between site and reference IPM (lambda.site-lambda.ref)
#*******************************************************************************

# Create empty vectors to be filled with site and sum of vital rate contributions and lambda across each site
Site=c()
site.sum=c()

# sum LTRE contributions across all parameters within each site
for (i in 1:length(site)) {
  LTRE.site=subset(LTRE,Site==site[i])
  site.sum[i]=sum(LTRE.site[,1:13])
  Site[i]=as.character(site[i])
}

# make data frame of sum of LTRE contributions and IPM reference lambda and site name; then join with site-specific lambda
site.sum=data.frame(Site,site.sum) %>% join(site.lambda)

# calculate difference between site lambda and reference lambda
site.sum$lambda.diff=site.sum$lambda-lambda.ref

# caclulate percent difference between site.sum and lambda.diff
site.sum$percent.diff=abs(site.sum$lambda.diff-site.sum$site.sum)/abs(site.sum$lambda.diff)*100

# plot sum vs. site-specific lambda
plot(site.sum$Latitude,site.sum$site.sum,ylim=c(-1.1,1.1),pch=19,col="grey",cex=1.5,las=1,bty="l",xlab="Latitude",ylab="sum (LTRE contributions) or lambda.site-lambda.ref")
points(site.sum$Latitude,site.sum$lambda.diff,pch=19)
legend("topleft",legend=c("sum (LTRE contributions)","lambda.site-lambda.ref"),pch=19,pt.cex=c(1.5,1),col=c("grey","black"),bty="n")

#*******************************************************************************
### 8. Demographic compensation (Villellas et al. 2015, Ecology Letters)
#*******************************************************************************

#*******************************************************************************
### 8A. Determine proportion of observed significantly negative correlations among groups of vital rates
#*******************************************************************************

# Select grouped vital rates and rename columns
LTRE_vital_rates=data.frame(LTRE$LTRE_surv,LTRE$LTRE_growth,LTRE$LTRE_flowering,LTRE$LTRE_fruit,LTRE$LTRE_recruits,LTRE$seed.ct,LTRE$establishment.prob)
colnames(LTRE_vital_rates)=c("surv","growth","flowering","fruit","recruits","seed.ct","establishment.prob")

# Define function to compute pairwise Spearman correlations with 1-tailed test to detect negative correlations
# This code was taken from: http://rstudio-pubs-static.s3.amazonaws.com/3202_25dc9dde0d26417b9f88e7be4c4b05ce.html
my_cor <- function(x) {
  params <- colnames(x)  # get list of parameter codes
  params.grid <- expand.grid(params, params)  # make list of parameter pairs (XX, XY, XZ, YX, ...)
  cor.results <- data.frame()
  for (i in 1:nrow(params.grid)) {
    # loop through each row of params.grid
    x.param <- as.character(params.grid$Var1)[i]  # get the x parameter as a character
    y.param <- as.character(params.grid$Var2)[i]  # get the y parameter as a character
    x.values <- subset(x, select = x.param)[,1]  # extract x values
    y.values <- subset(x, select = y.param)[,1]  # extract y values
    xy.cor <- cor.test(x.values, y.values, method = "spearman",alternative="less")
    cor.results <- rbind(cor.results, data.frame(ParameterX = x.param, ParameterY = y.param, 
                                                 Rho = xy.cor$estimate, PValue = xy.cor$p.value, row.names = NULL))
  }
  return(cor.results)
}

# Compute all pairwise Spearman correlations among vital rates; use 1-tailed test to detect significant negative correlations; remove redundant pairs of correlations
vital_rates_corr=my_cor(LTRE_vital_rates) %>% subset(Rho<1&!duplicated(Rho))

# Compute proportion of significantly negative correlations at P < 0.05
sig_neg=subset(vital_rates_corr,Rho<0&PValue<0.05)
obs_percent_neg=length(sig_neg$ParameterX)/length(vital_rates_corr$ParameterX)

# Compute proportion of significantly positive correlations at P < 0.05
sig_pos=subset(vital_rates_corr,Rho>0&PValue<0.05)
obs_percent_pos=length(sig_pos$ParameterX)/length(vital_rates_corr$ParameterX)

#*******************************************************************************
### 8B. Perform randomization test to assess whether there are more negative correlations among vital rates than expected by chance
#*******************************************************************************

# set seed to obtain reproducible results
set.seed(12345)

# Create empty vector to be filled in randomization loop
percent_negative=c()
percent_positive=c()

for (i in 1:10000) {
  
  # Shuffle each vital rate contribution among populations by sampling WITHOUT replacement
  LTRE_vital_rates_randomized=apply(LTRE_vital_rates,2,sample) %>% data.frame()
  
  # Compute all pairwise Spearman correlations among vital rates; use 1-tailed test to detect significant negative correlations; remove redundant correlations
  vital_rates_randomized_corr=my_cor(LTRE_vital_rates_randomized) %>% subset(Rho<1&!duplicated(Rho)) 
  
  # Compute proportion of significantly negative correlations at P < 0.05
  sig_neg_randomized=subset(vital_rates_randomized_corr,Rho<0&PValue<0.05)
  percent_negative[i]=length(sig_neg_randomized$ParameterX)/length(vital_rates_randomized_corr$ParameterX)
  
  # Compute proportion of significantly positive correlations at P < 0.05
  sig_pos_randomized=subset(vital_rates_randomized_corr,Rho>0&PValue<0.05)
  percent_positive[i]=length(sig_pos_randomized$ParameterX)/length(vital_rates_randomized_corr$ParameterX)
  
} # end loop

# Obtain significance level of observed proportion of negative correlations as proportion of values in the null distribution that were as high as or higher than the observed percentage 
# See Appendix S1 from Villellas et al. 2015, Ecology Letters for further details
neg_corr_P=length(percent_negative[percent_negative>=obs_percent_neg])/length(percent_negative)

# Obtain significance level of observed proportion of positive correlations as proportion of values in the null distribution that were as low as or lower than the observed percentage 
# See Appendix S1 from Villellas et al. 2015, Ecology Letters for further details
pos_corr_P=length(percent_positive[percent_positive<=obs_percent_pos])/length(percent_positive)

# Plot results
hist(percent_negative, main="",xlab="% negative correlations")
abline(v=obs_percent_neg,col="red",lty=2,lwd=2)

#*******************************************************************************
### 8C. Plot significantly negative pairwise correlations among LTRE contributions
#*******************************************************************************

# Set up color vector
lat_cols=colorRampPalette(brewer.pal(11,"Spectral"))
LTRE$site.cols=lat_cols(32)[as.numeric(cut(LTRE$Latitude,breaks = 32))]

pdf("Figures/FigS3_negative_LTRE_correlations.pdf")

par(las=1,bty="l",cex.lab=1.1,cex.axis=1,mfrow=c(2,2),oma = c(5,2,0,3) + 0.1,mar = c(0,2,2,2) + 0.1,xpd=NA)

plot(LTRE$LTRE_surv,LTRE$LTRE_flowering,ylab="Prob (Flowering) contribution",xlab="",xaxt="n",bg=LTRE$site.cols,pch=21,col="lightgrey",cex=1.5,xlim=c(min(LTRE$surv.lower.ci),max(LTRE$surv.upper.ci)),ylim=c(min(LTRE$flowering.lower.ci),max(LTRE$flowering.upper.ci)))
arrows(x0=LTRE$LTRE_surv, x1=LTRE$LTRE_surv, y0=LTRE$flowering.lower.ci, y1=LTRE$flowering.upper.ci, code=3, angle=90, length=0.02,col=LTRE$site.cols)
arrows(x0=LTRE$surv.lower.ci, x1=LTRE$surv.upper.ci, y0=LTRE$LTRE_flowering, y1=LTRE$LTRE_flowering, code=3, angle=90, length=0.02,col=LTRE$site.cols)
mtext("A",side=3,adj=-0.1,font=2,line=0.5)

plot(LTRE$LTRE_fruit,LTRE$establishment.prob,xlab="",ylab="Prob (Recruitment) contribution",xaxt="n",bg=LTRE$site.cols,pch=21,col="lightgrey",cex=1.5,xlim=c(min(LTRE$fruit.lower.ci),max(LTRE$fruit.upper.ci)),ylim=c(min(LTRE$estab.lower.ci),max(LTRE$estab.upper.ci)))
arrows(x0=LTRE$LTRE_fruit, x1=LTRE$LTRE_fruit, y0=LTRE$estab.lower.ci, y1=LTRE$estab.upper.ci, code=3, angle=90, length=0.02,col=LTRE$site.cols)
arrows(x0=LTRE$fruit.lower.ci, x1=LTRE$fruit.upper.ci, y0=LTRE$establishment.prob, y1=LTRE$establishment.prob, code=3, angle=90, length=0.02,col=LTRE$site.cols)
mtext("B",side=3,adj=-0.1,font=2,line=0.5)


plot(LTRE$LTRE_surv,LTRE$LTRE_fruit,ylab="Fruit # contribution",xlab="Prob (Survival) contribution",bg=LTRE$site.cols,pch=21,col="lightgrey",cex=1.5,ylim=c(min(LTRE$fruit.lower.ci),max(LTRE$fruit.upper.ci)),xlim=c(min(LTRE$surv.lower.ci),max(LTRE$surv.upper.ci)))
arrows(y0=LTRE$LTRE_fruit, y1=LTRE$LTRE_fruit, x0=LTRE$surv.lower.ci, x1=LTRE$surv.upper.ci, code=3, angle=90, length=0.02,col=LTRE$site.cols)
arrows(y0=LTRE$fruit.lower.ci, y1=LTRE$fruit.upper.ci, x0=LTRE$LTRE_surv, x1=LTRE$LTRE_surv, code=3, angle=90, length=0.02,col=LTRE$site.cols)
mtext("C",side=3,adj=-0.1,font=2,line=0.5)

plot(LTRE$LTRE_fruit,LTRE$LTRE_recruits,xlab="Fruit # contribution",ylab="Offspring size (mean & SD) contribution",bg=LTRE$site.cols,pch=21,col="lightgrey",cex=1.5,xlim=c(min(LTRE$fruit.lower.ci),max(LTRE$fruit.upper.ci)),ylim=c(min(LTRE$recruit.lower.ci),max(LTRE$recruit.upper.ci)))
arrows(x0=LTRE$LTRE_fruit, x1=LTRE$LTRE_fruit, y0=LTRE$recruit.lower.ci, y1=LTRE$recruit.upper.ci, code=3, angle=90, length=0.02,col=LTRE$site.cols)
arrows(x0=LTRE$fruit.lower.ci, x1=LTRE$fruit.upper.ci, y0=LTRE$LTRE_recruits, y1=LTRE$LTRE_recruits, code=3, angle=90, length=0.02,col=LTRE$site.cols)
mtext("D",side=3,adj=-0.1,font=2,line=0.5)

legend(x=0.45,y=1.4,legend=seq(1,32,1),pch=19,col=LTRE$site.cols,bty="n",xpd=NA,title="Population",pt.cex=1.5)

dev.off()