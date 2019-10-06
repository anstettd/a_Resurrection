#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Obtain vital rate contributions to variation in lambda and perform permutation test to test for temporal demographic compensation at southern range edge (Regions S1 & S2)
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171121

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(plyr)
require(dplyr)
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
# Class: stage class (juvenile, adult, or NA) of plant at time = t (PY)
# Fec1: TotFr (Total number of fruits per individual)   
# logSize: total stem length of the individual
# ClassNext: stage class (juvenile, adult, dead, or NA) of plant at time = t+1 (CY)
# logSizeNext: same as "logSize" above, for t+1
# Surv: survival (1) or not (0) of individuals between time = t (PY) and time = t+1 (CY)
# Year: annual transition of the long-term data at time = t (2010-2013)
# Fec0: Probability of flowering (1 if Class=="A" for adult, 0 if Class=="J" for juvenile)
# RegionRank: ordinal rank of regions from south to north
# SeedCt: mean seed count, rounded to the nearest integer, for each site

#*******************************************************************************
#### 2. Bring in vital rate coefficients and bootstrapped vital rate contributions ###
#*******************************************************************************

# read in .csv file of vital rate model coefficients, produced by running "M_cardinalis_2010-2013_IPM_7-27-16.R" script
params=read.csv("R_output/temporal_demog_compensation_TableS7_FigS7/vital_rate_coefficients_south.csv")

# read in .csv file of site info and lambda estimates, produced by running "M_cardinalis_2010-2013_IPM_7-27-16.R" script
year.lambda=read.csv("R_output/temporal_demog_compensation_TableS7_FigS7/lambda_south.csv")

# read in .csv file of bootstrapped vital rate contributions, produced by running "13_LTRE_demog_comp_bootstrap.R" script
LTRE.boot=read.csv("R_output/temporal_demog_compensation_TableS7_FigS7/Mcard_demog_LTRE_BOOTSTRAP_yearly_south.csv")

# Remove all regions except southern region
data=subset(data,Region=="S1"|Region=="S2")

#*******************************************************************************
### 3. Create reference IPM parameterized by coefficients averaged across all years without any perturbations
#*******************************************************************************

#  Compute mean parameter estimates across all years 
# Note that the "2" in the middle slot refers to 2nd dimension, columns, rather than rows
params_mean=apply(params[,2:14],2,mean) %>% t() %>% data.frame()
params_mean$seed.ct=round(params_mean$seed.ct,digits=0)

# Rename parameters and data for use in IPM script
params1=params_mean
data1=data

# Run script to create survival, growth, and fecundity functions and build reference IPM
source("R_scripts/integral_projection_model.R")

# obtain lambda estimate
(lambda.ref <- Re(eigen(K)$values[1]))

#*******************************************************************************
### 4. Calculate sensitivity of each vital rate coefficient in reference IPM (built from coefficients averaged across years)
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
	} # end loop across all coefficients of a given year
  
# make a data frame of sensitivities and their respective perturbed parameters
sensitivity.ref=data.frame(sensParam,sensitivity,lambda.perturbed)

# write sensitivities of reference matrix to .csv
write.csv(sensitivity.ref,"R_output/temporal_demog_compensation_TableS7_FigS7/sensitivity_ref_matrix_south.csv",row.names=FALSE)

#*******************************************************************************
### 5. Calculate year-specific contributions of each vital rate parameter to differences in lambda
#*******************************************************************************

# Create a vector of unique years for subsetting
year=unique(data$Year)

# Create empty objects to be filled in loop
LTRE=data.frame()

# For each year, calculate LTRE contribution of each vital rate parameter by looping through parameters of each year

for (i in 1:length(year)) {
	params1=subset(params,Year==year[i],select=-Year) # select parameters for year f and remove year column
	params_diff=params1-params_mean # calculate difference between parameters of year f and mean parameters
	LTRE.year=params_diff*sensitivity.ref$sensitivity
	LTRE.year$Year=year[i]
	LTRE=rbind(LTRE,LTRE.year)
}

# Summarize contributions by vital rates

# survival contribution
LTRE$LTRE_surv=LTRE$surv.int+LTRE$surv.slope

# growth contribution
LTRE$LTRE_growth=LTRE$growth.int+LTRE$growth.slope+LTRE$growth.sd

# flowering probability contribution
LTRE$LTRE_flowering=LTRE$flowering.int+LTRE$flowering.slope

# fruit # contribution; could add seed count to this since it's not really year-specific
LTRE$LTRE_fruit=LTRE$fruit.int+LTRE$fruit.slope

# offspring size distribution contribution
LTRE$LTRE_recruits=LTRE$recruit.logSize.mean+LTRE$recruit.logSize.sd

#*******************************************************************************
### 5. Plot LTRE vs. year and perform statistical tests of relationships
#*******************************************************************************

# Write function to obtain 0.025 and 0.975 quantiles 
ci=function(x) {
  quantile(x,probs=c(0.025,0.975))
}

# Obtain 95% confidence intervals around LTRE contributions for each year (NOTE: NOT BIAS-CORRECTED!)
surv.ci=summarise(group_by(LTRE.boot,Year), surv.lower.ci = quantile(LTRE_surv,probs=0.025), surv.upper.ci=quantile(LTRE_surv,probs=0.975)) %>% data.frame() 
growth.ci=summarise(group_by(LTRE.boot,Year), growth.lower.ci = quantile(LTRE_growth,probs=0.025), growth.upper.ci=quantile(LTRE_growth,probs=0.975)) %>% data.frame() 
flowering.ci=summarise(group_by(LTRE.boot,Year), flowering.lower.ci = quantile(LTRE_flowering,probs=0.025), flowering.upper.ci=quantile(LTRE_flowering,probs=0.975)) %>% data.frame() 
fruit.ci=summarise(group_by(LTRE.boot,Year), fruit.lower.ci = quantile(LTRE_fruit,probs=0.025), fruit.upper.ci=quantile(LTRE_fruit,probs=0.975)) %>% data.frame() 
estab.ci=summarise(group_by(LTRE.boot,Year), estab.lower.ci = quantile(establishment.prob,probs=0.025), estab.upper.ci=quantile(establishment.prob,probs=0.975)) %>% data.frame() 
recruit.ci=summarise(group_by(LTRE.boot,Year), recruit.lower.ci = quantile(LTRE_recruits,probs=0.025), recruit.upper.ci=quantile(LTRE_recruits,probs=0.975)) %>% data.frame() 
LTRE_ci=data.frame(surv.ci,growth.ci[,2:3],flowering.ci[,2:3],fruit.ci[,2:3],estab.ci[,2:3],recruit.ci[,2:3])
# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
LTRE=join(LTRE,LTRE_ci,by="Year")

# Obtain 95% bias-corrected confidence intervals around survival contribution for each year
surv.ci.bias=c()

for (i in 1:length(year)) {
  boot.surv=LTRE.boot$LTRE_surv[LTRE.boot$Year==year[i]]
  obs.surv=LTRE$LTRE_surv[LTRE$Year==year[i]]
  z=qnorm(length(boot.surv[boot.surv<obs.surv])/length(boot.surv))
  surv.ci.bias$Year[i]=as.character(year[i]) 
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  surv.ci.bias$surv.lower.ci[i]=quantile(boot.surv,probs=lower.ci)
  surv.ci.bias$surv.upper.ci[i]=quantile(boot.surv,probs=upper.ci)
}

surv.ci.bias=data.frame(surv.ci.bias)

# Obtain 95% bias-corrected confidence intervals around growth contribution for each year
growth.ci.bias=c()

for (i in 1:length(year)) {
  boot.growth=LTRE.boot$LTRE_growth[LTRE.boot$Year==year[i]]
  obs.growth=LTRE$LTRE_growth[LTRE$Year==year[i]]
  z=qnorm(length(boot.growth[boot.growth<obs.growth])/length(boot.growth))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  growth.ci.bias$growth.lower.ci[i]=quantile(boot.growth,probs=lower.ci)
  growth.ci.bias$growth.upper.ci[i]=quantile(boot.growth,probs=upper.ci)
}

growth.ci.bias=data.frame(growth.ci.bias)

# Obtain 95% bias-corrected confidence intervals around flowering contribution for each year
flowering.ci.bias=c()

for (i in 1:length(year)) {
  boot.flowering=LTRE.boot$LTRE_flowering[LTRE.boot$Year==year[i]]
  obs.flowering=LTRE$LTRE_flowering[LTRE$Year==year[i]]
  z=qnorm(length(boot.flowering[boot.flowering<obs.flowering])/length(boot.flowering))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  flowering.ci.bias$flowering.lower.ci[i]=quantile(boot.flowering,probs=lower.ci)
  flowering.ci.bias$flowering.upper.ci[i]=quantile(boot.flowering,probs=upper.ci)
}

flowering.ci.bias=data.frame(flowering.ci.bias)

# Obtain 95% bias-corrected confidence intervals around fruit # contribution for each year
fruit.ci.bias=c()

for (i in 1:length(year)) {
  boot.fruit=LTRE.boot$LTRE_fruit[LTRE.boot$Year==year[i]]
  obs.fruit=LTRE$LTRE_fruit[LTRE$Year==year[i]]
  z=qnorm(length(boot.fruit[boot.fruit<obs.fruit])/length(boot.fruit))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  fruit.ci.bias$fruit.lower.ci[i]=quantile(boot.fruit,probs=lower.ci)
  fruit.ci.bias$fruit.upper.ci[i]=quantile(boot.fruit,probs=upper.ci)
}

fruit.ci.bias=data.frame(fruit.ci.bias)

# Obtain 95% bias-corrected confidence intervals around establishment probability contribution for each year
estab.ci.bias=c()

for (i in 1:length(year)) {
  boot.estab=LTRE.boot$establishment.prob[LTRE.boot$Year==year[i]]
  obs.estab=LTRE$establishment.prob[LTRE$Year==year[i]]
  z=qnorm(length(boot.estab[boot.estab<obs.estab])/length(boot.estab))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  estab.ci.bias$estab.lower.ci[i]=quantile(boot.estab,probs=lower.ci)
  estab.ci.bias$estab.upper.ci[i]=quantile(boot.estab,probs=upper.ci)
}

estab.ci.bias=data.frame(estab.ci.bias)

# Obtain 95% bias-corrected confidence intervals around offspring size contribution for each year
recruit.ci.bias=c()

for (i in 1:length(year)) {
  boot.recruit=LTRE.boot$LTRE_recruits[LTRE.boot$Year==year[i]]
  obs.recruit=LTRE$LTRE_recruits[LTRE$Year==year[i]]
  z=qnorm(length(boot.recruit[boot.recruit<obs.recruit])/length(boot.recruit))
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  recruit.ci.bias$recruit.lower.ci[i]=quantile(boot.recruit,probs=lower.ci)
  recruit.ci.bias$recruit.upper.ci[i]=quantile(boot.recruit,probs=upper.ci)
}

recruit.ci.bias=data.frame(recruit.ci.bias)

LTRE_ci=data.frame(surv.ci.bias,growth.ci.bias,flowering.ci.bias,fruit.ci.bias,estab.ci.bias,recruit.ci.bias)

# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
LTRE=join(LTRE,LTRE_ci,by="Year")

#*******************************************************************************
### 5A. # Year vs. vital rate contributions
#*******************************************************************************

LTRE$Year=as.numeric(as.character(LTRE$Year))

# Latitude vs. vital rate contributions
pdf("Figures/FigS7_Year_LTRE_south.pdf")
par(las=1,bty="l",cex.axis=0.8,cex.lab=1,mfrow=c(2,3),oma = c(5,2,0,2) + 0.1,mar = c(0,2,1,2) + 0.1,xpd=NA)

# Year vs. survival
plot(LTRE$Year,LTRE$LTRE_surv,pch=19,xlab="",ylab="Prob (Survival) contribution",xaxt="n",cex=1.5,ylim=c(min(LTRE$surv.lower.ci),max(LTRE$surv.upper.ci)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$surv.lower.ci, y1=LTRE$surv.upper.ci, code=3, angle=90, length=0.02)
mtext("A",side=3,adj=-0.35,line=-1,font=2)

# Year vs. growth
plot(LTRE$Year,LTRE$LTRE_growth,pch=19,xlab="",ylab="Growth contribution",xaxt="n",cex=1.5,ylim=c(min(LTRE$growth.lower.ci),max(LTRE$growth.upper.ci)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$growth.lower.ci, y1=LTRE$growth.upper.ci, code=3, angle=90, length=0.02)
mtext("B",side=3,adj=-0.35,line=-1,font=2)

# Year vs. flowering
plot(LTRE$Year,LTRE$LTRE_flowering,pch=19,xlab="",ylab="Prob (Flowering) contribution",xaxt="n",cex=1.5,ylim=c(min(LTRE$flowering.lower.ci,LTRE$LTRE_flowering),max(LTRE$flowering.upper.ci,LTRE$LTRE_flowering)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$flowering.lower.ci, y1=LTRE$flowering.upper.ci, code=3, angle=90, length=0.02)
mtext("C",side=3,adj=-0.35,line=-1,font=2)

# Year vs. fruit #
plot(LTRE$Year,LTRE$LTRE_fruit,pch=19,xlab="",ylab="Fruit # contribution",cex=1.5,xaxt="n",ylim=c(min(LTRE$fruit.lower.ci),max(LTRE$fruit.upper.ci)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$fruit.lower.ci, y1=LTRE$fruit.upper.ci, code=3, angle=90, length=0.02)
axis(side=1,at=c(2010,2011,2012,2013),labels=c("2010-11","2011-12","2012-13","2013-14"),cex.axis=0.9)
mtext("D",side=3,adj=-0.35,line=-1,font=2)

# Year vs. establishment probability
plot(LTRE$Year,LTRE$establishment.prob,pch=19,xlab="",ylab="Prob (Recruitment) contribution",cex=1.5,xaxt="n",ylim=c(min(LTRE$estab.lower.ci),max(LTRE$estab.upper.ci)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$estab.lower.ci, y1=LTRE$estab.upper.ci, code=3, angle=90, length=0.02)
axis(side=1,at=c(2010,2011,2012,2013),labels=c("2010-11","2011-12","2012-13","2013-14"),cex.axis=0.9)
mtext("E",side=3,adj=-0.35,line=-1,font=2)

# Year vs. offspring size
plot(LTRE$Year,LTRE$LTRE_recruits,pch=19,xlab="",ylab="Offspring size contribution",cex=1.5,xaxt="n",ylim=c(min(LTRE$recruit.lower.ci),max(LTRE$recruit.upper.ci)))
arrows(x0=LTRE$Year, x1=LTRE$Year, y0=LTRE$recruit.lower.ci, y1=LTRE$recruit.upper.ci, code=3, angle=90, length=0.02)
axis(side=1,at=c(2010,2011,2012,2013),labels=c("2010-11","2011-12","2012-13","2013-14"),cex.axis=0.9)
mtext("F",side=3,adj=-0.35,line=-1,font=2)

# add common x-axis for all panels
title(xlab="Year",outer=TRUE,line=2.5,cex.lab=1.2)  

dev.off()