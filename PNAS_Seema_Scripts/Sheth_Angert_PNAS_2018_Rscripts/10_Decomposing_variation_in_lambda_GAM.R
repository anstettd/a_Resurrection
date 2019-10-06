#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Partition variance in lambda explained by each vital rate using a generalized additive model
############# Because conventional life table response experiments were not performing well, we used approach outlined in Ellner 2016 book, chapter 7
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(mgcv)
require(MASS)
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
#### 2. read in vital rate coefficients and lambda estimates and merge into one data frame
#*******************************************************************************

# read in .csv file of vital rate model coefficients
params=read.csv("R_output/vital_rate_coefficients.csv")

# remove growth.sd parameter because it is constant across all sites
params_coef=subset(params,select=-c(growth.sd,Site))

# read in .csv file of site info and lambda estimates
site.lambda=read.csv("R_output/site.lambda.csv")

# merge vital rate parameters and lambda estimates into one data frame
params=join(params,site.lambda)

#*******************************************************************************
### 3. Steve Ellner's way of simulating "random" datasets of parameters, see "Params2.r" code that Ellner e-mailed on 20160822
##### This is needed because most parameter distributions across sites do not closely approximate a Gaussian distribution
#*******************************************************************************
 
par(mfrow=c(3,4)); 
for(j in 1:ncol(params_coef)) hist(params_coef[,j],xlab="Value",ylab="Frequency",main=paste(j," ",names(params_coef)[j])) 
# not even close to Gaussian, for a majority of them. 

# 
# Generate a large set of parameters with same marginal distributions
#    and Spearman rank-correlations as the estimated parameters 
#

## Generate Gaussians with the same rank-correlations as the data using Fackler's method 
##  Fackler PL. 1991 Modeling interdependence: an approach to simulation and elicitation. 
#    Am J Agric Econ 73: 1091 – 1097.
##  Full details are in www4.ncsu.edu/~pfackler/randcorr.ps

set.seed(12345)

# make params_coef data frame a matrix
params_coef_mat=as.matrix(params_coef)

C=cor(params_coef_mat,method="spearman");  # rank correlations of the data 
Sigma=2*sin((pi/6)*C);			# Generate Gaussians with same rank correlations 
mu=apply(params_coef_mat,2,mean);
Z = mvrnorm(10000,mu,Sigma);

## Back-transform from Gaussian to the actual parameter distributions 
## using quantile transformation. 
random_params=matrix(NA,nrow(Z),ncol(Z));
for(j in 1:ncol(params_coef)) {
  px = (0:(nrow(params_coef)-1))/(nrow(params_coef)-1); py = sort(params_coef[,j]);
  cfun = approxfun(px,py); # value as a function of quantile
  z1 = Z[,j]; pz = (rank(z1)-1)/(length(z1)-1);
  random_params[,j] = cfun(pz);     # transform Z values based on their quantile 
}
random_params = data.frame(random_params); names(random_params) <- names(params_coef); 

## Check that it works. The rank-cor should be nearly the same, the range should be exactly the same.
## The means and ordinary Pearson correlations should be similar, but the method does not preserve
## those exactly. 
par(mfrow=c(2,2))
Cnew=cor(random_params,method="spearman");
plot(Sigma,Cnew,main="Compare rank-correlation matrices"); abline(0,1,lty=2)
plot(log(abs(cov(params_coef))),log(abs(cov(random_params))),main="Compare variance-covariance matrices"); abline(0,1,lty=2)
plot(log(abs(apply(params_coef,2,mean))),log(abs(apply(random_params,2,mean))),main="Compare means"); abline(0,1,lty=2)

apply(params_coef,2,range)-apply(random_params,2,range) # should be all zeros 

# Visualize what this is doing 
plot(random_params[,2],random_params[,3],type="p",pch=1)
points(params_coef[,2],params_coef[,3],type="p",pch=16,col="red",cex=2);

# Add invariant growth sd back to parameters table
random_params$growth.sd=1.107029

# Round seed count back to nearest integer
random_params$seed.ct=round(random_params$seed.ct,digits=0)

#*******************************************************************************
### 4. Create IPMs parameterized by simulated vital rate coefficients
#*******************************************************************************

# create empty vector for lambda to be filled
random_params$lambda=c()

# rename data object data1 for IPM script
data1=data

# initiate loop to create survival, growth, and fecundity functions and estimate IPM 
for (f in 1:10000) {
	params1=random_params[f,]
    
# Create survival, growth, and fecundity functions and build IPM by running integral_projection_model.R script
source("R_scripts/integral_projection_model.R")

# obtain lambda estimate
(random_params$lambda[f] <- Re(eigen(K)$values[1]))
        
	} # end loop to run IPMs and estimate lambdas simulated parameter dataset
        
#*******************************************************************************
### 5. Fit GAM to simulated parameter sets and their respective lambda estimates
#*******************************************************************************

#*******************************************************************************
### 5A. Fit a GAM to lambda as a function of parameters 
#*******************************************************************************

# log transform lambda
random_params$log.lambda=log(random_params$lambda)

# fit GAM
fit.lambda=gam(log.lambda ~ s(surv.int) + s(surv.slope) + s(growth.int) + s(growth.slope) + s(flowering.int) + s(flowering.slope) + s(fruit.int)
            +s(fruit.slope) + s(recruit.logSize.mean) + s(recruit.logSize.sd) + s(seed.ct) + s(establishment.prob), data=random_params)

#*******************************************************************************
### 5B. Evaluate the accuracy of the GAM at the observed parameter values
#*******************************************************************************

lambda.true = log(params$lambda) # real lambdas 
lambda.hat = predict(fit.lambda,type="response",newdata=params); # lambdas predicted from the GAM

plot(lambda.hat,lambda.true); abline(0,1); 
rsq = 1- mean((lambda.hat-lambda.true)^2)/mean((lambda.true-mean(lambda.true))^2); # fraction of variance explained by GAM fit 
title(main=round(rsq,digits=3))

#*******************************************************************************
### 5C. Do what Rees & Ellner (Ecological Monographs, 2009) did in the Carlina LTRE to calculate contributions of variance in each parameter to variance in lambda
#*******************************************************************************

terms = predict(fit.lambda,data=params,type="terms"); 
C = cov(terms); 
round(C,digits=2) # diagonal entries are contribution of variance in each parameter to variance in lambda, 
                   # off-diagonal entries are contributions from pairwise covariance of parameters.  

# check LTRE (see Ellner 2016 book, chapter 7 & Carlina LTRE.R script )
sum(C)+summary(fit.lambda)$scale
var(random_params[,"log.lambda"])

# calculate Cont terms (normalized to sum to 1) and sort; note that these can be + or -, with negative values meaning that variation in vital rate decreases variation in lambda
order.terms.gam <-apply(C,1,sum)/sum(C) 
order.terms.gam

# calculate % variance explained by each vital rate parameter (how much variance in lambda would be produced by the observed variance in that parameter acting on its own)
var_explained=diag(C)/sum(diag(C))

#*******************************************************************************
### 5D. Obtain total contribution of each vital rate
#*******************************************************************************

# Proportion variance in lambda explained by each parameter's total effect (i.e. from its intercept and slope parameters combined). In other words, how much variance in lambda would be produced by the observed variance in that parameter acting on its own?
prop_var=c()
prop_var$surv=var_explained[1]+var_explained[2]
prop_var$growth=var_explained[3]+var_explained[4]
prop_var$flowering=var_explained[5]+var_explained[6]
prop_var$fruit=var_explained[7]+var_explained[8]
prop_var$recruit=var_explained[9]+var_explained[10]
prop_var$seed.ct=var_explained[11]
prop_var$establishment.prob=var_explained[12]
prop_var=data.frame(prop_var)

