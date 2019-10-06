#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Sample unique individuals from each site with replacement to create bootstrap datasets
############ LTRE (Life Table Response Experiment) contributions are then obtained for each bootstrapped dataset
############ Replicate bootstrap datasets will be used to obtain confidence intervals around LTRE contributions for each population
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(plyr)
require(dplyr)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. Read in replicate bootstrap datasets for each site, sampling with replacement ###
####### Read in parameter estimates from bootstrapped datasets 
#*******************************************************************************

# Set number of bootstrap replicate datasets
n.boot=2000

# Read in bootstrapped data 
bootstrapped.data=readRDS("R_output/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013.rds")

# Read in bootstrapped parameter sets for each site 
bootstrapped.params=read.csv("R_output/Mcard_demog_INDIV_BOOTSTRAP_params_2010-2013.csv")

# Read in bootstrapped lambdas for each site 
bootstrapped.lambda=read.csv("R_output/Mcard_demog_INDIV_BOOTSTRAP_lambda_2010-2013.csv")

# Remove monster plants where individuals were not distinguished
#### NOTE: these are plants that A. Angert noted as "not ok, definitely exclude from survival, growth, and fecundity but ok for seed input denominator for recruitment (history of lumping/splitting/relumping; redundant IDs)"
bootstrapped.data=subset(bootstrapped.data,NotAnIndividual!=1|is.na(NotAnIndividual))

# Replace parameters with NA values with 0s
bootstrapped.params[is.na(bootstrapped.params)]<-0

#*******************************************************************************
#### 2. Create a reference matrix based on mean parameter estimates for each replicate bootstrap dataset ###
#*******************************************************************************

# Create empty list to be filled in loop
lambda.boot=list()

for (k in 1:n.boot) {
  data.rep=subset(bootstrapped.data,Replicate==k) # select data from replicate k
  params.rep=subset(bootstrapped.params,Replicate==k) # select data from replicate k
   
#*******************************************************************************
### 3. Create reference IPM parameterized by coefficients averaged across all sites without any perturbations
#*******************************************************************************

#  Compute mean parameter estimates across all sites 
# Note that the "2" in the middle slot refers to 2nd dimension, columns, rather than rows
params_mean=c()
params_mean=apply(params.rep[,2:14],2,mean) %>% t() %>% data.frame()
params_mean$seed.ct=round(params_mean$seed.ct,digits=0)

# Rename parameters and data for use in IPM script
params1=params_mean
data1=data.rep

# Run script to create survival, growth, and fecundity functions and build reference IPM
source("R_scripts/integral_projection_model.R")

# obtain lambda estimate
(params_mean$lambda_ref <- Re(eigen(K)$values[1]))
params_mean$Replicate=k
lambda.boot[[k]]=params_mean
} # end loop

#*******************************************************************************
### 4. Calculate sensitivity of each vital rate coefficient in reference IPM (built from coefficients averaged across sites)
#*******************************************************************************

# set value for delta, amount that will be added to each coefficient
delta=0.01

# create empty vectors corresponding to which coefficient was perturbed, its corresponding lambda estimate, perturbed parameters, and sensitivity
sensParam=c()
lambda.perturbed=c()
sensitivity=c()
params_mean.perturbed=c()
sensitivity.ref=list()

# Initiate loop across bootstrapped datasets
for (k in 1:n.boot) {
  data.rep=subset(bootstrapped.data,Replicate==k) # select data from replicate k
  lambda.rep=lambda.boot[[k]] # select data from replicate k
  params_mean=lambda.rep[1:13]
  lambda.ref=lambda.rep[14]
  
# initiate loop to perturb g'th parameter  
for (g in 1:length(params_mean)) {

	# save parameters to a new object in which the gth element will be perturbed
	params_mean.perturbed=params_mean
    	
  # replace original coefficient g with coefficient + delta
  params_mean.perturbed[g]=params_mean[g] + delta
    	
  # store name of coefficient being perturbed
  sensParam[g] <- names(params_mean[g])
  
  # Rename parameters and data for use in IPM script
  params1=params_mean.perturbed
  data1=data.rep
  
  # Run script to create survival, growth, and fecundity functions and build reference IPM
  source("R_scripts/integral_projection_model.R")

# obtain lambda estimate
(lambda.perturbed[g] <- Re(eigen(K)$values[1]))
    
# obtain estimate of sensitivity, calculated as (lam.perturbed-lambda)/delta, for the gth perturbed coefficient 
sensitivity[g]=(lambda.perturbed[g]-lambda.ref$lambda_ref)/delta
	} # end loop across all coefficients of a given replicate
  
# make a data frame of sensitivities and their respective perturbed parameters
sensitivity.ref[[k]]=data.frame(sensParam,sensitivity,lambda.perturbed)}

# save list of sensitivities of reference matrix to R data
save(sensitivity.ref,file='R_output/sensitivity_ref_matrix_bootstrapped.rda')

#*******************************************************************************
### 5. Calculate site-specific contributions of each vital rate parameter to differences in lambda
#*******************************************************************************

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(bootstrapped.data$Site)

# Create empty list to be filled in loop
LTRE.rep=list()

for (k in 1:n.boot) {
  params.rep=subset(bootstrapped.params,Replicate==k) # select data from replicate k
  lambda.rep=lambda.boot[[k]] # select data from replicate k
  params_mean=lambda.rep[1:13]
  lambda.ref=lambda.rep[14]
   
# For each site, calculate LTRE contribution of each vital rate parameter by looping through parameters of each site
  
  # Create empty objects to be filled in loop
  LTRE=data.frame()
  
for (i in 1:length(site)) {
  params1=subset(params.rep,Site==site[i],select=-Site) # select parameters for site f and remove Site column
	params_diff=params1[1:13]-params_mean # calculate difference between parameters of site f and mean parameters
	LTRE.site=params_diff*sensitivity.ref[[k]]$sensitivity
	LTRE.site$Site=site[i]
	LTRE.site$Replicate=k
	LTRE=rbind(LTRE,LTRE.site)
} # end among-site loop

  LTRE.rep[[k]]=LTRE

  } # end among-bootstrapped replicates loop

# Convert to data frame
LTRE.rep=do.call(rbind, LTRE.rep)
str(LTRE.rep)

# Summarize contributions by vital rates

# survival contribution
LTRE.rep$LTRE_surv=LTRE.rep$surv.int+LTRE.rep$surv.slope

# growth contribution
LTRE.rep$LTRE_growth=LTRE.rep$growth.int+LTRE.rep$growth.slope+LTRE.rep$growth.sd

# flowering probability contribution
LTRE.rep$LTRE_flowering=LTRE.rep$flowering.int+LTRE.rep$flowering.slope

# fruit # contribution; could add seed count to this since it's not really site-specific
LTRE.rep$LTRE_fruit=LTRE.rep$fruit.int+LTRE.rep$fruit.slope

# offspring size distribution contribution
LTRE.rep$LTRE_recruits=LTRE.rep$recruit.logSize.mean+LTRE.rep$recruit.logSize.sd

# write to .csv file
write.csv(LTRE.rep,"R_output/Mcard_demog_LTRE_BOOTSTRAP.csv",row.names = FALSE)
