#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Obtain bootstrapped vital rate coefficients
############# IPMs parameterized by bootstrapped coeffcients will then be run and lambdas will be obtained
############# Replicate bootstrap datasets will be used to obtain confidence intervals around lambda estimates for each site
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(plyr)
require(dplyr)
require(lme4)
require(glmmADMB)
require(doParallel)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. Preliminaries ###
#*******************************************************************************

# Read in & examine bootstrapped data 
bootstrapped.data=readRDS("R_output/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013.rds")
str(bootstrapped.data)

# Remove monster plants where individuals were not distinguished 
#### NOTE: these are plants that A. Angert noted as "not ok, definitely exclude from survival, growth, and fecundity but ok for seed input denominator for recruitment (history of lumping/splitting/relumping; redundant IDs)"
bootstrapped.data=subset(bootstrapped.data,NotAnIndividual!=1|is.na(NotAnIndividual))

# set # of cores to use for parallel processing
registerDoParallel(cores=4)

# Set number of bootstrap replicate datasets
n.boot=2000

#*******************************************************************************
#### 2. Survival probability ###
#*******************************************************************************

# create function to run survival model
surv <- function(i) {
  glmer(Surv~logSize+(logSize|Site)+(logSize|Year),data=subset(bootstrapped.data,Replicate==i),family=binomial,control=glmerControl(optimizer = "bobyqa"))  
}

# run mixed effects model for survival on each bootstrapped dataset
surv.reg <- lapply(1:2000, surv)

# save model output
saveRDS(surv.reg,"R_output/surv.reg_boot.rds") # 12 convergence warnings

#*******************************************************************************
#### 3. Growth ###
#*******************************************************************************

# Create list to store regression objects
results=c()
growth.reg=list()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('lmer'), .packages=c('lme4')) %dopar% {
  
  # Inspect model
  growth.reg[[i]]=lmer(logSizeNext~logSize+(logSize|Site)+(1|Year),data=subset(bootstrapped.data,Replicate==i),control=lmerControl(optimizer = "bobyqa"))   
} # End loop

# save model output
saveRDS(results,"R_output/growth.reg_boot.rds")

#*******************************************************************************
#### 4. Flowering probability ###
#*******************************************************************************

# create function to run survival model
flowering <- function(i) {
  glmer(Fec0~logSize+(logSize|Site)+(logSize|Year),data=subset(bootstrapped.data,Replicate==i),family=binomial,control=glmerControl(optimizer = "bobyqa"))  
}

# run mixed effects model for survival on each bootstrapped dataset
flowering.reg <- lapply(1:2000, flowering)

# save model output
saveRDS(flowering.reg,"R_output/flowering.reg_boot.rds") # 2 convergence warnings

#*******************************************************************************
#### 5. Fruit # ###
#*******************************************************************************

# Create list to store regression objects
fruit.reg=list()
results=c()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('glmmadmb'), .packages=c('glmmADMB')) %dopar% {
  
  # Inspect model
  fruit.reg[[i]]=glmmadmb(Fec1~logSize+(logSize|Site)+(logSize|Year),data=subset(bootstrapped.data,Replicate==i&!is.na(Fec1)),family="nbinom",link="log")   
} # End loop

# save model outputs
saveRDS(results,"R_output/fruit.reg_boot.rds")




