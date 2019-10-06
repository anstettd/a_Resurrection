#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Obtain bootstrapped vital rate coefficients
############# IPMs parameterized by bootstrapped coeffcients will then be run and lambdas will be obtained at southern edge (Regions S1 & S2)
############# Replicate bootstrap datasets will be used to obtain confidence intervals around lambda estimates for each year 
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

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
bootstrapped.data=readRDS("R_output/temporal_demog_compensation_TableS7_FigS7/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013_yearly_south.rds")
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

# Create list to store regression objects
results=c()
surv.reg=list()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('glmer'), .packages=c('lme4')) %dopar% {
  
  # Inspect model
  surv.reg[[i]]=glmer(Surv~logSize+(1|Site)+(logSize|Year),data=subset(bootstrapped.data,Replicate==i),family=binomial)   
} # End loop

saveRDS(results,"R_output/temporal_demog_compensation_TableS7_FigS7/surv.reg_boot_south.rds")

#*******************************************************************************
#### 3. Growth ###
#*******************************************************************************

# Create list to store regression objects
results=c()
growth.reg=list()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('lmer'), .packages=c('lme4')) %dopar% {
  
  # Inspect model
  growth.reg[[i]]=lmer(logSizeNext~logSize+(logSize|Site)+(1|Year),data=subset(bootstrapped.data,Replicate==i))   
} # End loop

# save model outputs
saveRDS(results,"R_output/temporal_demog_compensation_TableS7_FigS7/growth.reg_boot_south.rds")

#*******************************************************************************
#### 4. Flowering probability ###
#*******************************************************************************

# Create list to store regression objects
results=c()
flowering.reg=list()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('glmer'), .packages=c('lme4')) %dopar% {
  
  # Inspect model
  flowering.reg[[i]]=glmer(Fec0~logSize+(1|Site)+(logSize|Year),data=subset(bootstrapped.data,Replicate==i),family=binomial,control=glmerControl(optimizer = "bobyqa"))   
} # End loop

saveRDS(results,"R_output/temporal_demog_compensation_TableS7_FigS7/flowering.reg_boot_south.rds")

#*******************************************************************************
#### 5. Fruit # ###
#*******************************************************************************

# Create list to store regression objects
fruit.reg=list()
results=c()

# parallel processing
results=foreach(i = 1:n.boot,.export=c('glmmadmb'), .packages=c('glmmADMB')) %dopar% {
  
  # Inspect model
  fruit.reg[[i]]=glmmadmb(Fec1~logSize+(logSize|Site)+(1|Year),data=subset(bootstrapped.data,Replicate==i&!is.na(Fec1)),family="nbinom",link="log")   
} # End loop

# save model outputs
saveRDS(results,"R_output/temporal_demog_compensation_TableS7_FigS7/fruit.reg_boot_south.rds")



