#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Create data frame of vital rate parameters and build integral projection models 
############# Obtain estimates of lambda for each of 32 populations
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# require packages
require(lme4)
require(glmmADMB)
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
#### 2. Create global survival, growth and fecundity models using data from all sites ###
#*******************************************************************************

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(data$Site)

# Set up data frame of model parameters
params=c()

#*******************************************************************************
  ### 3A. Survival ###
  #*******************************************************************************

  # Read in top survival model output (Formula: Surv ~ logSize + (logSize | Site) + (logSize | Year))
  surv.reg=load("R_output/surv.reg.rda")

  # Store model coefficients
  params$surv.int=coefficients(s3)$Site[,1] 
  params$surv.slope=coefficients(s3)$Site[,2] 
  params$Site=rownames(coefficients(s3)$Site)
  
  #*******************************************************************************
  ### 3B. Growth ###
  #*******************************************************************************
  
  # Read in top growth model output (Formula: logSizeNext ~ logSize + (logSize | Site) + (1 | Year))
  growth.reg=load("R_output/growth.reg.rda")
  
  # Store model coefficients
  params$growth.int=coefficients(g4)$Site[,1] 
  params$growth.slope=coefficients(g4)$Site[,2] 
  params$growth.sd=rep(sigma(g4),times=length(site)) 
  
  #*******************************************************************************
  ### 3C. Flowering ###
  #*******************************************************************************
  
  # Read in top flowering model output (Formula: Fec0 ~ logSize + (logSize | Site) + (logSize | Year))
  flowering.reg=load("R_output/flowering.reg.rda")

  # Store model coefficients
  params$flowering.int=coefficients(fl3)$Site[,1] 
  params$flowering.slope=coefficients(fl3)$Site[,2] 
  
  #*******************************************************************************
  ### 3D. Fruit number (untransformed) using negative binomial regression ###
  #*******************************************************************************
  
  # Read in top model output for fruit.reg (Formula: Fec0 ~ logSize + (logSize | Site) + (logSize | Year))   
  fruit.reg=load("R_output/fruit.reg.rda")

  # Store model coefficients
  params$fruit.int=fixef(fr6)[1]+ranef(fr6)$Site[,1] 
  params$fruit.slope=fixef(fr6)[2]+ranef(fr6)$Site[,2] 

  #*******************************************************************************
  ### 3E. Size distribution of recruits ###
  #*******************************************************************************
  recruit.size.mean=tapply(data$logSizeNext[is.na(data$logSize)],data$Site[is.na(data$logSize)],FUN="mean") %>% data.frame()
  recruit.size.sd=tapply(data$logSizeNext[is.na(data$logSize)],data$Site[is.na(data$logSize)],FUN="sd") %>% data.frame()
  
  params$recruit.logSize.mean=recruit.size.mean[,1]  
  params$recruit.logSize.sd=recruit.size.sd[,1]  
  
  params$recruit.logSize.mean[is.na(params$recruit.logSize.mean)]=0
  params$recruit.logSize.sd[is.na(params$recruit.logSize.sd)]=0

  #*******************************************************************************
  ### 3F. Create data frame of site-specific parameter estimates ###
  #*******************************************************************************
  
  params=data.frame(params)

  #*******************************************************************************
  ### 3G. Number of seeds per fruit ###
  #*******************************************************************************
  
  seeds.per.site=tapply(data$SeedCt,data$Site,FUN=min,na.rm=T) # obtain mean seed counts per fruit per site
  seeds.per.site=data.frame(seeds.per.site,rownames(seeds.per.site)) # make into a data frame
  colnames(seeds.per.site)=c("seed.ct","Site") # define column names for merging
  params=merge(params,seeds.per.site,by.x="Site",by.y="Site") # site-specific seed counts; note that this only works because both data frames are ordered in the same way!

  #*******************************************************************************
  ### 3H. Establishment probability ###
  #*******************************************************************************
  
  # Obtain number of new recruits per site
  recruit.number=tapply(data$logSizeNext[is.na(data$logSize)],data$Site[is.na(data$logSize)],FUN="length") %>% data.frame()
  colnames(recruit.number)="recruit.number"
  
  # Obtain total fruit count per site 
  fruits.per.site=tapply(site_fruit_count_data$Fec1[!is.na(site_fruit_count_data$Fec1)],site_fruit_count_data$Site[!is.na(site_fruit_count_data$Fec1)],sum)
  
  # Obtain total seed count per site (= # fruits per site * # seeds per fruit per site)
  total.seeds.per.site=fruits.per.site*seeds.per.site$seed.ct	
  
  # Estimate establishment probability as # of new recruits/# of seeds
  params$establishment.prob=recruit.number$recruit.number/total.seeds.per.site
  
  # Set establishment probability as 0 for Hauser Creek (was calculated as NA because Hauser creek has 0 new recruits)
  params$establishment.prob[is.na(params$establishment.prob)]=0	

  # Store parameters in .csv file for later use
  write.csv(params,"R_output/vital_rate_coefficients.csv",row.names=FALSE)
  
#*******************************************************************************
### 4. Create site-specific IPMs parameterized by site-specific parameters derived from global vital rates models 
#*******************************************************************************

  #*******************************************************************************
  ### 4A. Subset data for site f
  #*******************************************************************************
  
  # create empty vectors for lambda and site to be filled
  lambda=c()
  Site=character()
  
  for (f in 1:length(site)) {
    data1=subset(data,Site==site[f])
    params1=subset(params,Site==site[f])
    params1=subset(params1,select=-Site)
    
    #*******************************************************************************
    ### 4B. Create survival, growth, and fecundity functions and build IPM by running integral_projection_model.R script
    #*******************************************************************************
    
    source("R_scripts/integral_projection_model.R")
    
    #*******************************************************************************
    ### 4C. Obtain lambda estimate for site f
    #*******************************************************************************
    
    lambda[f] <- Re(eigen(K)$values[1])
    Site[f]=as.character(site[f])
    } # end loop to run IPMs and estimate lambdas for each site
    
    # make data frame of site and lambda
    site.lambda=data.frame(Site,lambda)
    
#*******************************************************************************
### 5. Merge site information with lambda estimates and save to .csv file
#*******************************************************************************

# Create data frame of Site, Latitude, Longitude, Region, and Elevation for hypothesis testing
site.info=subset(data,select=c(Site,Latitude,Longitude,Elevation,Region,RegionRank)) %>% unique() %>% arrange(-Latitude)
    
# merge site info with lambda estimates
site.info=join(site.info,site.lambda)

# save to .csv file 
write.csv(site.info,"R_output/site.lambda.csv",row.names=FALSE)

