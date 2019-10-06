#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Create data frame of vital rate parameters and build integral projection models 
############# Obtain yearly estimates of lambda for all individuals pooled in southern region (S1, S2)
############# This allows examination of temporal compensation at southern range edge of M. cardinalis
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

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
# Class: stage class (juvenile, adult, or NA) of plant at time = t (PY)
# Fec1: Total number of fruits per individual  
# logSize: total stem length of the individual
# ClassNext: stage class (juvenile, adult, dead, or NA) of plant at time = t+1 (CY)
# logSizeNext: same as "logSize" above, for t+1
# Surv: survival (1) or not (0) of individuals between time = t (PY) and time = t+1 (CY)
# Year: annual transition of the long-term data at time = t (2010-2013)
# Fec0: Probability of flowering (1 if Class=="A" for adult, 0 if Class=="J" for juvenile)
# RegionRank: ordinal rank of regions from south to north
# SeedCt: mean seed count, rounded to the nearest integer, for each site

# Remove all regions except southern region
data=subset(data,Region=="S1"|Region=="S2")

# Obtain total fruit count for each indivdiual in each year, including monster plants
year_fruit_count_data=subset(site_fruit_count_data,Region=="S1"|Region=="S2",select=c(Year,Fec1)) 

#*******************************************************************************
#### 3. Create global survival, growth and fecundity models using data from all sites ###
#*******************************************************************************

# Create a vector of unique years for subsetting
year=unique(data$Year)

# Set up data frame of model parameters
params=c()

#*******************************************************************************
  ### 3A. Survival ###
  #*******************************************************************************

  # Read in top survival model output (Formula: Surv ~ logSize + (1 | Site) + (logSize | Year))
  surv.reg=load("R_output/temporal_demog_compensation_TableS7_FigS7/surv.reg_South.rda")

  # Store model coefficients
  params$surv.int=coefficients(s5)$Year[,1] 
  params$surv.slope=coefficients(s5)$Year[,2] 
  params$Year=rownames(coefficients(s5)$Year)
      
  #*******************************************************************************
  ### 3B. Growth ###
  #*******************************************************************************
  
  # Read in top growth model output (Formula: logSizeNext ~ logSize + (logSize | Site) + (1 | Year))
  growth.reg=load("R_output/temporal_demog_compensation_TableS7_FigS7/growth.reg_South.rda")
  
  # Store model coefficients
  params$growth.int=coefficients(g4)$Year[,1] 
  params$growth.slope=coefficients(g4)$Year[,2] 
  params$growth.sd=rep(sigma(g4),times=length(year)) 
  
  #*******************************************************************************
  ### 3C. Flowering ###
  #*******************************************************************************
  
  # Read in top flowering model output (Formula: Fec0 ~ logSize + (1 | Site) + (logSize | Year))
  flowering.reg=load("R_output/temporal_demog_compensation_TableS7_FigS7/flowering.reg_South.rda")

  # Store model coefficients
  params$flowering.int=coefficients(fl5)$Year[,1] 
  params$flowering.slope=coefficients(fl5)$Year[,2] 
    
  #*******************************************************************************
  ### 3D. Fruit number (untransformed) using negative binomial regression ###
  #*******************************************************************************
  
  # Read in top model output for fruit.reg (Formula: Fec1 ~ logSize + (logSize | Site) + (1 | Year))   
  fruit.reg=load("R_output/temporal_demog_compensation_TableS7_FigS7/fruit.reg_South.rda")

  # Store model coefficients
  params$fruit.int=fixef(fr7)[1]+ranef(fr7)$Year[,1] 
  params$fruit.slope=fixef(fr7)[2]

  #*******************************************************************************
  ### 3E. Size distribution of recruits ###
  #*******************************************************************************
  recruit.size.mean=tapply(data$logSizeNext[is.na(data$logSize)],data$Year[is.na(data$logSize)],FUN="mean") %>% data.frame()
  recruit.size.sd=tapply(data$logSizeNext[is.na(data$logSize)],data$Year[is.na(data$logSize)],FUN="sd") %>% data.frame()
  
  params$recruit.logSize.mean=recruit.size.mean[,1]  
  params$recruit.logSize.sd=recruit.size.sd[,1]  
  
  params$recruit.logSize.mean[is.na(params$recruit.logSize.mean)]=0
  params$recruit.logSize.sd[is.na(params$recruit.logSize.sd)]=0
  
  #*******************************************************************************
  ### 3F. Create data frame of year-specific parameter estimates ###
  #*******************************************************************************
  
  params=data.frame(params)

  #*******************************************************************************
  ### 3G. Number of seeds per fruit ###
  #*******************************************************************************
  
  seeds.per.year=tapply(data$SeedCt,data$Year,FUN=min,na.rm=T) # obtain mean seed counts per fruit per year
  seeds.per.year=data.frame(seeds.per.year,rownames(seeds.per.year)) # make into a data frame
  colnames(seeds.per.year)=c("seed.ct","Year") # define column names for merging
  params=merge(params,seeds.per.year,by.x="Year",by.y="Year") # year-specific seed counts; note that this only works because both data frames are ordered in the same way!

  #*******************************************************************************
  ### 3H. Establishment probability ###
  #*******************************************************************************
  
  # Obtain number of new recruits per year
  recruit.number=tapply(data$logSizeNext[is.na(data$logSize)],data$Year[is.na(data$logSize)],FUN="length") %>% data.frame()
  colnames(recruit.number)="recruit.number"

  # Obtain total fruit count per year
  fruits.per.year=tapply(year_fruit_count_data$Fec1[!is.na(year_fruit_count_data$Fec1)],year_fruit_count_data$Year[!is.na(year_fruit_count_data$Fec1)],sum)
  
  # Obtain total seed count per year (= # fruits per year * # seeds per fruit per year)
  total.seeds.per.year=fruits.per.year*seeds.per.year$seed.ct	
  
  # Estimate establishment probability as # of new recruits/# of seeds
  params$establishment.prob=recruit.number$recruit.number/total.seeds.per.year
  
  # Store parameters in .csv file for later use
  write.csv(params,"R_output/temporal_demog_compensation_TableS7_FigS7/vital_rate_coefficients_south.csv",row.names=FALSE)
  
#*******************************************************************************
### 4. Create site-specific IPMs parameterized by site-specific parameters derived from global vital rates models 
#*******************************************************************************

  #*******************************************************************************
  ### 4A. Create survival, growth, and fecundity functions
  #*******************************************************************************
  
  # create empty vector for lambda to be filled
  lambda=c()
      
  for (f in 1:length(year)) {
    data1=subset(data,Year==year[f])
    params1=subset(params,Year==year[f]) %>% select(-Year)
    
	#*******************************************************************************
    ### 4B. Create survival, growth, and fecundity functions and build IPM by running integral_projection_model.R script
    #*******************************************************************************
    
    source("R_scripts/integral_projection_model.R")
    
    #*******************************************************************************
    ### 4C. Obtain lambda estimate for year f 
    #*******************************************************************************
    
    # obtain lambda estimate
    (lambda[f] <- Re(eigen(K)$values[1]))
     } # end loop to run IPMs and estimate lambdas for each year
        
#*******************************************************************************
### 5. Merge site information with lambda estimates and save to .csv file
#*******************************************************************************

# Create data frame of Year & lambda
lambda.year=data.frame(unique(data$Year),lambda)
colnames(lambda.year)=c("Year","lambda")

# save to .csv file 
write.csv(lambda.year,"R_output/temporal_demog_compensation_TableS7_FigS7/lambda_south.csv",row.names=FALSE)


