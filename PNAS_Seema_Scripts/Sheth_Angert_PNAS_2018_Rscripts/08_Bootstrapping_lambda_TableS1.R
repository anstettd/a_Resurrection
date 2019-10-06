#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Sample unique individuals from each site with replacement to create bootstrap datasets
############# From each bootstrapped dataset, vital rate models are created and IPMs are run to obtain bootstrapped lambdas
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

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. Preliminaries ###
#*******************************************************************************

# Read in & examine bootstrapped data 
bootstrapped.data=readRDS("R_output/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013.rds")
str(bootstrapped.data)

# obtain mean seed counts per fruit per site
seeds.per.site=tapply(bootstrapped.data$SeedCt,bootstrapped.data$Site,FUN=min,na.rm=T) # obtain mean seed counts per fruit per site
seeds.per.site=data.frame(seeds.per.site,rownames(seeds.per.site)) # make into a data frame
colnames(seeds.per.site)=c("seed.ct","Site") # define column names for merging

# Read in & examine bootstrapped coefficients from vital rate models
surv.reg_boot=readRDS("R_output/surv.reg_boot.rds")
growth.reg_boot=readRDS("R_output/growth.reg_boot.rds")
flowering.reg_boot=readRDS("R_output/flowering.reg_boot.rds")
fruit.reg_boot=readRDS("R_output/fruit.reg_boot.rds")

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(bootstrapped.data$Site)

# Set number of bootstrap replicate datasets
n.boot=2000

#*******************************************************************************
#### 2. Obtain vital rate parameters across all sites for each replicate bootstrap dataset ###
#*******************************************************************************

# Create empty list to be filled in loop
params.boot=list()

# Begin loop to extract vital rate parameters for each bootstrapped dataset
for (k in 1:n.boot) {
  data.rep=subset(bootstrapped.data,Replicate==k) # select data from replicate k
  
  # Obtain total fruit count for each indivdiual at each site in each year, including monster plants
  site_fruit_count_data=subset(data.rep,select=c(Site,Fec1)) 
  
  # Remove monster plants where individuals were not distinguished
  #### NOTE: these are plants that A. Angert noted as "not ok, definitely exclude from survival, growth, and fecundity but ok for seed input denominator for recruitment (history of lumping/splitting/relumping; redundant IDs)"
  data.rep=subset(data.rep,NotAnIndividual!=1|is.na(NotAnIndividual))
  
  # Set up data frame of model parameters
  params=c()
  
  #*******************************************************************************
  ### 3A. Survival ###
  #*******************************************************************************
  
  # Read in model coefficients
  surv.reg=surv.reg_boot[[k]]
   
  # Store model coefficients
  params$surv.int=coefficients(surv.reg)$Site[,1] 
  params$surv.slope=coefficients(surv.reg)$Site[,2] 
  params$Site=rownames(coefficients(surv.reg)$Site)
  
  # Make into data frame
  params=data.frame(params)
  
  #*******************************************************************************
  ### 3B. Growth ###
  #*******************************************************************************
  
  # Read in model coefficients
  growth.reg=growth.reg_boot[[k]]
  
  # Store model coefficients
  growth.params=c()
  growth.params$growth.int=coefficients(growth.reg)$Site[,1]  
  growth.params$growth.slope=coefficients(growth.reg)$Site[,2]
  growth.params$Site=rownames(coefficients(growth.reg)$Site)
  growth.params=data.frame(growth.params)
  params=join(params,growth.params,by="Site")
  params$growth.sd=rep(sigma(growth.reg),times=length(site)) 
  
  #*******************************************************************************
  ### 3C. Flowering ###
  #*******************************************************************************
  
  # Read in model coefficients
  flowering.reg=flowering.reg_boot[[k]]
  
  # Store model coefficients
  params$flowering.int=coefficients(flowering.reg)$Site[,1] 
  params$flowering.slope=coefficients(flowering.reg)$Site[,2] 
  
  #*******************************************************************************
  ### 3D. Fruit number (untransformed) using negative binomial regression ###
  #*******************************************************************************
  
  # Read in model coefficients
  fruit.reg=fruit.reg_boot[[k]]
  
  # Store model coefficients
  fruit.int=fixef(fruit.reg)[1]+ranef(fruit.reg)$Site[,1] %>% data.frame()
  fruit.int$Site=rownames(fruit.int)
  colnames(fruit.int)=c("fruit.int","Site")
  fruit.slope=fixef(fruit.reg)[2]+ranef(fruit.reg)$Site[,2] %>% data.frame()
  fruit.slope$Site=rownames(fruit.slope)
  colnames(fruit.slope)=c("fruit.slope","Site")
  params=join(params,fruit.int,by="Site") %>% join(fruit.slope,by="Site")
  params$fruit.int[is.na(params$fruit.int)]=0
  params$fruit.slope[is.na(params$fruit.slope)]=0
  
  #*******************************************************************************
  ### 3E. Size distribution of recruits ###
  #*******************************************************************************
  recruit.size.mean=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Site[is.na(data.rep$logSize)],FUN="mean") %>% data.frame()
  recruit.size.sd=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Site[is.na(data.rep$logSize)],FUN="sd") %>% data.frame()
  
  params$recruit.logSize.mean=recruit.size.mean[,1]  
  params$recruit.logSize.sd=recruit.size.sd[,1]  
  
  params$recruit.logSize.mean[is.na(params$recruit.logSize.mean)]=0
  params$recruit.logSize.sd[is.na(params$recruit.logSize.sd)]=0
  
  #*******************************************************************************
  ### 3F. Number of seeds per fruit ###
  #*******************************************************************************
  
  params=merge(params,seeds.per.site,by.x="Site",by.y="Site") # site-specific seed counts
  
  #*******************************************************************************
  ### 3G. Establishment probability ###
  #*******************************************************************************
  
  # Obtain number of new recruits per site
  recruit.number=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Site[is.na(data.rep$logSize)],FUN="length") %>% data.frame()
  colnames(recruit.number)="recruit.number"
  
  # Obtain total fruit count per site 
  fruits.per.site=tapply(site_fruit_count_data$Fec1[!is.na(site_fruit_count_data$Fec1)],site_fruit_count_data$Site[!is.na(site_fruit_count_data$Fec1)],sum)
  
  # Obtain total seed count per site (= # fruits per site * # seeds per fruit per site)
  total.seeds.per.site=fruits.per.site*seeds.per.site$seed.ct	
  
  # Estimate establishment probability as # of new recruits/# of seeds
  params$establishment.prob=recruit.number$recruit.number/total.seeds.per.site
  
  # Set establishment probability as 0 for Hauser Creek (was calculated as NA because Hauser creek has 0 new recruits)
  params$establishment.prob[is.na(params$establishment.prob)|params$establishment.prob=="Inf"]=0	
  
  # Store data frame of parameter values for a given bootstrap replicate dataset into list
  params$Replicate=rep(k,each=nrow(params)) # create a column in data frame that corresponds to bootstrap replicate
  params.boot[[k]]=params
  print(k)
  } # end loop

# remove large objects
rm(surv.reg_boot)
rm(growth.reg_boot)
rm(flowering.reg_boot)
rm(fruit.reg_boot)
  
# Convert list of bootstrapped vital rate parameters to data frame
bootstrapped.params <- do.call(rbind, params.boot)
  
# Write bootstrapped parameter estimates to .csv file
write.csv(bootstrapped.params,"R_output/Mcard_demog_INDIV_BOOTSTRAP_params_2010-2013.csv",row.names=FALSE)  
  
  #*******************************************************************************
  ### 4. Create site-specific IPMs parameterized by site-specific parameters derived from global vital rates models 
  #*******************************************************************************

# Create empty list to be filled in loop
lambda.boot=list()

# Begin loop to extract vital rate parameters for each bootstrapped dataset
for (k in 1:n.boot) {
  data.rep=subset(bootstrapped.data,Replicate==k) # select data from replicate k
  params=subset(bootstrapped.params,Replicate==k) # select parameters from replicate K
  
# Remove monster plants where individuals were not distinguished; do this again when estimating lambda to restrict size range
#### NOTE: these are plants that A. Angert noted as "not ok, definitely exclude from survival, growth, and fecundity but ok for seed input denominator for recruitment (history of lumping/splitting/relumping; redundant IDs)"
data.rep=subset(data.rep,NotAnIndividual!=1|is.na(NotAnIndividual))
  
  #*******************************************************************************
  ### 4A. Set up loop to create site-specific vital rate functions and IPMs
  #*******************************************************************************
  
  # create empty vector for lambda and siteID to be filled
  lambda=c()
  siteID=c()
  
  for (j in 1:length(site)) {
    data1=subset(data.rep,Site==site[j])
    params1=subset(params,Site==site[j])
    params1=subset(params1,select=-Site)
    
    # write if else statement so that if all individuals in bootstrap died, lambda is manually set equal to 0
    
    if(length(data1$logSizeNext[!is.na(data1$logSizeNext)])>0)
      
    {
      
      #*******************************************************************************
      ### 4B. Create survival, growth, and fecundity functions and build IPM by running integral_projection_model.R script
      #*******************************************************************************
      
      source("R_scripts/integral_projection_model.R")
      
      #*******************************************************************************
      ### 4C. Obtain lambda
      #*******************************************************************************
      
      # obtain lambda estimate
      (lambda[j] <- Re(eigen(K)$values[1])) 
      
      # obtain site name corresponding to lambda estimate
      siteID[j]=as.character(unique(data1$Site))
      
    } # end if loop
    else{
      lambda[j]=0     
      siteID[j]=as.character(unique(data1$Site))} # end else loop
    
    # merge lambda estimate with site name
    lambda.site=data.frame(siteID,lambda)
    print(siteID[j]) # print site ID to keep track of what is currently running
  } # end among-site loop
  
  lambda.boot[[k]]=lambda.site
  print(k) # print replicate number to keep track of how many bootstraps are complete
} # end among-replicate loop

#*******************************************************************************
### 5. Write bootstrapped lambda and vital rate parameter estimates to .csv file
#*******************************************************************************

# Convert list of bootstrapped lambdas to data frame and arrange by Site
bootstrapped.lambda <- do.call(rbind, lambda.boot) %>% arrange(siteID)
bootstrapped.lambda$Replicate=rep(seq(1,n.boot,1),times=length(site))

# write bootstrapped lambda estimates to .csv
write.csv(bootstrapped.lambda,"R_output/Mcard_demog_INDIV_BOOTSTRAP_lambda_2010-2013.csv",row.names=FALSE) 