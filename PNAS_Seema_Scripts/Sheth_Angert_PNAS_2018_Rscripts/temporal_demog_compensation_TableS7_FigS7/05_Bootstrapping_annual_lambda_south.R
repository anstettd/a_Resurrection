#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Sample unique individuals from each site with replacement to create bootstrap datasets
############# From each bootstrapped dataset, vital rate models are created and IPMs are run to obtain bootstrapped lambdas at southern edge (Regions S1 & S2)
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

# obtain mean seed counts per fruit per year
seeds.per.year=tapply(bootstrapped.data$SeedCt,bootstrapped.data$Year,FUN=min,na.rm=T) # obtain mean seed counts per fruit per year
seeds.per.year=data.frame(seeds.per.year,rownames(seeds.per.year)) # make into a data frame
colnames(seeds.per.year)=c("seed.ct","Year") # define column names for merging

# Read in & examine bootstrapped coefficients from vital rate models
surv.reg_boot=readRDS("R_output/temporal_demog_compensation_TableS7_FigS7/surv.reg_boot_south.rds")
growth.reg_boot=readRDS("R_output/temporal_demog_compensation_TableS7_FigS7/growth.reg_boot_south.rds")
flowering.reg_boot=readRDS("R_output/temporal_demog_compensation_TableS7_FigS7/flowering.reg_boot_south.rds")
fruit.reg_boot=readRDS("R_output/temporal_demog_compensation_TableS7_FigS7/fruit.reg_boot_south.rds")

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(bootstrapped.data$Site)

# Create a vector of unique years for subsetting
year=unique(bootstrapped.data$Year)

# Set number of bootstrap replicate datasets
n.boot=2000

#*******************************************************************************
#### 3. Obtain vital rate parameters across all years for each replicate bootstrap dataset ###
#*******************************************************************************

# Create empty list to be filled in loop
lambda.boot=list()
params.boot=list()

for (k in 1:n.boot) {
  data.rep=subset(bootstrapped.data,Replicate==k) # select data from replicate k
  
  # Obtain total fruit count for each indivdiual in each year, including monster plants
  year_fruit_count_data=subset(data.rep,select=c(Year,Fec1)) 
  
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
  params$surv.int=coefficients(surv.reg)$Year[,1] 
  params$surv.slope=coefficients(surv.reg)$Year[,2] 
  params$Year=rownames(coefficients(surv.reg)$Year)

  # Make into data frame
  params=data.frame(params)
  
  #*******************************************************************************
  ### 3B. Growth ###
  #*******************************************************************************
  
  # Read in model coefficients
  growth.reg=growth.reg_boot[[k]]
  
  # Store model coefficients
  growth.params=c()
  growth.params$growth.int=coefficients(growth.reg)$Year[,1]  
  growth.params$growth.slope=coefficients(growth.reg)$Year[,2]
  growth.params$Year=rownames(coefficients(growth.reg)$Year)
  growth.params=data.frame(growth.params)
  params=join(params,growth.params,by="Year")
  params$growth.sd=rep(sigma(growth.reg),times=length(year)) 
  
  #*******************************************************************************
  ### 3C. Flowering ###
  #*******************************************************************************
  
  # Read in model coefficients
  flowering.reg=flowering.reg_boot[[k]]
  
  # Store model coefficients
  params$flowering.int=coefficients(flowering.reg)$Year[,1] 
  params$flowering.slope=coefficients(flowering.reg)$Year[,2] 
  
  #*******************************************************************************
  ### 3D. Fruit number (untransformed) using negative binomial regression ###
  #*******************************************************************************
  
  # Read in model coefficients
  fruit.reg=fruit.reg_boot[[k]]
  
  # Store model coefficients
  fruit.int=fixef(fruit.reg)[1]+ranef(fruit.reg)$Year[,1] %>% data.frame()
  fruit.int$Year=rownames(fruit.int)
  colnames(fruit.int)=c("fruit.int","Year")
  fruit.slope=rep((fixef(fruit.reg)[2]+0),each=4) %>% data.frame() # Note this step is because there is a constant slope for fruit # across all years
  fruit.slope$Year=params$Year # Note this step is because there is a constant slope for fruit # across all years
  colnames(fruit.slope)=c("fruit.slope","Year")
  params=join(params,fruit.int,by="Year") %>% join(fruit.slope,by="Year")
  params$fruit.int[is.na(params$fruit.int)]=0
  params$fruit.slope[is.na(params$fruit.slope)]=0
  
  #*******************************************************************************
  ### 3E. Size distribution of recruits ###
  #*******************************************************************************
  recruit.size.mean=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Year[is.na(data.rep$logSize)],FUN="mean") %>% data.frame()
  recruit.size.sd=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Year[is.na(data.rep$logSize)],FUN="sd") %>% data.frame()
  
  params$recruit.logSize.mean=recruit.size.mean[,1]  
  params$recruit.logSize.sd=recruit.size.sd[,1]  
  
  params$recruit.logSize.mean[is.na(params$recruit.logSize.mean)]=0
  params$recruit.logSize.sd[is.na(params$recruit.logSize.sd)]=0

  #*******************************************************************************
  ### 3F. Number of seeds per fruit ###
  #*******************************************************************************
  
  params=merge(params,seeds.per.year,by.x="Year",by.y="Year") # year-specific seed counts

  #*******************************************************************************
  ### 3G. Establishment probability ###
  #*******************************************************************************
  
  # Obtain number of new recruits per year
  recruit.number=tapply(data.rep$logSizeNext[is.na(data.rep$logSize)],data.rep$Year[is.na(data.rep$logSize)],FUN="length") %>% data.frame()
  colnames(recruit.number)="recruit.number"
  
  # Obtain total fruit count per year
  fruits.per.year=tapply(year_fruit_count_data$Fec1[!is.na(year_fruit_count_data$Fec1)],year_fruit_count_data$Year[!is.na(year_fruit_count_data$Fec1)],sum)
  
  # Obtain total seed count per year (= # fruits per year * # seeds per fruit per year)
  total.seeds.per.year=fruits.per.year*seeds.per.year$seed.ct	
  
  # Estimate establishment probability as # of new recruits/# of seeds
  params$establishment.prob=recruit.number$recruit.number/total.seeds.per.year
  
  # Set establishment probability as 0 for Hauser Creek (was calculated as NA because Hauser creek has 0 new recruits)
  params$establishment.prob[is.na(params$establishment.prob)|params$establishment.prob=="Inf"]=0	
  
  # Store data frame of parameter values for a given bootstrap replicate dataset into list
  params$Replicate=rep(k,each=nrow(params)) # create a column in data frame that corresponds to bootstrap replicate
  params.boot[[k]]=params
  
#*******************************************************************************
### 4. Create year-specific IPMs parameterized by year-specific parameters derived from global vital rates models 
#*******************************************************************************

  #*******************************************************************************
  ### 4A. Set up loop to create year-specific vital rate functions and IPMs
  #*******************************************************************************
  
  # create empty vector for lambda and yearID to be filled
  lambda=c()
  yearID=c()
  
  for (j in 1:length(year)) {
    data1=subset(data.rep,Year==year[j])
    params1=subset(params,Year==year[j])
    params1=subset(params1,select=-Year)
    
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
    
    # obtain year corresponding to lambda estimate
   yearID[j]=as.character(unique(data1$Year))
    
    } # end if loop
  else{
      lambda[j]=0     
    yearID[j]=as.character(unique(data1$Year))} # end else loop
      	
    # merge lambda estimate with year name
    lambda.year=data.frame(yearID,lambda)
    print(yearID[j]) # print year ID to keep track of what is currently running
    } # end among-year loop
  
    lambda.boot[[k]]=lambda.year
    print(k) # print replicate number to keep track of how many bootstraps are complete
    } # end among-replicate loop

#*******************************************************************************
### 5. Write bootstrapped lambda estimates to .csv file
#*******************************************************************************

# Convert list to data frame and arrange by Year
bootstrapped.lambda <- do.call(rbind, lambda.boot) %>% arrange(yearID)
bootstrapped.lambda$Replicate=rep(seq(1,n.boot,1),times=length(year))

write.csv(bootstrapped.lambda,"R_output/temporal_demog_compensation_TableS7_FigS7/Mcard_demog_INDIV_BOOTSTRAP_lambda_2010-2013_yearly_south.csv",row.names=FALSE)  

# Convert list to data frame
bootstrapped.params <- do.call(rbind, params.boot)

# Write bootstrapped parameter estimates to .csv file
write.csv(bootstrapped.params,"R_output/temporal_demog_compensation_TableS7_FigS7/Mcard_demog_INDIV_BOOTSTRAP_params_2010-2013_yearly_south.csv",row.names=FALSE)  
