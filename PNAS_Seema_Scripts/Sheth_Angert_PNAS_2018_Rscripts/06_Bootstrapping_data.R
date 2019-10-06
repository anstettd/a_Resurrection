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

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. bring in M. cardinalis demography data from 2010-2014 ###
#*******************************************************************************

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

# read in data and sort by latitude
data=read.csv("Data/Mcard_demog_data_2010-2013.csv") %>% arrange(-Latitude)

# convert Site, Year and ID columns to factors
data$Site=factor(data$Site)
data$Year=factor(data$Year)
data$ID=factor(data$ID)
data$NotARecruit=factor(data$NotARecruit)
data$NotAnIndividual=factor(data$NotAnIndividual)

# Remove plants that should not have been recorded as new recruits
#### NOTE: these are plants that A. Angert noted as "wrong, definitely exclude (reasons include new plot, site not visited in prior year, ID within prior years' ranges, coordinates well outside of prior year's search)"
data=subset(data,NotARecruit!=1|is.na(NotARecruit))
unique(data$NotARecruit)
length(data$Site) # 16971 rows; NOTE: there are 8 rows in which NotAnIndividual=1 & NotARecruit=1

# obtain seed counts per fruit per site
seeds.per.site=tapply(data$SeedCt,data$Site,FUN=mean,na.rm=T) # obtain seed counts per fruit per site
seeds.per.site=data.frame(seeds.per.site,rownames(seeds.per.site)) # make into a data frame
colnames(seeds.per.site)=c("seed.ct","Site") # define column names for merging

#*******************************************************************************
#### 2. Create nested loop to obtain replicate bootstrap datasets for each site, sampling with replacement ###
#*******************************************************************************

# Obtain a data frame of unique IDs from each Site
ID.by.Site=unique(data[,1:2])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(data$Site)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
seed=123

# Set number of bootstrap replicate datasets
n.boot=2000

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.site=subset(data,Site==site[i]) # select data from site i
  id.site=subset(ID.by.Site,Site==site[i]) # select list of unique individual IDs from site i
  id.boot <- lapply(1:n.boot, function(j) { 
    set.seed(j+seed)
    sample_n(id.site,size=nrow(id.site), replace = T)}) %>% ldply() # resample rows of site i's data with replacement and size=number of unique individuals in original dataset for each site and convert list to data frame
  
  id.boot$Replicate=rep(seq(1:n.boot),each=nrow(id.site)) # create a column in data frame that corresponds to bootstrap replicate
  data.boot=join(id.boot,data.site,type="left",match="all") # merge bootstrapped list of unique IDs to full dataset
  data.boot.rep[[i]]=data.boot # add each site's dataframe of n.boot bootstrap replicates to list
}

# Convert list to data frame
bootstrapped.data <- do.call(rbind, data.boot.rep) 

# Write bootstrapped datasets to .rds file
saveRDS(bootstrapped.data,"R_output/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013.rds")  
