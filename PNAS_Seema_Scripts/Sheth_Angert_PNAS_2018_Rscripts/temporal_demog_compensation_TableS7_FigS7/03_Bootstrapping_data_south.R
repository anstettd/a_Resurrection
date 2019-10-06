#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Sample unique individuals from each year with replacement to create bootstrap datasets for southern edge (Regions S1 & S2)
############# From each bootstrapped dataset, vital rate models are created and IPMs are run to obtain bootstrapped lambdas
############# Replicate bootstrap datasets will be used to obtain confidence intervals around lambda estimates for each year
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
#### 1. bring in M. cardinalis demography data from 2010-2014 ###
#*******************************************************************************

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

# Create separate object for southern region
data=subset(data,Region=="S1"|Region=="S2")

# obtain mean seed counts per fruit per year
seeds.per.year=tapply(data$SeedCt,data$Year,FUN=min,na.rm=T) # obtain mean seed counts per fruit per year
seeds.per.year=data.frame(seeds.per.year,rownames(seeds.per.year)) # make into a data frame
colnames(seeds.per.year)=c("seed.ct","Year") # define column names for merging

#*******************************************************************************
#### 2. Create nested loop to obtain replicate bootstrap datasets for each year, sampling with replacement ###
#*******************************************************************************

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(data$Site)

# Create a vector of unique years for subsetting
year=unique(data$Year)

# Create empty list to be filled in loop
data.boot.rep=list()

# Set seed for random sampling to obtain reproducible results
seed=123

# Set number of bootstrap replicate datasets
n.boot=2000

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(year)) {
	data.year=subset(data,Year==year[i]) # select data from year i
  	data.boot <- lapply(1:n.boot, function(j) {
  	set.seed(j+seed)
  	sample_n(data.year,size=nrow(data.year), replace = T)}) %>% ldply() # resample rows of year i's data with replacement and size=number of rows in original dataset for each year and convert list to data frame
  	data.boot$Replicate=rep(seq(1:n.boot),each=nrow(data.year)) # create a column in data frame that corresponds to bootstrap replicate
  	data.boot.rep[[i]]=data.boot # add each year's dataframe of n.boot bootstrap replicates to list
	}
	
# Convert list to data frame
bootstrapped.data <- do.call(rbind, data.boot.rep)

# Write bootstrapped datasets to .rds file
saveRDS(bootstrapped.data,"R_output/temporal_demog_compensation_TableS7_FigS7/Mcard_demog_INDIV_BOOTSTRAP_data_2010-2013_yearly_south.rds")  
