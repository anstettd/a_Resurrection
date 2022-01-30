##########################################################################################################
## Use climwin to analyse climate data
## Find window that influences SPRI
## Author Daniel Anstett
## 
##
## Last Modified November 8, 2021
##########################################################################################################
##########################################################################################################
# Clear environment
rm(list = ls())

# Get this package retrieving function
## This function will automatically load packages that you already have
## and will install packages you don't yet have then load them
ipak <- function(pkg){
  # Function written by Dr. Evan Fricke
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

# Define the packages that the script needs
myPackages <- c("tidyverse")

# Load the packages
ipak(myPackages)

##########################################################################################################

#Get SPRI for 2010-2016 plus 1 and 2 year lags
#Get 1 and 2 year lags for all anomaly values








#Import data
spei_full <- read.csv("Data/spei_gather.csv", header=T) #Import SPEI data
spei_gather <- spei_full %>% select(Year,SPEI,Site) #Reformat
y3 <- read.csv("Data/y3.csv", header=T) #Import trait data

#Change sites codes to match Anstett et al. 2021 Evolution Letters
y3$Site <- gsub('S02',"1",y3$Site)
y3$Site <- gsub('S11',"12",y3$Site)
y3$Site <- gsub('S07',"2",y3$Site)
y3$Site <- gsub('S10',"3",y3$Site)
y3$Site <- gsub('S08',"4",y3$Site)
y3$Site <- gsub('S32',"5",y3$Site)

y3$Site <- gsub('S29',"6",y3$Site)
y3$Site <- gsub('S18',"7",y3$Site)
y3$Site <- gsub('S17',"8",y3$Site)
y3$Site <- gsub('S16',"9",y3$Site)
y3$Site <- gsub('S36',"12",y3$Site)
y3$Site <- gsub('S15',"11",y3$Site)

#Make a Site/Year Varaible with for cliamte and for y3
spei_extra <- spei_gather %>% unite(col="Site_Year", c("Site","Year"),sep="_") 
#%>% select(Site_Year)
#spei_gather <- cbind(spei_gather,spei_extra)

y3_extra <- y3 %>% unite(col="Site_Year", c("Site","Year"),sep="_") %>% select(Site_Year)
y3 <- cbind(y3,y3_extra)

y9 <- left_join(y3,spei_extra,by="Site_Year")








