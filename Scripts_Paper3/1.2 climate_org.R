##########################################################################################################
## Organize climate lag data into trait spreadsheet
## Standardised Precipitation-Evapotranspiration Index
## Author Daniel Anstett
## 
##
## Last Modified Feb 3, 2022
##########################################################################################################

# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)
##########################################################################################################
#Import files
spei_pop <- read.csv("Data/spei_pop.csv", header=T)

y3 <- read.csv("Data/y3.csv", header=T) #Import trait data
y3$Site_Amy <- y3$Site

##########################################################################################################

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

##########################################################################################################
#Setup SPRI Lags
#Make lag columns
spei_pop_2010 <- spei_pop %>% select(SPEI_2010, SPEI_2009, SPEI_2008, SPEI_2007)
colnames(spei_pop_2010) <- c("SPEI_2010_lag0","SPEI_2010_lag1", "SPEI_2010_lag2","SPEI_2010_lag3")

spei_pop_2011 <- spei_pop %>% select(SPEI_2011, SPEI_2010, SPEI_2009, SPEI_2008)
colnames(spei_pop_2011) <- c("SPEI_2011_lag0","SPEI_2011_lag1", "SPEI_2011_lag2","SPEI_2011_lag3")

spei_pop_2012 <- spei_pop %>% select(SPEI_2012, SPEI_2011, SPEI_2010, SPEI_2009)
colnames(spei_pop_2012) <- c("SPEI_2012_lag0","SPEI_2012_lag1", "SPEI_2012_lag2","SPEI_2012_lag3")

spei_pop_2013 <- spei_pop %>% select(SPEI_2013, SPEI_2012, SPEI_2011, SPEI_2010)
colnames(spei_pop_2013) <- c("SPEI_2013_lag0","SPEI_2013_lag1", "SPEI_2013_lag2","SPEI_2013_lag3")

spei_pop_2014 <- spei_pop %>% select(SPEI_2014, SPEI_2013, SPEI_2012, SPEI_2011)
colnames(spei_pop_2014) <- c("SPEI_2014_lag0","SPEI_2014_lag1", "SPEI_2014_lag2","SPEI_2014_lag3")

spei_pop_2015 <- spei_pop %>% select(SPEI_2015, SPEI_2014, SPEI_2013, SPEI_2012)
colnames(spei_pop_2015) <- c("SPEI_2015_lag0","SPEI_2015_lag1", "SPEI_2015_lag2","SPEI_2015_lag3")

spei_pop_2016 <- spei_pop %>% select(SPEI_2016, SPEI_2015, SPEI_2014, SPEI_2013)
colnames(spei_pop_2016) <- c("SPEI_2016_lag0","SPEI_2016_lag1", "SPEI_2016_lag2","SPEI_2016_lag3")

#Merge with rest of data
spei_pop_basic <- spei_pop %>% select(Site:Long_spei)
spei_lag_columns <- cbind(spei_pop_basic,spei_pop_2010,spei_pop_2011,spei_pop_2012,spei_pop_2013,
                          spei_pop_2014,spei_pop_2015,spei_pop_2016)

#Re-organize columns to get lag data as separate columns 
spei_gather <- spei_lag_columns %>% gather(Year, SPEI,SPEI_2010_lag0:SPEI_2016_lag3)
spei_gather$Year <- gsub("SPEI_","",spei_gather$Year)
spei_lag <- spei_gather %>% separate(Year,c("Year","Lag"),sep="_")
spei_lag <- spei_lag %>% spread(Lag,SPEI) 

#Calculate average across multiple years
spei_lag$lag01 <- (spei_lag$lag0 + spei_lag$lag1)/2
spei_lag$lag012 <- (spei_lag$lag0 + spei_lag$lag1 + spei_lag$lag2)/3
spei_lag$lag0123 <- (spei_lag$lag0 + spei_lag$lag1 + spei_lag$lag2 + spei_lag$lag3)/4

#Make a Site/Year Variable with for climate and for y3
spei_lag$ID2 <- spei_lag$ID
spei_lag$Year2 <- spei_lag$Year
spei_lag <- spei_lag %>% unite(col="ID_Year", c("ID2","Year2"),sep="_")



#Input Lag data into phenotypic dataframe
y9 <- left_join(y3,spei_lag,by="ID_Year")
names(y9)[names(y9) == 'Year.x'] <- 'Year'
write.csv(y9,"Data/y9.csv")













