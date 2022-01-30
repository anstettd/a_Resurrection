##########################################################################################################
## Prep climwin temporal data in correct format
##
## Author Daniel Anstett
## 
##
## Last Modified November 8, 2021
##########################################################################################################
#Import Libraries
library(climwin)
library(dplyr)

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

#Change Year coding to be compatible with climwin
spei_gather$Year <- gsub('2007',"07/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2008',"08/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2009',"09/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2010',"10/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2011',"11/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2012',"12/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2013',"13/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2014',"14/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2015',"15/01/2017",spei_gather$Year)
spei_gather$Year <- gsub('2016',"16/01/2017",spei_gather$Year)

y3$Year <- gsub('2007',"07/01/2017",y3$Year)
y3$Year <- gsub('2008',"08/01/2017",y3$Year)
y3$Year <- gsub('2009',"09/01/2017",y3$Year)
y3$Year <- gsub('2010',"10/01/2017",y3$Year)
y3$Year <- gsub('2011',"11/01/2017",y3$Year)
y3$Year <- gsub('2012',"12/01/2017",y3$Year)
y3$Year <- gsub('2013',"13/01/2017",y3$Year)
y3$Year <- gsub('2014',"14/01/2017",y3$Year)
y3$Year <- gsub('2015',"15/01/2017",y3$Year)
y3$Year <- gsub('2016',"16/01/2017",y3$Year)

#Write out new files
write.csv(spei_gather,"Data/spei9.csv")
write.csv(y3,"Data/y9.csv")


