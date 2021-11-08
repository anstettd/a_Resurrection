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
spei_gather$Year <- gsub('2007',"01/07/2007",spei_gather$Year)
spei_gather$Year <- gsub('2008',"01/07/2008",spei_gather$Year)
spei_gather$Year <- gsub('2009',"01/07/2009",spei_gather$Year)
spei_gather$Year <- gsub('2010',"01/07/2010",spei_gather$Year)
spei_gather$Year <- gsub('2011',"01/07/2011",spei_gather$Year)
spei_gather$Year <- gsub('2012',"01/07/2012",spei_gather$Year)
spei_gather$Year <- gsub('2013',"01/07/2013",spei_gather$Year)
spei_gather$Year <- gsub('2014',"01/07/2014",spei_gather$Year)
spei_gather$Year <- gsub('2015',"01/07/2015",spei_gather$Year)
spei_gather$Year <- gsub('2016',"01/07/2016",spei_gather$Year)

y3$Year <- gsub('2007',"01/07/2007",y3$Year)
y3$Year <- gsub('2008',"01/07/2008",y3$Year)
y3$Year <- gsub('2009',"01/07/2009",y3$Year)
y3$Year <- gsub('2010',"01/07/2010",y3$Year)
y3$Year <- gsub('2011',"01/07/2011",y3$Year)
y3$Year <- gsub('2012',"01/07/2012",y3$Year)
y3$Year <- gsub('2013',"01/07/2013",y3$Year)
y3$Year <- gsub('2014',"01/07/2014",y3$Year)
y3$Year <- gsub('2015',"01/07/2015",y3$Year)
y3$Year <- gsub('2016',"01/07/2016",y3$Year)

#Write out new files
#write.csv(spei_gather,"Data/spei9.csv")
#write.csv(y3,"Data/y9.csv")


