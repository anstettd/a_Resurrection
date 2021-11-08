##########################################################################################################
## Use climwin to analyse climate data
## Find window that influences SPRI
## Author Daniel Anstett
## 
##
## Last Modified November 8, 2021
##########################################################################################################
#Import Libraries
library(climwin)
library(dplyr)

#Import data
spei9 <- read.csv("Data/spei9.csv", header=T) #Import SPEI data
y9 <- read.csv("Data/y9.csv", header=T) #Import trait data

## Wet, flowering date
wflower <- y9 %>% filter(Drought=="W") %>% select(Year,Experiment_Date,Site) #Reformat
wflower <- na.omit(wflower)


MassWin <- slidingwin(xvar = list(Temp = spei_gather$Temp),
                      cdate = spei_gather$Year,
                      bdate = wflower$Year,
                      baseline = lm(Mass ~ 1, data = Mass),
                      cinterval = "year",
                      range = c(48, 0),
                      type = "relative",
                      stat = "mean",
                      func = "lin", spatial = list(y3w_flower$Site, spei_gather$SiteID))







