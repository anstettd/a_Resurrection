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
spei9<-na.omit(spei9)

MassWin <- slidingwin(exclude=NA, xvar = list(SPEI = spei9$SPEI),
                      cdate = spei9$Year,
                      bdate = wflower$Year,
                      baseline = lm(Experiment_Date ~ 1, data = wflower),
                      cinterval = "day",
                      range = c(3, 0),
                      type = "relative",
                      stat = "mean",
                      cmissing = FALSE,
                      func = "lin", spatial = list(wflower$Site, spei9$Site))

#Get info
head(MassWin[[1]]$Dataset)
MassWin[[1]]$BestModel
head(MassWin[[1]]$BestModelData)






