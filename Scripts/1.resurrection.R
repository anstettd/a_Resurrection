###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
### Data prep
Y <- read.csv("Data/drought.csv", header=T) #specify relative paths within the project folder instead of using setwd

#Add 1981-2010 climate data to drought
#wna<-read.csv("Climate/timeseries_lat_Normal_1981_2010Y.csv", header=T)
#y1<-left_join(Y,wna,by=c("Site"="ID"))

#Add in individual info yearly environmental variables
wna1<-read.csv("Climate/timeseries_lat_2010-2016.csv", header=T)
#add dummy variable to wna1
ID.Year1<-paste(wna$ID,wna$Year)
wna2<-merge(ID.Year1,wna1)
#add dummy variable to Y2
ID.Year2<-paste(Y$Site,Y$Year)
Y2<-
y2<-left_join(Y2,wna2,by=c("Site"="ID"|"x"="x"))



# Flowering time evolution
yWet<-y1 %>% 
  filter(Drought=="W") %>% 
  droplevels()
attach(yWet)
lme1.3way<-lme(Flower_Date~CMD*Site*Year, random = Block)
summary(lme1.3way)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way 

#Plasticity in flowering time
attach(y1)
lme2.4way<-lme(Flower_Date~CMD*Site*Year*Drought, random = Block)
summary(lme2.4way)
a2.4way<-Anova(lm1.3way, type=3)
a2.4way 





