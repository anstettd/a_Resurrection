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
library(ggplot2)
### Data prep
Y <- read.csv("Data/drought1.csv", header=T) #specify relative paths within the project folder instead of using setwd

#Add 1981-2010 climate data to drought for average. Not useful.
#wna<-read.csv("Climate/timeseries_lat_Normal_1981_2010Y.csv", header=T)
#y1<-left_join(Y,wna,by=c("Site"="ID"))

#Add in individual info yearly environmental variables
wna1<-read.csv("Climate/timeseries_lat_2010-2016.csv", header=T)
wna2<-wna1 %>% select(ID_Year1,Latitude,Longitude,Elevation,MAT,MAP,CMD)
#Note useful
#ID.Year2<-paste(Y$Site,Y$Year,sep="_")
#colnames(wna2)[1] <- "Site_year1"
#Y2<-merge(ID.Year2,Y)
Y3<-left_join(Y,wna2,by=c("ID_Year"="ID_Year1"))

#Add in flowering time data
flower1<-read.csv("Data/flower_date.csv", header=T)
colnames(flower1)[1]<-"Order1"
colnames(flower1)[5]<-"Flowering_Date"
y1<-left_join(Y3,flower1,by=c("Order"="Order1"))

#Select year and site




# Flowering time evolution
yWet<-y1 %>% 
  filter(Drought=="W") %>% 
  droplevels()
attach(yWet)
lme1.3way<-lme(Flower_Date~CMD*Site*Year, random = Block.x)
summary(lme1.3way)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way 

#Plasticity in flowering time
attach(y1)
lme2.4way<-lme(Flower_Date~CMD*Site*Year*Drought, random = Block)
summary(lme2.4way)
a2.4way<-Anova(lm1.3way, type=3)
a2.4way 





