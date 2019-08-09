#################
# Generation of Site/Year means data frame
#################
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggeffects)
library(lmtest)
library(glmmTMB)

y3 <- read.csv("Data/y3.csv", header=T)
#### Set up data frames
trait.means.w <- data.frame() #Set up Wet Treatment data frame
trait.means.d <- data.frame() #Set up Drought Treatment data frame
y3.w<- y3 %>% filter(Drought=="W") #Filter for Wet treatment data
y3.d<- y3 %>% filter(Drought=="D") #Filter for Drought treatment data

### get trait-mean time series per site wtihin Dry Treatment

# this code accomplishes the same as lines 30-54 below, more efficiently and with fewer opportunities to introduce errors via typos
site.year.means.dry <- y3.d %>% group_by(Site, Year, Latitude, Longitude, Drought) %>% summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)
write.csv(site.year.means.dry,'Data/trait.means.w.csv') #Export file
### get traits means for dourght across year-site for Wet Treatment

# this code accomplishes the same as lines 62-86 below, more efficiently and with fewer opportunities to introduce errors via typos
site.year.means.wet <- y3.w %>% group_by(Site, Year, Latitude, Longitude, Drought) %>% summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)
write.csv(site.year.means.wet ,'Data/trait.means.w.csv') #Export file




#U_IDs<-unique(y3.d$ID_Year)
#for (i in 1:length(U_IDs)){
#  tmp.mean.df<-y3.d %>% filter(ID_Year==U_IDs[i])
#  tmp.mean.fl<-mean(tmp.mean.df$Experiment_Date, na.rm=TRUE) #Generate mean per site/year
#  tmp.mean.wc<-mean(tmp.mean.df$Water_Content, na.rm=TRUE)
#  tmp.mean.SLA<-mean(tmp.mean.df$SLA, na.rm=TRUE)
#  tmp.mean.gs<-mean(tmp.mean.df$Stomatal_Conductance, na.rm=TRUE) #Generate mean per site/year
#  tmp.mean.A<-mean(tmp.mean.df$Assimilation, na.rm=TRUE)
#  tmp.mean.bio<-mean(tmp.mean.df$Biomass, na.rm=TRUE)
  
#  trait.means.d[i,1]<-unique(tmp.mean.df$ID_Year)  
#  trait.means.d[i,2]<-unique(tmp.mean.df$Site)
#  trait.means.d[i,3]<-unique(tmp.mean.df$Year)
#  trait.means.d[i,4]<-unique(tmp.mean.df$Latitude)
#  trait.means.d[i,5]<-unique(tmp.mean.df$Longitude)
#  trait.means.d[i,6]<-unique(tmp.mean.df$Drought)
#  trait.means.d[i,7]<-tmp.mean.fl
#  trait.means.d[i,8]<-tmp.mean.wc
#  trait.means.d[i,9]<-tmp.mean.SLA
#  trait.means.d[i,10]<-tmp.mean.gs
#  trait.means.d[i,11]<-tmp.mean.A
#  trait.means.d[i,12]<-tmp.mean.bio
#}
#colnames(trait.means.d)<-c("ID_Year", "Site", "Year", "Latitude", "Longitude", "Drought", "Experiment_Date",
#                           "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass")
#write.csv(trait.means.d,'Data/trait.means.d.csv') #Export file

#U_IDs<-unique(y3.w$ID_Year)
#for (i in 1:length(U_IDs)){
#  tmp.mean.df<-y3.w %>% filter(ID_Year==U_IDs[i])
#  tmp.mean.fl<-mean(tmp.mean.df$Experiment_Date, na.rm=TRUE) #Generate mean per site/year
#  tmp.mean.wc<-mean(tmp.mean.df$Water_Content, na.rm=TRUE)
#  tmp.mean.SLA<-mean(tmp.mean.df$SLA, na.rm=TRUE)
#  tmp.mean.gs<-mean(tmp.mean.df$Stomatal_Conductance, na.rm=TRUE)
#  tmp.mean.A<-mean(tmp.mean.df$Assimilation, na.rm=TRUE)
#  tmp.mean.bio<-mean(tmp.mean.df$Biomass, na.rm=TRUE)
  
#  trait.means.w[i,1]<-unique(tmp.mean.df$ID_Year)  
#  trait.means.w[i,2]<-unique(tmp.mean.df$Site)
#  trait.means.w[i,3]<-unique(tmp.mean.df$Year)
#  trait.means.w[i,4]<-unique(tmp.mean.df$Latitude)
#  trait.means.w[i,5]<-unique(tmp.mean.df$Longitude)
#  trait.means.w[i,6]<-unique(tmp.mean.df$Drought)
#  trait.means.w[i,7]<-tmp.mean.fl
#  trait.means.w[i,8]<-tmp.mean.wc
#  trait.means.w[i,9]<-tmp.mean.SLA
#  trait.means.w[i,10]<-tmp.mean.gs
#  trait.means.w[i,11]<-tmp.mean.A
#  trait.means.w[i,12]<-tmp.mean.bio
#}
#colnames(trait.means.w)<-c("ID_Year", "Site", "Year", "Latitude", "Longitude", "Drought", "Experiment_Date",
#                          "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass")
#write.csv(trait.means.w,'Data/trait.means.w.csv') #Export file


