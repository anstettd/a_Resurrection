#################
# Generation of one .csv file for all data in resurrection project
#################
# Also assessment of normality
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

### Data prep
Y <- read.csv("Data/drought1.csv", header=T) #Read in basic site,year...etc data
flower1<-read.csv("Data/flower_date_ver2.csv", header=T) #Add in flowering time data
colnames(flower1)[1]<-"Order1"; colnames(flower1)[5]<-"Flowering_Date" #Re-lable variable names
flower1[,6]<-flower1[,5]-101 ; colnames(flower1)[6]<-"Experiment_Date" #Make Experiment Date variable (Flowering Date - 101)
y1<-left_join(Y,flower1,by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment")) #join flower & basic info
rapid<-read.csv("Data/rapid.csv", header=T) #Import physical traits
y2<-left_join(y1,rapid, by=c("Order"="Order2", "Family"="Family", "Block"="Block", "Drought"="Treatment")) # Physical with flowering
y2[,19]<-y2$Area/y2$D_Mass ; colnames(y2)[19]<-"SLA" #Calculate SLA
y2[,20]<-y2$D_Mass/y2$W_Mass ; colnames(y2)[20]<-"Water_Content" #Calculate Water Content fraction

#Make a categorical site variable that is ordered by latitude
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv") %>% 
  select(ID_Year1,Latitude,Longitude) %>%
  separate(ID_Year1, into = c("Site", "Year"), sep = "_") #separate Site from year into two different variables 
wna1$Site <- as.factor(wna1$Site) ; wna1$Year <- as.numeric(wna1$Year) #define Site and Year
y3 <- left_join(y2, wna1, by=c("Site", "Year")) #Add in Lat and Long
y3 <- y3 %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_")) #Generate new catagorical variable

###### Bring in photosythesis point measures data set (Mimulus 2018) ##
#point_measure_error<-read.csv("Data/mimulusjuly2018.csv", header=T) #data set still with errors
point_measure<-read.csv("Data/point_measure_input.csv", header=T) #still wrong
point_measure<-read.csv("Data/point_measure_3.csv", header=T)
point_measure<- point_measure %>% select(Plant.ID,Block,Treatment, gsw, A) #Select wanted data
colnames(point_measure)<-c("Family", "Block", "Drought", "Stomatal_Conductance", "Assimilation") #rename
point_measure <- point_measure %>% mutate(ID.B.D = paste(Family, Block, Drought, sep="_")) #Make categorical variable to designate each triplicate
#use grou_by and summarize to take the mean of each triplicate and generate dataset copatable for left_join with y3
point_measure_join<- point_measure%>% group_by(Family, Block, Drought, ID.B.D) %>% 
  summarise_at(c("Stomatal_Conductance", "Assimilation"), mean, na.rm=TRUE)
#joing Haley's summarized data with full data set
y3 <- left_join(y3, point_measure_join, by=c("Family", "Block", "Drought"))

####### Data Import Climate and Anomaly #########  
### Add in climate and weather covariates
wna <- read_csv("Climate/timeseries_lat_Normal_1981_2010Y.csv") %>% 
  select(Site=ID, MAT.clim=MAT,MAP.clim=MAP,CMD.clim=CMD) %>% mutate(log.MAP.clim = log(MAP.clim))
wna$Site <- as.factor(wna$Site)
write.csv(wna,'Data/wna.csv') #Export file

# Weather for the years 2010-2016; use these to calculate anomalies
wna2 <- read_csv("Data/weather.csv") #Import
wna2 <- wna2 %>% #Selects MAT, MAP, CMD
  select(ID_Year1,Latitude,Longitude,Elevation,MAT.weath=MAT,MAP.weath=MAP,CMD.weath=CMD) %>% 
  mutate(log.MAP.weath = log(MAP.weath)) %>% 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_") #makes site/year variable
wna2$Site <- as.factor(wna2$Site) ; wna2$Year <- as.numeric(wna2$Year) #define variables

# join climate and weather, calculate anomaly, scale everything
wna_all <- left_join(wna2, wna, by="Site") %>% 
  mutate(CMD.anom = CMD.weath - CMD.clim, #switched order
         MAT.anom = MAT.weath - MAT.clim,
         MAP.anom = log.MAP.weath - log.MAP.clim, #reverted to log scale
         CMD.clim.s = as.vector(scale(CMD.clim)),
         MAT.clim.s = as.vector(scale(MAT.clim)),
         MAP.clim.s = as.vector(scale(log.MAP.clim)),
         CMD.weath.s = as.vector(scale(CMD.weath)),
         MAT.weath.s = as.vector(scale(MAT.weath)),
         MAP.weath.s = as.vector(scale(log.MAP.weath)),
         CMD.anom.s = as.vector(scale(CMD.anom)),
         MAT.anom.s = as.vector(scale(MAT.anom)),
         MAP.anom.s = as.vector(scale(MAP.anom)))
write.csv(wna_all,'Data/wna_all.csv') #Export file
#remove Latitude and Longitude from wna_all
wna_all<- wna_all %>% select(-c(Latitude,Longitude))

# join all data into one frame
y3 <- left_join(y3, wna_all, by=c("Site"="Site", "Year"="Year"))


#Scale all the main traits
y3 <- y3 %>% mutate(Experiment_Date.s = scale(Experiment_Date),
                    SLA.s = scale(SLA),
                    Water_Content.s = scale(Water_Content),
                    #Structure.s = scale (Structure), #don't scale binary variables
                    #Wilted.s = scale(Wilted),
                    Stomatal_Conductance.s = scale(Stomatal_Conductance),
                    Assimilation.s = scale(Assimilation),
                    Biomass.s = scale(Biomass))

write.csv(y3,'Data/y3.csv') #Export file
