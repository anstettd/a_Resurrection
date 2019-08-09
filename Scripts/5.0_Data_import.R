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
point_measure<-read.csv("Data/mimulusjuly2018.csv", header=T) 

# Make variable that sumarizes each set of three within plant replicates
point_measure <- point_measure %>% mutate(ID.B.D = paste(Plant.ID, Block, Treatment, sep="_")) #Make categorical variable to designate each triplicate

#using summarize
point_measure_gs <- point_measure %>% group_by(ID.B.D) %>% summarize(mean(gsw)) 
point_measure_A <- point_measure %>% group_by(ID.B.D) %>% summarize(mean(A)) 
point_measure_1 <- left_join(point_measure_gs,point_measure_A,by=c("ID.B.D"="ID.B.D"))
#can get this far, but then do not have Family, Block and Drought in the dataset, so I can't left join to y3 (full dataset)
point_measure_2 <- point_measure %>% group_by(ID.B.D) %>% summarize_each(funs(mean)) 
#This just doesn'work since I can't average categorical variables


#For loop
point_1 <- data.frame() #Set up data frame
U_ID3<-unique(point_measure$ID.B.D) # Lengh of vector of unique ID.B.D
# take 3 obs and turn into one for A and gs (photosythesis point measures)
for (i in 1:length(U_ID3)){ #establish for loop going from 1 to the length of unique ID vector (U_ID3)
  point.temp <- point_measure %>% filter(ID.B.D==U_ID3[i]) 
  temp.mean.gs <- mean(point.temp$gsw) 
  temp.mean.A <- mean(point.temp$A)
  point_1[i,1]<-unique(point.temp$Plant.ID)
  point_1[i,2]<-unique(point.temp$Site)
  point_1[i,3]<-unique(point.temp$Year)
  point_1[i,4]<-unique(point.temp$Block)
  point_1[i,5]<-unique(point.temp$Treatment)
  point_1[i,6]<-unique(point.temp$ID.B.D)
  point_1[i,7]<-temp.mean.gs
  point_1[i,8]<-temp.mean.A
}
colnames(point_1)<-c("Family", "Site.1", "Year.1", "Block", "Drought", "ID.B.D", "Stomatal_Conductance","Assimilation")

#joing Haley's summarized data with full data set
y3 <- left_join(y3, point_1, by=c("Family", "Block", "Drought"))


####### Data Import Climate and Anomaly #########  
### Add in climate and weather covariates
wna <- read_csv("Climate/timeseries_lat_Normal_1981_2010Y.csv") %>% 
  select(Site=ID, MAT.clim=MAT,MAP.clim=MAP,CMD.clim=CMD) %>% mutate(log.MAP.clim = log(MAP.clim))
wna$Site <- as.factor(wna$Site)
write.csv(wna,'Data/wna.csv') #Export file

# Weather for the years 2010-2016; use these to calculate anomalies
wna2 <- read_csv("Climate/timeseries_lat_2010-2016.csv") #Import
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
                    Assimilation.s = scale(Assimilation))

write.csv(y3,'Data/y3.csv') #Export file
