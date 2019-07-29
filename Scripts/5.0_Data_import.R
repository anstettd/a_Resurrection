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
Y <- read.csv("Data/drought1.csv", header=T)

#Add in flowering time data
flower1<-read.csv("Data/flower_date_ver2.csv", header=T)
colnames(flower1)[1]<-"Order1"
colnames(flower1)[5]<-"Flowering_Date"
flower1[,6]<-flower1[,5]-101
colnames(flower1)[6]<-"Experiment_Date"
#y1<-left_join(Y,flower1,by=c("Order"="Order1"))
y1<-left_join(Y,flower1,by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment"))

#Add in other physical traits
rapid<-read.csv("Data/rapid.csv", header=T)
y2<-left_join(y1,rapid, by=c("Order"="Order2", "Family"="Family", "Block"="Block", "Drought"="Treatment"))
#Calculate SLA and & water content
y2[,19]<-y2[,18]/y2[,17] #better habit to do this by col names instead of col #s so that if you ever reorder anything the calculations won't be screwed up
y2[,20]<-y2[,17]/y2[,16]
colnames(y2)[19]<-"SLA"
colnames(y2)[20]<-"Water_Content"

#Make a categorical site variable that is ordered by latitude
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv") %>%
  select(ID_Year1,Latitude,Longitude) %>% #,Elevation,MAT,MAP,CMD 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_")
wna1$Site <- as.factor(wna1$Site)
wna1$Year <- as.numeric(wna1$Year)

y3 <- left_join(y2, wna1, by=c("Site", "Year"))

y3 <- y3 %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))  
attach(y3)

# Bring in point measures data set (Mimulus 2018)
point_measure<-read.csv("Data/mimulusjuly2018.csv", header=T)
# Make vairable that sumarizes each set of three within plant replicates
point_measure <- point_measure %>% mutate(ID.B.D = paste(Plant.ID, Block, Treatment, sep="_"))  

#Take average of 3 reps per A and gs
point_1 <- data.frame()

# take 3 obs and turn into one for photosythesis point measures
U_ID3<-unique(point_measure$ID.B.D) # Lengh of vector of unique ID.B.D
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
colnames(point_1)<-c("Family", "Site", "Year", "Block", "Drought", "ID.B.D", "Stomatal_Conductance","Assimilation")

#Join point measurements to full data set
y3 <- left_join(y3, point_1, by=c("Family", "Block", "Drought"))








####### Data Import Climate and Anomaly #########  
### Add in climate and weather covariates
wna <- read_csv("Climate/timeseries_lat_Normal_1981_2010Y.csv") %>% 
  select(Site=ID, MAT.clim=MAT,MAP.clim=MAP,CMD.clim=CMD)
wna$Site <- as.factor(wna$Site)

# Weather for the years 2010-2016; use these to calculate anomalies
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv")
wna2 <- wna1 %>% 
  select(ID_Year1,Latitude,Longitude,Elevation,MAT.weath=MAT,MAP.weath=MAP,CMD.weath=CMD) %>% 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_")
wna2$Site <- as.factor(wna2$Site)
wna2$Year <- as.numeric(wna2$Year)

# join climate and weather 
wna_all <- left_join(wna2, wna, by="Site") %>% 
  mutate(CMD.anom = CMD.clim-CMD.weath,
         MAT.anom = MAT.clim-MAT.weath,
         MAP.anom = (MAP.clim)-(MAP.weath), #remove log
         CMD.clim.scaled = as.vector(scale(CMD.clim)),
         MAT.clim.scaled = as.vector(scale(MAT.clim)),
         MAP.clim.scaled = as.vector(scale(MAP.clim)),
         CMD.weath.scaled = as.vector(scale(CMD.weath)),
         MAT.weath.scaled = as.vector(scale(MAT.weath)),
         MAP.weath.scaled = as.vector(scale(MAP.weath)),
         CMD.anom.scaled = as.vector(scale(CMD.anom)),
         MAT.anom.scaled = as.vector(scale(MAT.anom)),
         MAP.anom.scaled = as.vector(scale(MAP.anom)),)

# join all data into one frame
y4 <- left_join(y3, wna_all, by=c("Site.x"="Site", "Year.x"="Year"))

#Scale 
y4 <- y4 %>% mutate(Experiment_Date.scaled = scale(Experiment_Date),
                    SLA.scaled = scale(SLA),
                    Water_Content.scaled = scale(Water_Content),
                    Structure.scaled = scale (Structure),
                    Wilted.scaled = scale(Wilted),
                    Stomatal_Conductance.s = scale(Stomatal_Conductance),
                    Assimilation.s = (Assimilation))










