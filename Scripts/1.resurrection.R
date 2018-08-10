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
y1<-left_join(Y3,flower1,by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment"))


###### Basic Graphing #######

##Select All Wet##
yWet<-y1 %>% 
  filter(Drought=="W") %>% 
  droplevels()
#Box and Wisker Plot
ggplot(yWet, aes(Year, Flowering_Date)) +
  geom_boxplot(aes(group=Year)) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Wet_Wisker.png", width = 5, height = 5)
#Scatter plot
ggplot(yWet, aes(Year, Flowering_Date)) +
  geom_jitter(width = 0.10,size=0.3) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Wet_jitter.png", width = 5, height = 5)

###CMD###
#ggsave("Summary_Graphs/.png", width = 5, height = 5)
attach(yWet)
lmyWCMD<-lm(Flowering_Date~CMD)
summary(lmyWCMD)
visreg(lmyWCMD)

###MAP###
attach(yWet)
lmyMAP<-lm(Flowering_Date~MAP)
summary(lmyMAP)
visreg(lmyMAP) #Earlier floweringtime in sites with lower precipitation! P<0.0001


###MAT### This is a bit strange, will leave out for now.
attach(yWet)
lmyMAT<-lm(Flowering_Date~MAT)
summary(lmyMAT)
visreg(lmyMAT) #Earlier floweringtime in sites with lower temperature? P<0.0001




###Select All Dry##
yDry<-y1 %>% 
  filter(Drought=="D") %>% 
  droplevels()
#Box and Wisker Plot
ggplot(yDry, aes(Year, Flowering_Date)) +
  geom_boxplot(aes(group=Year)) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Dry_Wisker.png", width = 5, height = 5)
#Scatter plot
ggplot(yDry, aes(Year, Flowering_Date)) +
  geom_jitter(width = 0.10,size=0.3) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Dry_jitter.png", width = 5, height = 5)

###CMD###
attach(yDry)
lmyDCMD<-lm(Flowering_Date~CMD)
summary(lmyDCMD)
visreg(lmyDCMD)

###MAP###
attach(yDry)
lmyMAP<-lm(Flowering_Date~MAP)
summary(lmyMAP)
visreg(lmyMAP)

###MAT###
attach(yDry)
lmyMAT<-lm(Flowering_Date~MAT)
summary(lmyMAT)
visreg(lmyMAT)








##### Individual Sites #######

##Wet##
#WetS02
yWetS02<-y1 %>% 
  filter(Drought=="W",Site=="S02") %>% 
  droplevels()
attach(yWetS02)
lmyWS02<-lm(Flowering_Date~Year)
summary(lmyWS02)
visreg(lmyWS02) # significant evolution of earlier flowering time P<0.001

#WetS07
yWetS07<-y1 %>% 
  filter(Drought=="W",Site=="S07") %>% 
  droplevels()
attach(yWetS07)
lmyWS07<-lm(Flowering_Date~Year)
summary(lmyWS07)
visreg(lmyWS07) # significant evolution of later flowering time P=0.0425


#WetS08
yWetS08<-y1 %>% 
  filter(Drought=="W",Site=="S08") %>% 
  droplevels()
attach(yWetS08)
lmyWS08<-lm(Flowering_Date~Year)
summary(lmyWS08)
visreg(lmyWS08) # No significant pattern, yett....

#WetS10
yWetS10<-y1 %>% 
  filter(Drought=="W",Site=="S10") %>% 
  droplevels()
attach(yWetS10)
lmyWS10<-lm(Flowering_Date~Year)
summary(lmyWS10)
visreg(lmyWS10) # significant evolution of earlier flowering time P=0.03

#WetS11
yWetS11<-y1 %>% 
  filter(Drought=="W",Site=="S11") %>% 
  droplevels()
attach(yWetS11)
lmyWS11<-lm(Flowering_Date~Year)
summary(lmyWS11)
visreg(lmyWS11) # No signifiant pattern

#WetS15
yWetS15<-y1 %>% 
  filter(Drought=="W",Site=="S15") %>% 
  droplevels()
attach(yWetS15)
lmyWS15<-lm(Flowering_Date~Year)
summary(lmyWS15)
visreg(lmyWS15) # No signifiant pattern

#WetS16
yWetS16<-y1 %>% 
  filter(Drought=="W",Site=="S16") %>% 
  droplevels()
attach(yWetS16)
lmyWS16<-lm(Flowering_Date~Year)
summary(lmyWS16)
visreg(lmyWS16) # No signifiant pattern

#WetS17
yWetS17<-y1 %>% 
  filter(Drought=="W",Site=="S17") %>% 
  droplevels()
attach(yWetS17)
lmyWS17<-lm(Flowering_Date~Year)
summary(lmyWS17)
visreg(lmyWS17) # significant evolution of earlier flowering time 0.029

#WetS18
yWetS18<-y1 %>% 
  filter(Drought=="W",Site=="S18") %>% 
  droplevels()
attach(yWetS18)
lmyWS18<-lm(Flowering_Date~Year)
summary(lmyWS18)
visreg(lmyWS18) # significant evolution of later flowering time P=0.049

#WetS29
yWetS29<-y1 %>% 
  filter(Drought=="W",Site=="S29") %>% 
  droplevels()
attach(yWetS29)
lmyWS29<-lm(Flowering_Date~Year)
summary(lmyWS29)
visreg(lmyWS29) # No significant pattern, yett....

#WetS32
yWetS32<-y1 %>% 
  filter(Drought=="W",Site=="S32") %>% 
  droplevels()
attach(yWetS32)
lmyWS32<-lm(Flowering_Date~Year)
summary(lmyWS32)
visreg(lmyWS32) # Not significant

#WetS36
yWetS36<-y1 %>% 
  filter(Drought=="W",Site=="S36") %>% 
  droplevels()
attach(yWetS36)
lmyWS36<-lm(Flowering_Date~Year)
summary(lmyWS36)
visreg(lmyWS36) # Not significant








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


## Full Models
# prep factors
y1$Block <- as.factor(y1$Block)
y1$Family <- as.factor(y1$Family)
y1$Year <- as.factor(y1$Year)

# load packages
library(lme4) #for mixed models
library(lmtest) #for LRT
library(visreg) # one way to visualize marginal effects (better for datapoints)
library(ggeffects) # another way to visualize marginal effects (better for CIs)

fullmod <- lmer(Flowering_Date ~ CMD*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod)

# drop 3way
no3way <- lmer(Flowering_Date ~ CMD*Drought + Drought*Year + CMD*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way)
lrtest(fullmod, no3way) #3-way interaction highly significant

visreg(fullmod, xvar="CMD", by="Year") #2013 seems confounded by sampling, otherwise the cline seems to flatten towards more recent years
visreg(fullmod, xvar="CMD", by="Drought", overlay=T) #no apparent differences in plasticity based on historical CMD
visreg(fullmod, xvar="Drought", by="Year")
visreg(fullmod, by="Drought", xvar="Year") #evolution of earlier flowering seen in both wet and dry treatments

preds <- ggeffect(fullmod, terms = c("CMD", "Drought", "Year"))
plot(preds)
