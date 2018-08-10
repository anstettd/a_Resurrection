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

# how much inter-annual variation in climate was there, relative to average latitudinal clines?
ggplot(Y3, aes(x=Latitude, y=MAT, color=Year)) + geom_point()
ggplot(Y3, aes(x=Latitude, y=MAP, color=Year)) + geom_point()
ggplot(Y3, aes(x=Latitude, y=CMD, color=Year)) + geom_point()

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
visreg(lmyMAP) #Earlier flowering time in sites with lower precipitation! P<0.0001
# Amy note: because the climate values are specific to site x year, you can't interpret this solely as a site effect. in other words, a recent year at a dry site might have a similar MAP as an early year at a wet site. So, more precisely, the result is that earlier flowering time under conditions of low precipitation, whether those arose in different sites or years. this is why you need a full model that accounts for site and year.


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
visreg(lmyWS08) # No significant pattern, yet....

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

# CMD
fullmod.cmd <- lmer(Flowering_Date ~ CMD*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod.cmd)

# drop 3way
no3way.cmd <- lmer(Flowering_Date ~ CMD*Drought + Drought*Year + CMD*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.cmd)
lrtest(fullmod.cmd, no3way.cmd) #3-way interaction highly significant

visreg(fullmod.cmd, xvar="CMD", by="Year") #2013 seems confounded by sampling, otherwise the cline seems to flatten towards more recent years
visreg(fullmod.cmd, xvar="CMD", by="Drought", overlay=T) #no apparent differences in plasticity based on CMD
visreg(fullmod.cmd, xvar="Drought", by="Year")
visreg(fullmod.cmd, by="Drought", xvar="Year") #evolution of earlier flowering seen in both wet and dry treatments

preds.cmd <- ggeffect(fullmod.cmd, terms = c("CMD", "Drought", "Year"))
plot(preds.cmd) 

# MAT
fullmod.mat <- lmer(Flowering_Date ~ MAT*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod.mat)

# drop 3way
no3way.mat <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.mat)
lrtest(fullmod.mat, no3way.mat) #3-way interaction not significant

# drop 2ways
noTxD.mat <- lmer(Flowering_Date ~ Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noTxD.mat) #MAT x Drought not significant
noDxY.mat <- lmer(Flowering_Date ~ MAT*Drought + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noDxY.mat) # Drought x Year significant
noTxY.mat <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noTxY.mat) # MAT x Year not significant

visreg(noDxY.mat, xvar="Drought", by="Year", overlay=T) # I don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.


# MAP
fullmod.map <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod.map)

# drop 3way
no3way.map <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.map)
lrtest(fullmod.map, no3way.map) #3-way interaction highly significant

visreg(fullmod.map, xvar="MAP", by="Year") #2013 has poor sampling, otherwise the cline seems to flatten or even reverse in more recent years
visreg(fullmod.map, xvar="MAP", by="Drought", overlay=T) #cline more apparent in dry treatment than in wet. populations (site x years) sampled from wet conditions have stronger plastic response to drought treatment.
visreg(fullmod.map, xvar="Drought", by="Year")
visreg(fullmod.map, by="Drought", xvar="Year") #2013 is super wonky

preds.map <- ggeffect(fullmod.map, terms = c("MAP", "Drought", "Year"))
plot(preds.map) 


#trying all again with 2013 excluded

fullmod.cmd.no2013 <- lmer(Flowering_Date ~ CMD*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.cmd.no2013)
no3way.cmd.no2013 <- lmer(Flowering_Date ~ CMD*Drought + Drought*Year + CMD*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.cmd.no2013)
lrtest(fullmod.cmd.no2013, no3way.cmd.no2013) #3-way interaction still highly significant

visreg(fullmod.cmd.no2013, xvar="CMD", by="Year") #cline flattens towards more recent years
visreg(fullmod.cmd.no2013, xvar="CMD", by="Drought", overlay=T) #no apparent differences in plasticity based on CMD
visreg(fullmod.cmd.no2013, xvar="Drought", by="Year")
visreg(fullmod.cmd.no2013, xvar="Drought", by="Year", overlay=T)
visreg(fullmod.cmd.no2013, by="Drought", xvar="Year") #evolution of earlier flowering seen in both wet and dry treatments
visreg(fullmod.cmd.no2013, by="Drought", xvar="Year", overlay=T) #evolution of earlier flowering seen in both wet and dry treatments

preds.cmd.no2013 <- ggeffect(fullmod.cmd.no2013, terms = c("CMD", "Drought", "Year"))
plot(preds.cmd.no2013) 


fullmod.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.mat.no2013)
no3way.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.mat.no2013)
lrtest(fullmod.mat.no2013, no3way.mat.no2013) #3-way interaction not significant
noTxD.mat.no2013 <- lmer(Flowering_Date ~ Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noTxD.mat.no2013) #MAT x Drought not significant
noDxY.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noDxY.mat.no2013) # Drought x Year significant
noTxY.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noTxY.mat.no2013) # MAT x Year not significant

visreg(noDxY.mat.no2013, xvar="Drought", by="Year", overlay=T) # I don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.
visreg(noDxY.mat.no2013, xvar="Year", by="Drought", overlay=T) # I don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.


fullmod.map.no2013 <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.map.no2013)
no3way.map.no2013 <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.map.no2013)
lrtest(fullmod.map.no2013, no3way.map.no2013) #3-way interaction still highly significant

visreg(fullmod.map.no2013, xvar="MAP", by="Year") #cline was negative in 2010 by switches to positive in more recent years (strongly so in 2015)
visreg(fullmod.map.no2013, xvar="MAP", by="Drought", overlay=T) #populations sampled under high precip (sites x years) have stronger plastic response to experimental drought
visreg(fullmod.map.no2013, xvar="Drought", by="Year")
visreg(fullmod.map.no2013, xvar="Drought", by="Year", overlay=T) #greater spread among years apparent in drought treatment
visreg(fullmod.map.no2013, by="Drought", xvar="Year")
visreg(fullmod.map.no2013, by="Drought", xvar="Year", overlay=T)

preds.map.no2013 <- ggeffect(fullmod.map.no2013, terms = c("MAP", "Drought", "Year"))
plot(preds.map.no2013) 
