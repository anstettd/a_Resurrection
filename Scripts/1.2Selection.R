#################
# M. cardinalis Rapid Evolution
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
y2[,19]<-y2[,18]/y2[,17]
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

# prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
#y3$Year <- as.factor(y3$Year)

###### Calculate relative finess 
mean_flower_num <- mean(y3$Flower_num)
mean_flower_num <- mean(Flower_num, na.rm=T)
y3 <- y3 %>% mutate(relative_fitness = Flower_num/mean_flower_num) 

#Run selection equation
ns.lmer <- lmer(relative_fitness ~ Flowering_Date + Flowering_Date^2 + SLA + SLA^2 + Water_Content + 
                  Water_Content^2 + Structure + Structure^2 + (1|Block), data=y3)
Anova(ns.lmer)
#do lrtest?





