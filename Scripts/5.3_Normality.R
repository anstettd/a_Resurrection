#################
# Test for normality of data
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



#Experiment Date (Flowering Time)
qqnorm(Experiment_Date) #Aprox normal
ggplot(data=y3,aes(x=Experiment_Date))+
  geom_histogram()+theme_classic()

#Flower Number
qqnorm(Flower_num) # trucated left tail
ggplot(data=y3,aes(x=Flower_num))+
  geom_histogram()+theme_classic()

#Biomass
qqnorm(Biomass) # Approx normal
ggplot(data=y3,aes(x=Biomass))+
  geom_histogram()+theme_classic()

#SLA
qqnorm(SLA) #Not normal
ggplot(data=y3,aes(x=SLA))+
  geom_histogram()+theme_classic()

#log SLA
qqnorm(log(SLA)) #Use log
ggplot(data=y3,aes(x=log(SLA)))+
  geom_histogram()+theme_classic()

#Water Content
qqnorm(Water_Content) #Aprox normal
ggplot(data=y3,aes(x=Water_Content))+
  geom_histogram()+theme_classic()

#Stomatal_Conductance
qqnorm(y3$Stomatal_Conductance) # Approx normal
ggplot(data=y3,aes(x=Stomatal_Conductance))+
  geom_histogram()+theme_classic()

#Assimilation
qqnorm(y3$Assimilation) #
ggplot(data=y3,aes(x=Assimilation))+
  geom_histogram()+theme_classic()