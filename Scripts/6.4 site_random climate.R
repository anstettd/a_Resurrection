#################
# Site as a random variable, year*Drought
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
library(ggeffects)
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors


#Flowering Time
full.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Site.Lat/Family) + (1|Block) + (1|Year),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
full_y.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought  + (Year|Site.Lat/Family) + (1|Block) + (1|Year),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
full_d.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought  + (Drought|Site.Lat/Family) + (1|Block) + (1|Year),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(full.exp,full_d.exp) #select model with Drought intercept

#Flowering Time Drought|
no_1.D.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s +CMD.anom.s*Drought + CMD.anom.1.s*Drought +
                  (Drought|Site.Lat/Family) + (1|Block) + (1|Year),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(full_d.exp,no_1.D.exp) #no differenc, select no 
no_X.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s + CMD.anom.s*Drought +
                  (Drought|Site.Lat/Family) + (1|Block) + (1|Year),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(full_d.exp,no_X.exp) #no difference select main effect model




#Flowering Time 1|
no_1.D1.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s +CMD.anom.s*Drought + CMD.anom.1.s*Drought + (1|Site.Lat/Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(full.exp,no_1.D1.exp) #select interactions model
summary(y.d.exp)