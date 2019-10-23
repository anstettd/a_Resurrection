#################
# Site*Year*Drought 2nd order Mixed Models
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
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#####Experiment Date (flowering time since experiment start date)
fullmod.poly.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3) #3way interaction model
#poly drop 3way
no3way.poly.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2)+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.exp,no3way.poly.exp) #3-way poly supported
anova(fullmod.poly.exp)
summary(fullmod.poly.exp)

##### Water_Content ####
fullmod.poly.wc <- lmer(Water_Content ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
# drop 3way
no3way.poly.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.wc, no3way.poly.wc) # keep 3-way, keep fullmod.poly.wc 
anova(fullmod.poly.wc)
summary(fullmod.poly.wc)

##### SLA ####
#3-way poly
fullmod.poly.SLA <- lmer(SLA ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.SLA, no3way.poly.SLA) # accept 3-way model
anova(fullmod.poly.SLA)
summary(fullmod.poly.SLA)

######## Stomatal Conductance
fullmod.poly.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) 
                       + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.gs, no3way.poly.gs) # keep 3-way
anova(fullmod.poly.gs)
summary(fullmod.poly.gs)

######## Assimilation
fullmod.poly.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) 
                       + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.A, no3way.poly.A) # keep 3-way
anova(fullmod.poly.A)
summary(fullmod.poly.A)








