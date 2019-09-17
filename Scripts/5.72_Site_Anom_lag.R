#################
# Mixed Models comparing 4-way with 3-way
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


### 4-Way models compared to 3-Way models

#####Experiment Date
four_way.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.exp,fullmod.cmd.exp) #Select 4-way model


##### % Water Content
four_way.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.wc,fullmod.cmd.wc) #Select 3-way model
full_1.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_1.cmd.wc,fullmod.cmd.wc) #Select 3-way model

##### % SLA
four_way.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(four_way.cmd.SLA,fullmod.cmd.SLA) #Select 4-way model

##### Stomatal Conductance 
four_way.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
lrtest(four_way.cmd.gs,fullmod.cmd.gs) #Select 3-way model
full_1.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_1.cmd.gs,fullmod.cmd.gs) #Select 3-way model

##### Assimilation
four_way.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.A,fullmod.cmd.A) #Select 4-way model


