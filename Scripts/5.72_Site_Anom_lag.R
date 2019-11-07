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
#Best model:  4-way interaction
four_way.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.exp,fullmod.cmd.exp) #Select 4-way model


##### % Water Content
#Best model: Drought main effect only
four_way.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
full_1.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.s*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.wc,full_1.cmd.wc) #Select simpler model
full_2.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.1.s + CMD.anom.s*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_1.cmd.wc,full_2.cmd.wc) #Select simpler model
full_3.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_2.cmd.wc,full_3.cmd.wc) #Select simpler model
full_4.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_3.cmd.wc,full_4.cmd.wc) #Select simpler model
fullmod.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_3.cmd.wc,fullmod.cmd.wc) #Select simpler model

twomod_1.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s + Site.Lat*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(fullmod.cmd.wc,twomod_1.cmd.wc) #Select simpler model
twomod_2.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s + Site.Lat*Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_1.cmd.wc,twomod_2.cmd.wc) #Select simpler model
twomod_3.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_2.cmd.wc,twomod_3.cmd.wc) #Select simpler model
twomod_4.cmd.wc <- lmer(Water_Content ~ Site.Lat + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_3.cmd.wc,twomod_4.cmd.wc) #Select simpler model
twomod_4.cmd.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_3.cmd.wc,twomod_4.cmd.wc) #Select simpler model
twomod_5.cmd.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_4.cmd.wc,twomod_5.cmd.wc) #Retain model with just drought

##### % SLA
#Best model:  4-way interaction
four_way.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(four_way.cmd.SLA,fullmod.cmd.SLA) #Select 4-way model

##### % Stomatal Conductance
#Best model: Drought main effect only
four_way.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
full_1.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.s*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.gs,full_1.cmd.gs) #Select simpler model
full_2.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.1.s + CMD.anom.s*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_1.cmd.gs,full_2.cmd.gs) #Select simpler model
full_3.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + Site.Lat*CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_2.cmd.gs,full_3.cmd.gs) #Select simpler model
full_4.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + CMD.anom.1.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_3.cmd.gs,full_4.cmd.gs) #Select simpler model
fullmod.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(full_3.cmd.gs,fullmod.cmd.gs) #Select simpler model

twomod_1.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s + Site.Lat*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(fullmod.cmd.gs,twomod_1.cmd.gs) #Select simpler model
twomod_2.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s + Site.Lat*Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_1.cmd.gs,twomod_2.cmd.gs) #Select simpler model
twomod_3.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
#model failed to converge
twomod_4.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_2.cmd.gs,twomod_4.cmd.gs) #Select simpler model
twomod_4.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_3.cmd.gs,twomod_4.cmd.gs) #Select simpler model
twomod_5.cmd.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_4.cmd.gs,twomod_5.cmd.gs) #Retain model with just drought
twomod_6.cmd.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block) + (1|Year),
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(twomod_5.cmd.gs,twomod_6.cmd.gs) #Model with no fixed effect is better.

##### Assimilation
#Best model:  4-way interaction
four_way.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(four_way.cmd.A,fullmod.cmd.A) #Select 4-way model


