#################
# Mixed Models 2 year lag versus 1 year
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


### 2-year CMD Sum model
y3<-y3 %>% mutate(CMDA_2y=CMD.anom + CMD.anom.1)
y3<-y3 %>% mutate(CMDA_2y.s=scale(CMDA_2y))

#####Experiment Date
two_year.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMDA_2y.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.exp,fullmod.cmd.exp) #Select two-year model
four_way.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.exp,four_way.cmd.exp) #Select 4-way model




##### % Water Content
two_year.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMDA_2y.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.wc,fullmod.cmd.wc) #Select 3-way model

##### % SLA
two_year.cmd.SLA <- lmer(SLA ~ Site.Lat*CMDA_2y.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(two_year.cmd.SLA,fullmod.cmd.SLA) #Select 2-year model
four_way.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.SLA,four_way.cmd.SLA) #Select 4-way model




##### Stomatal Conductance 
two_year.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMDA_2y.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
lrtest(two_year.cmd.gs,fullmod.cmd.gs) #Select 3-way model


##### Assimilation
two_year.cmd.A <- lmer(Assimilation ~ Site.Lat*CMDA_2y.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
fullmod.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.A,fullmod.cmd.A) #Select 2-year model
four_way.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(two_year.cmd.A,four_way.cmd.A) #Select 4-way model

