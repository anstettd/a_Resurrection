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
y.d.exp<- lmer(Experiment_Date ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_d.exp<- lmer(Experiment_Date ~ Year*Drought + (Drought|Site.Lat/Family) + (1|Block),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d.exp,y.d_d.exp) #select model with Drought intercept

#Flowering Time 1|
no_X.exp<- lmer(Experiment_Date ~ Year + Drought + (1|Site.Lat/Family) + (1|Block),
                           control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.exp,no_X.exp) #select interactions model
summary(y.d.exp)

#Flowering Time Drought|
no_X.exp<- lmer(Experiment_Date ~ Year + Drought + (Drought|Site.Lat/Family) + (1|Block),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.exp,no_X.exp) #no difference select main effect model
no_year.exp<- lmer(Experiment_Date ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
                                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X,no_year.exp) #no difference, select main effect model
no.D.exp<- lmer(Experiment_Date ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.exp,no.D.exp) #no difference, select main effect model


#Water Content
y.d.wc<- lmer(Water_Content ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.wc<- lmer(Water_Content ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_d.wc<- lmer(Water_Content ~ Year*Drought + (Drought|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d.wc,y.d_d.wc) #select model with Drought intercept?

#wc 1|
no_X.wc.1<- lmer(Water_Content ~ Year + Drought + (1|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.wc,no_X.wc.1) #select simpler model
no_year.wc.1<- lmer(Water_Content ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc.1,no_year.wc.1) #no difference, select main effect model
no.D.wc.1<- lmer(Water_Content ~ (Drought|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc.1,no.D.wc.1) #Select Drought only model



#wc Drought|
no_X.wc<- lmer(Water_Content ~ Year + Drought + (Drought|Site.Lat/Family) + (1|Block),
                           control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.wc,no_X.wc) #No interaction model prefered
no_year.wc<- lmer(Water_Content ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc,no_year.wc) #no difference, select main effect model
no.D.wc<- lmer(Water_Content ~ (Drought|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc,no.D.wc) #Select Drought only model




