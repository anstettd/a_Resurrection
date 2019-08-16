#################
# Dropped mixed models using site, year & drought
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


########################################################################################################################
#Droped models. Some models do not converge, others are too similar to more acurately measured variables.
#Unclear if we will use flower number directly or a relative fitness only.

##### Structure #### Cannot run mixed model on this due to data convergence issues. Consider dropping
#fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
#fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
#fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
#fullmod.str <- glm(Structure ~ Site.Lat*Year*Drought, family=binomial, data=y3) #runs
# drop 3way
#no3way.str <- glmer(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), family=binomial, data=y3) # error message
#no3way.str <- glm(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year, family=binomial, data=y3) # runs
#lrtest(fullmod.str, no3way.str) #Models not significantly different, take simpler model??
#Drop 2way
#noDxY.str <- glm(Structure ~ Site.Lat*Drought + Site.Lat*Year, family=binomial, data=y3) 
#lrtest(no3way.str, noDxY.str) # Models not significnatly different Take simpler model.
#SxYD.str<- glm(Structure ~ Site.Lat*Year + Drought, family=binomial, data=y3)
#lrtest(noDxY.str,SxYD.str) # noDxY signifcantly better. Retain this model.
#Anova(noDxY.str, type = 3) # Site X drought main effect. VisReg not useful with binomial data.

##### log(SLA) #### SLA is to similar to Water Content. Retain analyses for Water Conent only, since its measured more rigorously
#fullmod.SLA <- lmer(log(SLA) ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
#no3way.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
#lrtest(fullmod.SLA, no3way.SLA) #model without 3-way intraction substantially better
# drop 2ways
#noDxY.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
#lrtest(no3way.SLA,noDxY.SLA) #noDxY.bio signifcnatly better, has larger LogLik
#SxYD.SLA<- lmer(log(SLA) ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
#lrtest(noDxY.SLA,SxYD.SLA) #No significant difference, retain simplier model (SxYD.SLA) with greater loglik
#no interactions
#nox.SLA <- lmer(log(SLA) ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
#lrtest(SxYD.SLA,nox.SLA) # no interactions model significantly better.
#noDrought.SLA <- lmer(log(SLA) ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
#lrtest(nox.SLA, noDrought.SLA) # no interactions model significantly better. Retain this model.
#no.year.SLA <- lmer(log(SLA) ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
#lrtest(nox.SLA, no.year.SLA) # Year removed.
#no.site.SLA <- lmer(log(SLA) ~ Drought + (1|Family) + (1|Block), data=y3)
#lrtest(no.year.SLA, no.site.SLA) # Retain just drought
#Anova(no.site.SLA, type = 3) # Year is not significant. Site and drought effect.
#visreg(no.site.SLA, xvar="Drought") # Unclear why its only showing some years.

##### Wilted #### Drop wilted, measure not particuarly rigorous
#fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
#fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
#fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
#fullmod.wil <- glm(Wilted ~ Site.Lat*Year, family=binomial, data=y3) # runs
# drop 2way
#no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
#no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
#no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
#no2way.wil <- glm(Wilted ~ Site.Lat + Year, family=binomial, data=y3) # runs
#lrtest(fullmod.wil, no2way.wil) # models not significantly different, take simpler model.
#noYear.wil <- glm(Wilted ~ Site.Lat, family=binomial, data=y3)
#lrtest(no2way.wil, noYear.wil) #Take simpler model
#Anova(noYear.wil , type = 3) # Nothing significant, not suprising considering how few plants were non-wilted during assessment.
