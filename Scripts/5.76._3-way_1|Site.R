#################
# CMDA * Lag_1 * Drought  + 1|Site
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
full.all.exp <- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
two_way.exp <- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s + CMD.anom.s*Drought + CMD.anom.1.s*Drought + 
                     (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(full.all.exp,two_way.exp) # Not difference, select two_way
noCMD.lag.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(two_way.exp,noCMD.lag.exp) # No difference, Select noCMD.lag.exp
CMD.D.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + CMD.anom.1.s + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(noCMD.lag.exp,CMD.D.exp) # Marginal evidence for nogCMD.lag.exp. Retain more complex model
Anova(noCMD.lag.exp,type=3)

visreg(noCMD.lag.exp, xvar="CMD.anom.s",by="CMD.anom.1.s",cond=list(Drought="W"))
visreg(noCMD.lag.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(noCMD.lag.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(noCMD.lag.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)

#SLA
full.all.SLA <- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
two_way.SLA <- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s + CMD.anom.s*Drought + CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(full.all.SLA,two_way.SLA) # Select full model

Anova(full.all.SLA,type=3)
visreg(full.all.SLA, xvar="CMD.anom.s",by="CMD.anom.1.s",cond=list(Drought="W"))
visreg(full.all.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.all.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.all.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)


#Assimilation
full.all.A <- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
two_way.A <- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s + CMD.anom.s*Drought + CMD.anom.1.s*Drought + 
                      (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(full.all.A,two_way.A) # Not difference, select two_way
noCMD.lag.A <- lmer(Assimilation ~ CMD.anom.s*Drought + CMD.anom.1.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(two_way.A,noCMD.lag.A) # No difference, Select noCMD.lag.A
CMD.D.A <- lmer(Assimilation~ CMD.anom.s*Drought + CMD.anom.1.s + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(noCMD.lag.A,CMD.D.A) # Marginal evidence for nogCMD.lag.A. Retain more complex model
main.A <- lmer(Assimilation ~ CMD.anom.s + Drought + CMD.anom.1.s + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(CMD.D.A,main.A) # No difference, keep simpler model
nolag.A <- lmer(Assimilation ~ CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(main.A,nolag.A) # No difference, keep simpler model
D.A <- lmer(Assimilation ~ Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(nolag.A,D.A) # No difference, keep simpler model
no.main.A <- lmer(Assimilation ~ (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(D.A,no.main.A) # No difference, select no main effect model
