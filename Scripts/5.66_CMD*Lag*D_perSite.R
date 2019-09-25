#################
# CMDA * Lag_1 * Drought per site
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

# Site 2
y4 <- y3 %>% filter(Site=="S02")
full.S2.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S2.exp, xvar="CMD.anom.s",by="CMD.anom.1.s")
visreg(full.S2.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S2.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S2.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S2.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S2.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S2.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S2.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S2.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S2.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S2.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S2.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 11
y4 <- y3 %>% filter(Site=="S11") 
full.S11.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
visreg(full.S11.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S11.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S11.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S11.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S11.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S11.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S11.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S11.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S11.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S11.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S11.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 7
y4 <- y3 %>% filter(Site=="S07") 
full.S7.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S7.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S7.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S7.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S7.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S7.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S7.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S7.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S7.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S7.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S7.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S7.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 10
y4 <- y3 %>% filter(Site=="S10") 
full.S10.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S10.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S10.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S10.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S10.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S10.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S10.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S10.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S10.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S10.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S10.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S10.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)


# Site 08
y4 <- y3 %>% filter(Site=="S08") 
full.S08.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S8.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S8.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S8.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S08.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S8.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S8.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S8.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S08.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 32
y4 <- y3 %>% filter(Site=="S32") 
full.S32.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S32.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S32.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S32.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S32.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S32.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S32.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S32.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S32.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S32.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S32.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S32.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 29
y4 <- y3 %>% filter(Site=="S29") 
full.S29.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S29.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S29.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S29.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S29.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S29.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S29.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S8.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S29.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S8.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 18
y4 <- y3 %>% filter(Site=="S18") 
full.S18.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S18.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S18.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S18.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S18.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S18.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S18.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S18.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S18.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S18.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S18.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S18.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 17
y4 <- y3 %>% filter(Site=="S17") 
full.S17.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S17.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S17.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S17.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S17.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S17.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S17.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S17.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S17.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S17.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S17.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S17.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 16
y4 <- y3 %>% filter(Site=="S16") 
full.S16.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S16.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S16.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S16.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S16.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S16.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S16.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S16.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S16.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

no.lag.CMD.S16.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S16.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)



# Site 36
y4 <- y3 %>% filter(Site=="S36") 
full.S36.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
visreg(full.S36.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S36.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S36.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S36.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S36.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S36.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S36.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S36.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S36.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S36.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S36.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

# Site 15
y4 <- y3 %>% filter(Site=="S15") 
full.S15.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S15.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S15.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S15.exp, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S15.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S15.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S15.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S15.SLA, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)

full.S15.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
visreg(full.S15.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-2),overlay=T)
visreg(full.S15.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=-0),overlay=T)
visreg(full.S15.A, xvar="CMD.anom.s",by="Drought",cond=list(CMD.anom.1.s=2),overlay=T)


















full.S2.wc<- lmer(Water_Content ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S2.wc<- lmer(Water_Content ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S2.wc,two.way.S2.wc) # Select full 2-way
no.lag.CMD.S2.wc<- lmer(Water_Content ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(two.way.S2.wc,no.lag.CMD.S2.wc) #Remove lag*CMD, accept no.lag.CMD.S2.wc
CMD.D.S2.wc<- lmer(Water_Content ~ CMD.anom.1.s + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(no.lag.CMD.S2.wc,CMD.D.S2.wc) #Select CMD.D.S2.wc
noX.S2.wc<- lmer(Water_Content ~ CMD.anom.1.s + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(CMD.D.S2.wc,noX.S2.wc) #Select no interactions, select noX.S2.wc
no.lag.S2.wc <- lmer(Water_Content ~ CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(noX.S2.wc,no.lag.S2.wc) #Remove lag
D.S2.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(no.lag.S2.wc,D.S2.wc) #Remove CMDA
nothing.S2.wc <- lmer(Water_Content ~ (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(D.S2.wc,nothing.S2.wc) #No fixed effects not different from Drought only model. Select nothing.S2.wc


full.S2.gs<- lmer(Stomatal_Conductance ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S2.gs<- lmer(Stomatal_Conductance ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S2.gs,two.way.S2.gs) # No difference, select simpler model
no.lag.CMD.S2.gs<- lmer(Stomatal_Conductance ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(two.way.S2.gs,no.lag.CMD.S2.gs) #Marginal support for 2-way model
CMD.D.S2.gs<- lmer(Stomatal_Conductance ~ CMD.anom.1.s + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(no.lag.CMD.S2.gs,CMD.D.S2.gs) #Select CMD.D.S2.gs
noX.S2.gs<- lmer(Stomatal_Conductance ~ CMD.anom.1.s + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(CMD.D.S2.gs,noX.S2.gs) #Select no interactions, select noX.S2.gs
no.lag.S2.gs <- lmer(Stomatal_Conductance ~ CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(noX.S2.gs,no.lag.S2.gs) #Remove lag
D.S2.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(no.lag.S2.gs,D.S2.gs) #Remove CMDA
nothing.S2.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(D.S2.gs,nothing.S2.gs) #No fixed effects not different from Drought only model. Select nothing.S2.gs



fl.S2 <- ggeffect(full.S2.exp, terms = c("CMD.anom.s","Drought"))
plot(fl.S2)
ggplot(fl.S2, aes(x,predicted,colour=Drought))


