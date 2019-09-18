#################
# CMDA * D versus lag_1 * D per site
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



# Site 2
y4 <- y3 %>% filter(Site=="S02")
CMDA.D.S2.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S2.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S2.exp,CMDA_1.D.S2.exp) #Select lag_1

CMDA.D.S2.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S2.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S2.SLA,CMDA_1.D.S2.SLA) #Select lag_1

CMDA.D.S2.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S2.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S2.A,CMDA_1.D.S2.A) #Select CMDA

# Site 11
y4 <- y3 %>% filter(Site=="S11") 
CMDA.D.S11.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S11.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S11.exp,CMDA_1.D.S11.exp) #Select CMDA

CMDA.D.S11.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S11.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S11.SLA,CMDA_1.D.S11.SLA) #Select CMDA

CMDA.D.S11.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S11.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S11.A,CMDA_1.D.S11.A) #Select CMDA


# Site 7
y4 <- y3 %>% filter(Site=="S07") 
CMDA.D.S7.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S7.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S7.exp,CMDA_1.D.S7.exp) #Select CMDA

CMDA.D.S7.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S7.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S7.SLA,CMDA_1.D.S7.SLA) #Select CMDA

CMDA.D.S7.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S7.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S7.A,CMDA_1.D.S7.A) #Select CMDA


# Site 10
y4 <- y3 %>% filter(Site=="S10") 
CMDA.D.S10.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S10.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S10.exp,CMDA_1.D.S10.exp) #Select CMDA

CMDA.D.S10.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S10.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S10.SLA,CMDA_1.D.S10.SLA) #Select CMDA

CMDA.D.S10.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S10.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S10.A,CMDA_1.D.S10.A) #Select CMDA

# Site 08
y4 <- y3 %>% filter(Site=="S08") 
CMDA.D.S8.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S8.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S8.exp,CMDA_1.D.S8.exp) #Select Lag

CMDA.D.S8.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S8.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S8.SLA,CMDA_1.D.S8.SLA) #Select Lag

CMDA.D.S8.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S8.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S8.A,CMDA_1.D.S8.A) #Select Lag

# Site 32
y4 <- y3 %>% filter(Site=="S32") 
CMDA.D.S32.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S32.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S32.exp,CMDA_1.D.S32.exp) #Select CMDA

CMDA.D.S32.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S32.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S32.SLA,CMDA_1.D.S32.SLA) #Select CMDA

CMDA.D.S32.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S32.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S32.A,CMDA_1.D.S32.A) #Select CMDA

# Site 29
y4 <- y3 %>% filter(Site=="S29") 
CMDA.D.S29.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S29.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S29.exp,CMDA_1.D.S29.exp) #Select Lag

CMDA.D.S29.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S29.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S29.SLA,CMDA_1.D.S29.SLA) #Select Lag

CMDA.D.S29.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S29.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S29.A,CMDA_1.D.S29.A) #Select Lag

# Site 18
y4 <- y3 %>% filter(Site=="S18") 
CMDA.D.S18.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S18.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S18.exp,CMDA_1.D.S18.exp) #Select lag

CMDA.D.S18.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S18.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S18.SLA,CMDA_1.D.S18.SLA) #CMDA

CMDA.D.S18.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S18.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S18.A,CMDA_1.D.S18.A) #Select Lag

# Site 17
y4 <- y3 %>% filter(Site=="S17") 
CMDA.D.S17.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S17.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S17.exp,CMDA_1.D.S17.exp) #Select Lag

CMDA.D.S17.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S17.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S17.SLA,CMDA_1.D.S17.SLA) #Select Lag

CMDA.D.S17.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S17.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S17.A,CMDA_1.D.S17.A) #Select CMDA

# Site 16
y4 <- y3 %>% filter(Site=="S16") 
CMDA.D.S16.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S16.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S16.exp,CMDA_1.D.S16.exp) #Select Lag

CMDA.D.S16.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S16.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S16.SLA,CMDA_1.D.S16.SLA) #Select Lag

CMDA.D.S16.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S16.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S16.A,CMDA_1.D.S16.A) #Select Lag

# Site 36
y4 <- y3 %>% filter(Site=="S36") 
CMDA.D.S36.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S36.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S36.exp,CMDA_1.D.S36.exp) #Select CMDA

CMDA.D.S36.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S36.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S36.SLA,CMDA_1.D.S36.SLA) #Select CMDA

CMDA.D.S36.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S36.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S36.A,CMDA_1.D.S36.A) #Select Lag

# Site 15
y4 <- y3 %>% filter(Site=="S15") 
CMDA.D.S15.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S15.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S15.exp,CMDA_1.D.S15.exp) #Select CMDA

CMDA.D.S15.SLA <- lmer(SLA ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S15.SLA <- lmer(SLA ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S15.SLA,CMDA_1.D.S15.SLA) #Select CMDA

CMDA.D.S15.A <- lmer(Assimilation ~ CMD.anom.s*Drought + (1|Family) + (1|Block),data=y4)
CMDA_1.D.S15.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S15.A,CMDA_1.D.S15.A) #Select CMDA

