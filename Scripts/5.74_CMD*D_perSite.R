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
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

# Site 2
y4 <- y3 %>% filter(Site=="S02")
full.S2.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S2.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S2.exp,two.way.S2.exp) # Select 3-way

full.S2.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S2.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S2.SLA,two.way.S2.SLA) # Select 3-way model

full.S2.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S2.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S2.A,two.way.S2.A) # Select 3-way

# Site 11
y4 <- y3 %>% filter(Site=="S11") 
full.S11.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
two.way.S11.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S11.exp,two.way.S11.exp) # Select 3-way

full.S11.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S11.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S11.SLA,two.way.S11.SLA) # Select 3-way model

full.S11.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S11.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S11.A,two.way.S11.A) # Select 3-way


# Site 7
y4 <- y3 %>% filter(Site=="S07") 
full.S7.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S7.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S7.exp,two.way.S7.exp) # Select 3-way

full.S7.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S7.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S7.SLA,two.way.S7.SLA) # Select 3-way model

full.S7.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S7.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S7.A,two.way.S7.A) # Select 3-way, marginally significant



# Site 10
y4 <- y3 %>% filter(Site=="S10") 
full.S10.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S10.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S10.exp,two.way.S10.exp) # Select 3-way, marginally significant

full.S10.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S10.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S10.SLA,two.way.S10.SLA) # Select 3-way model

full.S10.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S10.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S10.A,two.way.S10.A) # Select 3-way


# Site 08
y4 <- y3 %>% filter(Site=="S08") 
full.S08.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S08.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S08.exp,two.way.S08.exp) # Select 3-way

full.S08.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S08.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4) #error messages
lrtest(full.S08.SLA,two.way.S08.SLA) # Select 3-way model

full.S08.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S08.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4) #error messages
lrtest(full.S08.A,two.way.S08.A) # Select 3-way


# Site 32
y4 <- y3 %>% filter(Site=="S32") 
full.S32.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S32.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S32.exp,two.way.S32.exp) # Select 3-way

full.S32.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S32.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S32.SLA,two.way.S32.SLA) # Select 3-way model

full.S32.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S32.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S32.A,two.way.S32.A) # Select 3-way

# Site 29
y4 <- y3 %>% filter(Site=="S29") 
full.S29.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S29.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S29.exp,two.way.S29.exp) # Select 3-way

full.S29.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S29.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S29.SLA,two.way.S29.SLA) # Select 3-way model

full.S29.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S29.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S29.A,two.way.S29.A) # Select 3-way


# Site 18
y4 <- y3 %>% filter(Site=="S18") 
full.S18.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S18.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S18.exp,two.way.S18.exp) # Select 3-way

full.S18.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S18.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S18.SLA,two.way.S18.SLA) # Select 3-way model

full.S18.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S18.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S18.A,two.way.S18.A) # Select 3-way


# Site 17
y4 <- y3 %>% filter(Site=="S17") 
full.S17.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S17.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S17.exp,two.way.S17.exp) # Select 3-way

full.S17.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S17.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4) #error messages
lrtest(full.S17.SLA,two.way.S17.SLA) # Select 3-way model

full.S17.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), #error messages
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S17.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4) #error messages
lrtest(full.S17.A,two.way.S17.A) # Select 3-way


# Site 16
y4 <- y3 %>% filter(Site=="S16") 
full.S16.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S16.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S16.exp,two.way.S16.exp) # Select 3-way

full.S16.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S16.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S16.SLA,two.way.S16.SLA) # Select 3-way model

full.S16.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
two.way.S16.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
lrtest(full.S16.A,two.way.S16.A) # Not significant select 2-way
no.lag.CMD.S16.A <- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(two.way.S16.A,no.lag.CMD.S16.A) # Select CMDA*D + lag_1*D model. Marginal



# Site 36
y4 <- y3 %>% filter(Site=="S36") 
full.S36.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y4)
two.way.S36.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S36.exp,two.way.S36.exp) # Select 3-way

full.S36.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S36.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S36.SLA,two.way.S36.SLA) # Select 3-way model

full.S36.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S36.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S36.A,two.way.S36.A) # Select 3-way


# Site 15
y4 <- y3 %>% filter(Site=="S15") 
full.S15.exp<- lmer(Experiment_Date ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S15.exp<- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S15.exp,two.way.S15.exp) # Select 3-way

full.S15.SLA<- lmer(SLA ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S15.SLA<- lmer(SLA ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(full.S15.SLA,two.way.S15.SLA) # Select 3-way model

full.S15.A<- lmer(Assimilation ~ CMD.anom.s*CMD.anom.1.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y4)
two.way.S15.A<- lmer(Assimilation ~ CMD.anom.1.s*Drought + CMD.anom.s*Drought + CMD.anom.1.s*CMD.anom.s + (1|Family) + (1|Block) + (1|Year), data=y4)
lrtest(full.S15.A,two.way.S15.A) # Select 3-way



















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
