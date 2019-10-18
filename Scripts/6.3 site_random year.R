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
#y3$Year <- as.numeric(y3$Year)
y3.10site<- y3 %>% filter(Site.Lat!="36.7_S08" & Site.Lat!="41.7_S17") %>% droplevels()
y3.5year<- y3 %>% filter(Year!="2011" & Year!="2013") %>% droplevels()
y3.site.y<- y3.10site %>% filter(Year!="2011" & Year!="2013") %>% droplevels()

#Filter for Wet treatment data
#Flowering Time
y.d.exp<- lmer(Experiment_Date ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site) #failed to converge
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site) #failed to converge
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.5year) #failed to converge
y.d_y.exp<- lmer(Experiment_Date ~ Year*Drought + (Year|Site.Lat) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.site.y) #failed to converge

y.d_y_main.exp<- lmer(Experiment_Date ~ Year + Drought + (Year|Site.Lat),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #converges but no 1|block
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
no_year.wc.1<- lmer(Water_Content ~ Drought + (1|Site.Lat/Family) + (1|Block),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X.wc.1,no_year.wc.1) # select drought model
no.D.wc.1<- lmer(Water_Content ~ (1|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc.1,no.D.wc.1) #Select Drought only model

#wc Drought|
no_X.wc<- lmer(Water_Content ~ Year + Drought + (Drought|Site.Lat/Family) + (1|Block),
                           control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.wc,no_X.wc) #No interaction model prefered
no_year.wc<- lmer(Water_Content ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X.wc,no_year.wc) #Drought only model prefered
no.D.wc<- lmer(Water_Content ~ (Drought|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.wc,no.D.wc) #Select Drought only model


#SLA
y.d.SLA<- lmer(SLA ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.SLA<- lmer(SLA ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_d.SLA<- lmer(SLA ~ Year*Drought + (Drought|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d.SLA,y.d_d.SLA) #select model with Drought intercept?

#SLA 1|
no_X.SLA.1<- lmer(SLA ~ Year + Drought + (1|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.SLA,no_X.SLA.1) #select interaction model
summary(y.d.SLA)
Anova(y.d.SLA)

#SLA Drought|
no_X.SLA<- lmer(SLA ~ Year + Drought + (Drought|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.SLA,no_X.SLA) #Marginally perfer interaction model
summary(y.d_d.SLA)
Anova(y.d_d.SLA)


#Stomatal Conductance
y.d.gs<- lmer(Stomatal_Conductance ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.gs<- lmer(Stomatal_Conductance ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_d.gs<- lmer(Stomatal_Conductance ~ Year*Drought + (Drought|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3) #failed to converge
lrtest(y.d.gs,y.d_d.gs) #no model significantly better. What to do?

#gs 1|
no_X.gs.1<- lmer(Stomatal_Conductance ~ Year + Drought + (1|Site.Lat/Family) + (1|Block),data=y3)
lrtest(y.d_d.gs,no_X.gs.1) # No difference, select simpler model
no_year.gs.1<- lmer(Stomatal_Conductance ~ Drought + (1|Site.Lat/Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X.gs.1,no_year.gs.1) #Select simpler model
no.D.gs.1<- lmer(Stomatal_Conductance ~ Year + (1|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3) 
lrtest(no_year.gs.1,no.D.gs.1) #Select drought model


#Assimilation
y.d.A<- lmer(Assimilation ~ Year*Drought + (1|Site.Lat/Family) + (1|Block), data=y3)
y.d_y.A<- lmer(Assimilation ~ Year*Drought + (Year|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #failed to converge
y.d_d.A<- lmer(Assimilation ~ Year*Drought + (Drought|Site.Lat/Family) + (1|Block),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d.A,y.d_d.A) #select model with Drought intercept?

#A 1|
no_X.A.1<- lmer(Assimilation ~ Year + Drought + (1|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.A,no_X.A.1) #No difference, select simpler model
no_year.A.1<- lmer(Assimilation ~ Drought + (1|Site.Lat/Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X.A.1,no_year.A.1) # select drought model
no.D.A.1<- lmer(Assimilation ~ (1|Site.Lat/Family) + (1|Block),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.A.1,no.D.A.1) #No difference, nothing is better

#A Drought|
no_X.A<- lmer(Assimilation ~ Year + Drought + (Drought|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(y.d_d.A,no_X.A) #No interaction model prefered marginally
no_year.A<- lmer(Assimilation ~ Drought + (Drought|Site.Lat/Family) + (1|Block),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_X.A,no_year.A) #Drought only model prefered
no.D.A<- lmer(Assimilation ~ (Drought|Site.Lat/Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(no_year.A,no.D.A) #No difference, nothing is better



