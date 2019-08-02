#################
# Mixed Models using CMD.clim & Anomaly
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

#####Experiment Date
fullmod.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + 
                         (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(fullmod.cmd.exp, no3way.cmd.exp) #select simpler model: no3way.cmd.exp
# drop 2ways
noclimXd.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s 
                     + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(no3way.cmd.exp, noclimXd.exp) #drop clim x drought. select: noclimXd.exp
anomXD.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + CMD.clim.s + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(noclimXd.exp, anomXD.exp) # keep simpler model. Select anomXD.exp
#main effects
nox.exp <- lmer(Experiment_Date ~ CMD.anom.s + Drought + CMD.clim.s + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(anomXD.exp, nox.exp) # Retain Anomaly * Drought. Select anomXD.exp
# test main effect of climate with background of anom x drought
no.clim.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(anomXD.exp, no.clim.exp) # Select simpler model. Select no.clim.exp
# best model: anomaly x drought (no.clim.exp)
visreg(no.clim.exp, xvar="CMD.anom.s", by="Drought", overlay=T)
anom.fl.graph<-visreg(no.clim.exp, xvar="CMD.anom.s", by="Drought", overlay=T, gg=TRUE)+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
anom.fl.graph + 
  scale_x_continuous(name="CMD Anomaly") +
  scale_y_continuous(name="Date of Flowering") +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# plants from site/year comibnations with the greatest CMD anomalies have
# earlier flowering time 
# less plasticity in response to drought

##### % Water Content
fullmod.cmd.wc <- lmer(Water_Content ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.wc <- lmer(Water_Content ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + 
                        (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(fullmod.cmd.wc, no3way.cmd.wc) # drop 3-way interaction, select no3way.cmd.wc
# drop 2ways
noclimXd.wc <- lmer(Water_Content ~ CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + 
                      (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(no3way.cmd.wc, noclimXd.wc) # drop clim x drought, select noclimXd.wc
climXanom.wc <- lmer(Water_Content ~ CMD.clim.s*CMD.anom.s + Drought + 
                      (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noclimXd.wc, climXanom.wc) # drop anom x drought, select climXanom.wc
# main effects models
mains.wc <- lmer(Water_Content ~ CMD.clim.s + CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(climXanom.wc, mains.wc) # Select main effect model, select mains.wc
noD.clim <- lmer(Water_Content ~ CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.wc, noD.clim) # Remove climate, Select noD.clim
D.wc <- lmer(Water_Content ~ Drought + (1|Site/Family) + (1|Block) + (1|Year),
             control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noD.clim,D.wc) # Remove anomaly, select Drought only model: D.wc
nothing.wc <- lmer(Water_Content ~ (1|Site/Family) + (1|Block) + (1|Year), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(D.wc,nothing.wc) # Drought only model better than no fixed effects. Select D.wc
# Both climate and anomaly are not predictve of water content
# best model: main effect of drought (D.wc)
Anova(D.wc)
anom.wc.graph<-visreg(D.wc, xvar="Drought",gg=TRUE)+  
  theme_classic()
anom.wc.graph +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# higher water content in wet treatment

##### Stomatal Conductance 
fullmod.cmd.gs <- lmer(Stomatal_Conductance ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #warning about singular fit
# drop 3way
no3way.cmd.gs <- lmer(Stomatal_Conductance ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + 
                        (1|Site/Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #warning about singular fit
lrtest(fullmod.cmd.gs, no3way.cmd.gs) # Remove 3-way interaction, select no3way.cmd.gs
# drop 2ways
noclimXd.gs <- lmer(Stomatal_Conductance ~ CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + (1|Site/Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(no3way.cmd.gs, noclimXd.gs) # Drop clim * Drought, select noclimXd.gs
climXanom.gs <- lmer(Stomatal_Conductance ~ CMD.clim.s*CMD.anom.s + Drought + 
                       (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noclimXd.gs, climXanom.gs) # drop anom x drought, select climXanom.gs
# test main effect 
mains.gs <- lmer(Stomatal_Conductance ~ CMD.clim.s + CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(climXanom.gs, mains.gs) # Select main effect model, select mains.gs
noD.clim <- lmer(Stomatal_Conductance ~ CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.gs, noD.clim) # Remove climate, Select noD.clim
D.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Site/Family) + (1|Block) + (1|Year),data=y3)
lrtest(noD.clim,D.gs) # Remove anomaly, select Drought only model: D.gs
nothing.gs <- lmer(Stomatal_Conductance ~ (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(D.gs,nothing.gs) # Drought only model better than no fixed effects. Select D.gs
# Both climate and anomaly are not predictve of Stomatal Conductance
# best model: main effect of drought (D.gs)
Anova(D.gs)
anom.wc.graph<-visreg(D.gs, xvar="Drought",gg=TRUE)+  
  theme_classic()
anom.wc.graph +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# higher stomatal conductance in wet treatment

##### Assimilation
fullmod.cmd.A <- lmer(Assimilation ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #again singular fit
# drop 3way
no3way.cmd.A <- lmer(Assimilation ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + 
                        (1|Site/Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #warning about singular fit
lrtest(fullmod.cmd.A, no3way.cmd.A) # Select simpler model, select no3way.cmd.A
# drop 2ways
noclimXd.A <- lmer(Assimilation ~ CMD.anom.s*Drought + CMD.clim.s*CMD.anom.s + (1|Site/Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(no3way.cmd.A, noclimXd.A) # select simpler model, select noclimXd.A
climXanom.A <- lmer(Assimilation ~ CMD.clim.s*CMD.anom.s + Drought + 
                       (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noclimXd.A, climXanom.A) # select simpler model select climXanom.A
# test main effect 
mains.A <- lmer(Assimilation ~ CMD.clim.s + CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(climXanom.A, mains.A) # Select main effect model, select mains.A
noD.clim <- lmer(Assimilation ~ CMD.anom.s + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.A, noD.clim) # Select simpler model, Select noD.clim
D.A <- lmer(Assimilation ~ Drought + (1|Site/Family) + (1|Block) + (1|Year),data=y3)
lrtest(noD.clim,D.A) # Select simpler model, select Drought only model: D.A
nothing.A <- lmer(Assimilation ~ (1|Site/Family) + (1|Block) + (1|Year), data=y3)
lrtest(D.A,nothing.A) # Select simpler model s. Select nothing.A
# Best model has no fixed effect predictors


#################################################################################################################################
##### Above Ground Biomass
fullmod.cmd.bio <- lmer(Biomass ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.bio <- lmer(Biomass ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom 
                       + (1|Site/Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# warning about rank-deficiency. seems not to be a big deal https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
lrtest(fullmod.cmd.bio, no3way.cmd.bio) #retain 3-way

visreg(fullmod.cmd.bio, xvar="CMD.anom.s", by="CMD.clim.s", cond=list(Drought="D")) # not particuarly good plots
visreg(fullmod.cmd.bio, xvar="CMD.anom.s", by="CMD.clim.s", cond=list(Drought="W"))
# in wet treatment, historically dry sites are more responsive to cmd anomalies than historically wet sites (greater biomass with wet anomaly, lower biomass with dry anomaly)
# in dry treatment, all sites insensitive to cmd anomalies 

##### Flower Number
fullmod.cmd.num <- lmer(Flower_num ~ CMD.clim.s*CMD.anom.s*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y3)
# drop 3way
no3way.cmd.num <- lmer(Flower_num ~ CMD.clim.s*Drought + CMD.anom.s*Drought + CMD.clim.s*CMD.anom 
                       + (1|Site/Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# warning about rank-deficiency. seems not to be a big deal https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
lrtest(fullmod.cmd.num, no3way.cmd.num) #retain 3-way

visreg(fullmod.cmd.num, xvar="CMD.anom.s", by="CMD.clim.s", cond=list(Drought="D")) # not particuarly good plots
visreg(fullmod.cmd.num, xvar="CMD.anom.s", by="CMD.clim.s", cond=list(Drought="W"))



