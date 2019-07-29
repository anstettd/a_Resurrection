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


############## CMD & Anomaly ######################

#####Experiment Date
fullmod.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
summary(fullmod.cmd.exp)

# drop 3way
no3way.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.exp, no3way.cmd.exp) #no3way slightly lower likelihood but not by much. could select 3way b/c highest likelihood, or use parsimony to simplify whenever there's not support FOR retaining higher-order terms.
Anova(no3way.cmd.exp) #interactions involving clim not significant. Reducing complexity will likely lead to the best model.

# drop 2ways singly
noclimXd.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXd.exp) #same likelihood. can drop clim x drought.

noanomXD.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.exp, noanomXD.exp) #significant support for keeping anom x drought

noclimXanom.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXanom.exp) #same likelihood. can drop clim x anom.

# test main effect of climate with background of anom x drought
anomxDclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
anomxDnoclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(anomxDclim.exp, anomxDnoclim.exp) # drop main effect of climate
Anova(anomxDnoclim.exp)

# best model: anomaly x drought (anomxDnoclim.exp)
visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)
anom.fl.graph<-visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T, gg=TRUE)+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
anom.fl.graph + 
  scale_x_continuous(name="CMD Anomaly") +
  scale_y_continuous(name="Date of Flowering") +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# sites with the greatest CMD anomalies have less plasticity in response to drought and delay flowering less under wet conditions


##### % Water Content
fullmod.cmd.wc <- lmer(Water_Content ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
#summary(fullmod.exp)

# drop 3way
no3way.cmd.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.wc, no3way.cmd.wc) # drop 3-way interaction (simpler model has significantly higher likelihood)

# drop 2ways singly
noclimXd.wc <- lmer(Water_Content ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.wc, noclimXd.wc) # drop clim x drought

noanomXD.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.wc, noanomXD.wc) # drop anom x drought

noclimXanom.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.wc, noclimXanom.wc) # drop clim x anom

# main effects models
mains.wc <- lmer(Water_Content ~ CMD.clim.scaled + CMD.anom.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)

noD.wc <- lmer(Water_Content ~ CMD.clim.scaled + CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(mains.wc, noD.wc) # drop drought

noclim.wc <- lmer(Water_Content ~ CMD.anom.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(mains.wc, noclim.wc) # drop climate 

noanom.wc <- lmer(Water_Content ~ CMD.clim.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(mains.wc, noanom.wc) # drop anomaly

# test main effects alone
intercept.wc <- lmer(Water_Content ~ (1|Site/Family) + (1|Block) + (1|Year), data=y4)

drought.wc <- lmer(Water_Content ~ Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, drought.wc) #drought not supported in background of other main effects, but it is favored to add drought by itself

clim.wc <- lmer(Water_Content ~ CMD.clim.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, clim.wc) #climate is worse than nothing

anom.wc <- lmer(Water_Content ~ CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, anom.wc) #anomaly is worse than nothing

# best model: main effect of drought (drought.wc)
anom.wc.graph<-visreg(drought.wc, xvar="Drought",gg=TRUE)+  
  theme_classic()
anom.wc.graph +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# higher water content in wet treatment

##### % Above Ground Biomass
fullmod.cmd.bio <- lmer(Biomass ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
#summary(fullmod.exp)

# drop 3way
no3way.cmd.bio <- lmer(Biomass ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
# warning about rank-deficiency. seems not to be a big deal https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
lrtest(fullmod.cmd.bio, no3way.cmd.bio) #retain 3-way

visreg(fullmod.cmd.bio, xvar="CMD.anom.scaled", by="CMD.clim.scaled", cond=list(Drought="D"))
visreg(fullmod.cmd.bio, xvar="CMD.anom.scaled", by="CMD.clim.scaled", cond=list(Drought="W"))
# in wet treatment, historically dry sites are more responsive to cmd anomalies than historically wet sites (greater biomass with wet anomaly, lower biomass with dry anomaly)
# in dry treatment, all sites insensitive to cmd anomalies 


##### Stomatal Conductance
fullmod.cmd.gs <- lmer(Stomatal_Conductance~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)

# drop 3way
no3way.cmd.gs <- lmer(Stomatal_Conductance ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.gs, no3way.cmd.gs) #no3way slightly lower likelihood but not by much. could select 3way b/c highest likelihood, or use parsimony to simplify whenever there's not support FOR retaining higher-order terms.
Anova(no3way.cmd.exp) #interactions involving clim not significant. Reducing complexity will likely lead to the best model.

# drop 2ways singly
noclimXd.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXd.exp) #same likelihood. can drop clim x drought.

noanomXD.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.exp, noanomXD.exp) #significant support for keeping anom x drought

noclimXanom.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXanom.exp) #same likelihood. can drop clim x anom.

# test main effect of climate with background of anom x drought
anomxDclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
anomxDnoclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(anomxDclim.exp, anomxDnoclim.exp) # drop main effect of climate
Anova(anomxDnoclim.exp)

# best model: anomaly x drought (anomxDnoclim.exp)
visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)
anom.fl.graph<-visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T, gg=TRUE)+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
anom.fl.graph + 
  scale_x_continuous(name="CMD Anomaly") +
  scale_y_continuous(name="Date of Flowering") +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# sites with the greatest CMD anomalies have less plasticity in response to drought and delay flowering less under wet conditions

##### Stomatal Conductance
fullmod.cmd.gs <- lmer(Experiment_Date ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
summary(fullmod.cmd.exp)

# drop 3way
no3way.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.exp, no3way.cmd.exp) #no3way slightly lower likelihood but not by much. could select 3way b/c highest likelihood, or use parsimony to simplify whenever there's not support FOR retaining higher-order terms.
Anova(no3way.cmd.exp) #interactions involving clim not significant. Reducing complexity will likely lead to the best model.

# drop 2ways singly
noclimXd.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXd.exp) #same likelihood. can drop clim x drought.

noanomXD.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.exp, noanomXD.exp) #significant support for keeping anom x drought

noclimXanom.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXanom.exp) #same likelihood. can drop clim x anom.

# test main effect of climate with background of anom x drought
anomxDclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
anomxDnoclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(anomxDclim.exp, anomxDnoclim.exp) # drop main effect of climate
Anova(anomxDnoclim.exp)

# best model: anomaly x drought (anomxDnoclim.exp)
visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)
anom.fl.graph<-visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T, gg=TRUE)+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
anom.fl.graph + 
  scale_x_continuous(name="CMD Anomaly") +
  scale_y_continuous(name="Date of Flowering") +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
        axis.text.y = element_text(color="black", size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16,vjust = -15, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 15, face="bold"))
# sites with the greatest CMD anomalies have less plasticity in response to drought and delay flowering less under wet conditions


