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
fullmod.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + CMD.anom.s*Drought + Site*CMD.anom.s + 
                         (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(fullmod.cmd.exp, no3way.cmd.exp) #Select full model
Anova(fullmod.cmd.exp , type=3) # 3-way interaction significant
summary(fullmod.cmd.exp)


visreg_fl.anom.W<-visreg(fullmod.cmd.exp, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Wet Treatment
visreg_fl.anom.W + theme(legend.text = element_text(size = 12, face = "bold"), 
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Date of Flowering Wet")

visreg_fl.anom.D<-visreg(fullmod.cmd.exp, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Drought Treatment
visreg_fl.anom.D+ theme(legend.text = element_text(size = 12, face = "bold"), 
                        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                        axis.text.y = element_text(size=14,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Date of Flowering Dry")

##### % Water Content
fullmod.cmd.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year),
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.wc <- lmer(Water_Content ~ Site.Lat*Drought + CMD.anom.s*Drought + Site.Lat*CMD.anom.s + 
                        (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(fullmod.cmd.wc, no3way.cmd.wc) # drop 3-way interaction, select no3way.cmd.wc
# drop 2ways
noclimXd.wc <- lmer(Water_Content ~ CMD.anom.s*Drought + Site.Lat*CMD.anom.s + 
                      (1|Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(no3way.cmd.wc, noclimXd.wc) # drop clim x drought, select noclimXd.wc
climXanom.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom.s + Drought + 
                       (1|Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noclimXd.wc, climXanom.wc) # drop anom x drought, select climXanom.wc
# main effects models
mains.wc <- lmer(Water_Content ~ Site.Lat + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(climXanom.wc, mains.wc) # Select main effect model, select mains.wc
nosite.wc <- lmer(Water_Content ~ CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.wc, nosite.wc) # Retain site, select: mains.wc
no.anom.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.wc,no.anom.wc) # Remove CMD.anom, select: no.anom.wc 
Site.wc <- lmer(Water_Content ~ Site.Lat + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(no.anom.wc,Site.wc ) #Retain Drought. Select no.anom.wc
site.drought <- lmer(Water_Content ~ Drought*Site.Lat + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(no.anom.wc,site.drought ) #Retain Drought. Retain Site
Anova(no.anom.wc, type=3)

visreg_wc.anom.W<-visreg(fullmod.cmd.wc, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Wet Treatment
visreg_wc.anom.W + theme(legend.text = element_text(size = 12, face = "bold"), 
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Water Content Wet")

visreg_wc.anom.D<-visreg(fullmod.cmd.wc, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Drought Treatment
visreg_wc.anom.D+ theme(legend.text = element_text(size = 12, face = "bold"), 
                        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                        axis.text.y = element_text(size=14,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Water Content Dry")

##### % SLA
fullmod.cmd.SLA <- lmer(SLA ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
# drop 3way
no3way.cmd.SLA <- lmer(SLA ~ Site.Lat*Drought + CMD.anom.s*Drought + Site.Lat*CMD.anom.s + 
                        (1|Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
lrtest(fullmod.cmd.SLA, no3way.cmd.SLA) # Retain 3-way model. Select fullmod.cmd.SLA 
Anova(fullmod.cmd.SLA, type=3) #Site X Drought, marginal Site x CMD.anom. Significant main effects.
#SLA Graphs
visreg_SLA.anom.W<-visreg(fullmod.cmd.SLA, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Wet Treatment
visreg_SLA.anom.W + theme(legend.text = element_text(size = 12, face = "bold"), 
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="SLA Wet")

visreg_SLA.anom.D<-visreg(fullmod.cmd.SLA, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Drought Treatment
visreg_SLA.anom.D+ theme(legend.text = element_text(size = 12, face = "bold"), 
                        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                        axis.text.y = element_text(size=14,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="SLA Dry")




##### Stomatal Conductance 
fullmod.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
# drop 3way
no3way.cmd.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + CMD.anom.s*Drought + Site.Lat*CMD.anom.s + 
                        (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
lrtest(fullmod.cmd.gs, no3way.cmd.gs) # Remove 3-way interaction, select no3way.cmd.gs
# drop 2ways
noclimXd.gs <- lmer(Stomatal_Conductance ~ CMD.anom.s*Drought + Site.Lat*CMD.anom.s + (1|Family) + (1|Block) + (1|Year),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(no3way.cmd.gs, noclimXd.gs) # Drop clim * Drought, select noclimXd.gs
climXanom.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom.s + Drought + 
                       (1|Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(noclimXd.gs, climXanom.gs) # drop anom x drought, select climXanom.gs
# test main effect 
mains.gs <- lmer(Stomatal_Conductance ~ Site.Lat + CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(climXanom.gs, mains.gs) # Select main effect model, select mains.gs
nosite.SLA <- lmer(Stomatal_Conductance ~ CMD.anom.s + Drought + (1|Family) + (1|Block) + (1|Year), data=y3)
lrtest(mains.gs, nosite.SLA) # Remove site effect
D.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block) + (1|Year),data=y3)
lrtest(nosite.SLA,D.gs) # Remove anomaly, select Drought only model: D.gs
nothing.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block) + (1|Year),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
lrtest(D.gs,nothing.gs) # Drought only marginally better than nothing. Select nothing.
#No model to graph.


##### Assimilation
fullmod.cmd.A <- lmer(Assimilation ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.A <- lmer(Assimilation ~ Site.Lat*Drought + CMD.anom.s*Drought + Site.Lat*CMD.anom.s + 
                       (1|Family) + (1|Block) + (1|Year), 
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
lrtest(fullmod.cmd.A, no3way.cmd.A) # Retain 3-way model. Selectfullmod.cmd.A 
Anova(fullmod.cmd.A) #Site and Drought main effect significant

#Assimilation Graphs
visreg_A.anom.W<-visreg(fullmod.cmd.A, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Wet Treatment
visreg_SLA.anom.W + theme(legend.text = element_text(size = 12, face = "bold"), 
                          axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Assimilation Wet")

visreg_A.anom.D<-visreg(fullmod.cmd.A, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Drought Treatment
visreg_SLA.anom.D+ theme(legend.text = element_text(size = 12, face = "bold"), 
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Assimilation Dry")

##### Above Ground Biomass
fullmod.cmd.bio <- lmer(Biomass ~ Site.Lat*CMD.anom.s*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# drop 3way
no3way.cmd.bio <- lmer(Biomass ~ Site.Lat*Drought + CMD.anom.s*Drought + Site.Lat*CMD.anom 
                       + (1|Family) + (1|Block) + (1|Year), 
                       control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
# warning about rank-deficiency. seems not to be a big deal https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
lrtest(fullmod.cmd.bio, no3way.cmd.bio) #retain 3-way
Anova(fullmod.cmd.bio) #Significant SiteXDrought interaction. Other two 2X interactions are marginal. 

#Biomass Graphs
visreg_bio.anom.W<-visreg(fullmod.cmd.bio, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Wet Treatment
visreg_bio.anom.W + theme(legend.text = element_text(size = 12, face = "bold"), 
                          axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Biomass Wet")

visreg_bio.anom.D<-visreg(fullmod.cmd.bio, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
# Show 12 site plot for Drought Treatment
visreg_bio.anom.D+ theme(legend.text = element_text(size = 12, face = "bold"), 
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))+
  scale_x_continuous(name="Climate Moisture Deficit Anomaly")+
  scale_y_continuous(name="Biomass Dry")





