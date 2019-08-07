#################
# Mixed Models using site, year & drought
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

#####Experiment Date (flowering time since experiment start date)
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3) #3way interaction model
# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
Anova(fullmod.exp, type = 3) # Drought * Site interaction, Site * Year interaction 
#Simple graphing    
visreg_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_D # Show 12 site plot for Drought Treatment
visreg_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_W # Show 12 site plot for Wet Treatment
#See "5.6_Stie*Year*Drought_graphs.R" for graphs with W and D on same plot

##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.wc, no3way.wc) # two-way model supported
# drop 2ways
noDxY.wc <- lmer(Water_Content ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.wc,noDxY.wc) #noDxY.wc supported
SxYD.wc<- lmer(Water_Content ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.wc,SxYD.wc) #SxYD.wc supported
#no interactions
nox.wc <- lmer(Water_Content ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(SxYD.wc,nox.wc) # no interactions model significantly better.
noDrought.wc <- lmer(Water_Content ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.wc, noDrought.wc) # no interactions model significantly better. Retain drought in model.
noYear.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.wc, noYear.wc) # no year model significantly supported
Drought.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc, Drought.wc) # no year model significantly supported over Drought.wc, Retain site and drought.
siteXdrought.wc <- lmer(Water_Content ~ Site.Lat*Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc,siteXdrought.wc) # Main effect model with site and drought is best.
year.rad.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Year) + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc,year.rad.wc) # Main effect model with site and drought is best.
Anova(noYear.wc, type = 3) # Site and drought main effect.
#plotting water content vs time per Drought
visreg_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_D
visreg_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_W


##### SLA ####
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model
Anova(fullmod.SLA)
#See "5.6_Stie*Year*Drought_graphs.R" for graphs with W and D on same plot

######## Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.gs, no3way.gs) # drop 3way
# drop 2ways
noSxY.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.gs,noSxY.gs) # Remove Site X Year
DxYS.gs<- lmer(Stomatal_Conductance ~ Drought*Year + Site.Lat +  (1|Family) + (1|Block), data=y3)
lrtest(noSxY.gs,DxYS.gs) # Remove Site X Drought
#no interactions
nox.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(DxYS.gs,nox.gs) # no interactions model significantly better.
noDrought.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.gs, noDrought.gs) # Retain drought in model (retain nox.gs)
no.year.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.gs, no.year.gs ) # Simpler model supported. Remove Year effect.
Drought.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(no.year.gs, Drought.gs) # Retain simpler model
no.main.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block), data=y3)
lrtest(Drought.gs, no.main.gs) # Drought better than nothing
#Keep stomatal conductence model with only the effect of drought
visreg(Drought.gs, xvar="Drought")
#plotting stomtal conductence vs time per Drought
visreg_gs_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_gs_D
visreg_gs_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_gs_W

######## Assimilation
#fullmod.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3) #convergence problem
fullmod.Aba <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), 
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) #converges
# drop 3way
no3way.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.Aba, no3way.A) # Retain simpler model
# drop 2ways
noSxY.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*Year + (1|Family) + (1|Block), data=y3)
lrtest(no3way.A,noSxY.A) # Retain simpler model
SxDY.A <- lmer(Assimilation ~ Site.Lat*Drought + Year + (1|Family) + (1|Block), data=y3)
lrtest(noSxY.A,SxDY.A ) # Retain simpler model
#no interactions
nox.A <- lmer(Assimilation ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(SxDY.A,nox.A) # Retain Site X Drought + Year
Anova(SxDY.A, type = 3) # Site and drought main effect significant
visreg(SxDY.A, xvar="Site.Lat", by="Drought")
#Code for D and W separate
visreg_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_D
visreg_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_W

########################################################################################################################
#Fitness associated traits
#####Above Ground Biomass
fullmod.bio <- lmer(Biomass ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.bio <- lmer(Biomass ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.bio, no3way.bio) #three way marginally significant, no3way.bio has larger LogLik.
# drop 2ways
noDxY.bio <- lmer(Biomass ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.bio,noDxY.bio) #noDxY.bio signifcnatly better, has larger LogLik
SxYD.bio<- lmer(Biomass ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.bio,SxYD.bio) #noDxY.bio significantly better than SxYD.bio, had greater LogLik
#Therefore, retain Biomass ~ Site.Lat*Drought + Site.Lat*Year
Anova(noDxY.bio, type = 3) #Site X Drought, Site X Year
visreg(noDxY.bio, xvar="Year", by="Site.Lat") #some variaiblity in biomass over time. 
#Unclear how biologically meainingful it is.
visreg(noDxY.bio, xvar="Year", by="Drought") #trend to less biomass over time, less bioamss in Drought

##### Flower_num #### #glmer did not converge. glmmTMB also did not converge
#fullmod.num <- glmer(Flower_num ~ Site.Lat*Year*Drought + (1|Family) + (1|Block),
#                       control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), 
#                       data=y3, family=poisson(link = "log"))
#Still getting model convergence problems. We may need to simplify the model. 
#Should I take out (1|Family) or (1|Block)?
# drop 3way
#no3way.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(fullmod.num, no3way.num) 
# drop 2ways
#noDxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noDxY.num) # Drought x Year removal does not lead to a better model
#noSxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noSxY.num) # Site x Year removal does not lead to a better model
#noSxD.num <- glmer(Flower_num ~ Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noSxD.num) # Site x Drought should not be removed
#Anova(no3way.num, type = 3) #all 2-way interactions have some support
#visreg(no3way.num, xvar="Year", by="Site.Lat") 
