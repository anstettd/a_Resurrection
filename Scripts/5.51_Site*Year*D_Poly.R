#################
# Site*Year*Drought 2nd order Mixed Models
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
fullmod.poly.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3) #3way interaction model
#poly drop 3way
no3way.poly.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2)+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.exp,no3way.poly.exp) #3-way poly supported
Anova(fullmod.poly.exp, type = 3)

#Simple graphing 3-way poly
visreg_flower_D<-visreg(fullmod.poly.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Drought flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"), #theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_D # Show 12 site plot for Drought Treatment
visreg_flower_W<-visreg(fullmod.poly.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+ 
  facet_wrap(.~Site.Lat)+ # Make 12 pannel gg plot with visreg for Wet flower time data
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),#theme modifications
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_W # Show 12 site plot for Wet Treatment
#See "5.6_Stie*Year*Drought_graphs.R" for graphs with W and D on same plot


##### Water_Content ####
fullmod.poly.wc <- lmer(Water_Content ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y3)
# drop 3way
no3way.poly.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.wc, no3way.poly.wc) # keep 3-way, keep fullmod.poly.wc 
Anova(fullmod.poly.wc, type = 3)

#Poly plotting water content vs time per Drought
visreg_wc_D<-visreg(fullmod.poly.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_D
visreg_wc_W<-visreg(fullmod.poly.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_W




##### SLA ####
#3-way poly
fullmod.poly.SLA <- lmer(SLA ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.SLA, no3way.poly.SLA) # accept 3-way model
Anova(fullmod.poly.SLA, type = 3)

#Poly plotting water content vs time per Drought
visreg_wc_D<-visreg(fullmod.poly.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_D
visreg_wc_W<-visreg(fullmod.poly.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_W

######## Stomatal Conductance
fullmod.poly.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) 
                       + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.gs, no3way.poly.gs) # keep 3-way
Anova(fullmod.poly.gs, type = 3)
#poly plotting stomtal conductence vs time per Drought
visreg_gs_D<-visreg(fullmod.poly.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_gs_D
visreg_gs_W<-visreg(fullmod.poly.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_gs_W


######## Assimilation
fullmod.poly.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) 
                       + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.A, no3way.poly.A) # keep 3-way
Anova(fullmod.poly.A, type = 3)
#poly plotting stomtal conductence vs time per Drought
visreg_A_D<-visreg(fullmod.poly.A, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_A_D
visreg_A_W<-visreg(fullmod.poly.A, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_A_W










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

#Poly 3-way
fullmod.poly.bio <- lmer(Biomass ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
# drop 3way
no3way.poly.bio <- lmer(Biomass ~ Site.Lat*Drought + Drought*poly(Year,2) + Site.Lat*poly(Year,2) 
                       + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.poly.bio, no3way.poly.bio) # keep 3-way
#compare poly to non-poly-3way
lrtest(fullmod.poly.bio, fullmod.bio) #poly 3-way is better
Anova(fullmod.poly.bio, type = 3)
#poly plotting stomtal conductence vs time per Drought
visreg_bio_D<-visreg(fullmod.poly.bio, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_bio_D
visreg_bio_W<-visreg(fullmod.poly.bio, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_bio_W






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
