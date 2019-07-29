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



# prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
#y3$Year <- as.factor(y3$Year)

#####Experiment Date (flowering time since experiment start date)
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
Anova(fullmod.exp, type = 3) # Drought site interaction, Site year interaction 
#plotting floweirng date vs time per Drought

#visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)

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

#For together see end of script
#visreg(fullmod.exp, xvar="Drought", by="Site.Lat") #Some sites have plastic changes, other do not.


##### Flower_num #### 
#glmer did not converge. Try glmmTMB

fullmod.num <- glmmTMB(Flower_num ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
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



#####Above Ground Biomass
fullmod.bio <- lmer(Biomass ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.bio <- lmer(Biomass ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y2)
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


##### log(SLA) ####
fullmod.SLA <- lmer(log(SLA) ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.SLA, no3way.SLA) #model without 3-way intraction substantially better

# drop 2ways
noDxY.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.SLA,noDxY.SLA) #noDxY.bio signifcnatly better, has larger LogLik
SxYD.SLA<- lmer(log(SLA) ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.SLA,SxYD.SLA) #No significant difference, retain simplier model (SxYD.SLA) with greater loglik

#no interactions
nox.SLA <- lmer(log(SLA) ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(SxYD.SLA,nox.SLA) # no interactions model significantly better.
noDrought.SLA <- lmer(log(SLA) ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.SLA, noDrought.SLA) # no interactions model significantly better. Retain this model.
no.year.SLA <- lmer(log(SLA) ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.SLA, no.year.SLA) # Year removed.
no.site.SLA <- lmer(log(SLA) ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(no.year.SLA, no.site.SLA) # Retain just drought

Anova(no.site.SLA, type = 3) # Year is not significant. Site and drought effect.
visreg(no.site.SLA, xvar="Drought") # Unclear why its only showing some years.



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
#visreg(noYear.wc, xvar="Drought")
#visreg(noYear.wc, xvar="Site.Lat")
#visreg(noYear.wc, xvar="Site.Lat", by="Drought")

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



##### Structure #### 
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glm(Structure ~ Site.Lat*Year*Drought, family=binomial, data=y3) #runs

# drop 3way
no3way.str <- glmer(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), family=binomial, data=y3) # error message
no3way.str <- glm(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year, family=binomial, data=y3) # runs
lrtest(fullmod.str, no3way.str) #Models not significantly different, take simpler model??

#Drop 2way
noDxY.str <- glm(Structure ~ Site.Lat*Drought + Site.Lat*Year, family=binomial, data=y3) 
lrtest(no3way.str, noDxY.str) # Models not significnatly different Take simpler model.
SxYD.str<- glm(Structure ~ Site.Lat*Year + Drought, family=binomial, data=y3)
lrtest(noDxY.str,SxYD.str) # noDxY signifcantly better. Retain this model.
Anova(noDxY.str, type = 3) # Site X drought main effect. VisReg not useful with binomial data.

##### Wilted ####
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glm(Wilted ~ Site.Lat*Year, family=binomial, data=y3) # runs

# drop 2way
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glm(Wilted ~ Site.Lat + Year, family=binomial, data=y3) # runs
lrtest(fullmod.wil, no2way.wil) # models not significantly different, take simpler model.

noYear.wil <- glm(Wilted ~ Site.Lat, family=binomial, data=y3)
lrtest(no2way.wil, noYear.wil) #Take simpler model
Anova(noYear.wil , type = 3) # Nothing significant, not suprising considering how few plants were non-wilted during assessment.

######## Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.gs, no3way.gs) # Accept simpler model
# drop 2ways
noSxY.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.gs,noSxY.gs) # Remove Site X Year
DxYS.gs<- lmer(Stomatal_Conductance ~ Drought*Year + Site.Lat +  (1|Family) + (1|Block), data=y3)
lrtest(noSxY.gs,DxYS.gs) # Remove Site X Drought

#no interactions
nox.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(DxYS.gs,nox.gs) # no interactions model significantly better.
noDrought.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.gs, noDrought.gs) # Retain drought in model.
noYear.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.gs, noYear.gs) # no year model significantly supported
Drought.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.gs, Drought.gs) # Remove site
#Keep model with only the effect of drought

visreg(Drought.gs, xvar="Drought")



######## Assimilation
fullmod.a <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
# Not sure what is going on. This is the best model but also not converging.
#Waring: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge with max|grad| = 0.0047887 (tol = 0.002, component 1)

# drop 3way
no3way.a <- lmer(Assimilation ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.a, no3way.a) # Accept 3-way

Anova(fullmod.a, type = 3) # Drought site interaction, Site year interaction 
#plotting floweirng date vs time per Drought

#visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)

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




####### Graphs for Site x Year X Drought

Res_flower_D<-vis_flower_D$res
Res_flower_W<-vis_flower_W$res

Res_Flower_all<-rbind(Res_flower_D, Res_flower_W)

Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
#Date of Flowering
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(x=Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_flower_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

# Water Content
Res_wc_D<-vis_wc_D$res
Res_wc_W<-vis_wc_W$res
Res_wc_all<-rbind(Res_wc_D, Res_wc_W)
Res_wc_all_plot<-ggplot(Res_wc_all, aes(x=Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Year) +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_wc_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))





