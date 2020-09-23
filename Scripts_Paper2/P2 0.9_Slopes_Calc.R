#################
# Slopes calculation
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

y7<- read.csv("Data/y7.csv", header=T) #Imports main dataset

#################### Slopes ####################
slopes.rapid<-distinct(y7, Site, Site.Lat) #sets up site and site lat for slopes data frame

### Flowering_Dry ###
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y7)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) # plot only Drought treatment
fit_flower_D<-vis_flower_D$fit #put points representing line of best fit into new variable
flower_dry_pop<-unique(fit_flower_D$Site.Lat) # sets up a vector with each site code as one entry

#For each population run lm on points on line of fit, then extract slope
for (i in 1:12){
  fit_flower_D_tmp<-fit_flower_D %>% filter(Site.Lat==flower_dry_pop[i]) #filter Drought data set by accending site
  lm_flower_D<-lm(visregFit~Year, data=fit_flower_D_tmp) # take a lm of residuals
  summary_flower_D<-summary(lm_flower_D) #get summary of lm
  slopes.rapid[i,3]<-summary_flower_D$coefficients[2,1] #extract slope
}
colnames(slopes.rapid)[3]<-"Flowering_Dry" # label new variable

#The same process get repeated for each variable of interest under both wet and dry treatment.
#Flowering_Wet
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_flower_W<-vis_flower_W$fit
for (i in 1:12){
  fit_flower_W_tmp<-fit_flower_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_flower_W<-lm(visregFit~Year, data=fit_flower_W_tmp)
  summary_flower_W<-summary(lm_flower_W)
  slopes.rapid[i,4]<-summary_flower_W$coefficients[2,1]
}
colnames(slopes.rapid)[4]<-"Flowering_Wet"


#SLA_Wet
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y7)
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_SLA_W<-vis_SLA_W$fit
for (i in 1:12){
  fit_SLA_W_tmp<-fit_SLA_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_SLA_W<-lm(visregFit~Year, data=fit_SLA_W_tmp)
  summary_SLA_W<-summary(lm_SLA_W)
  slopes.rapid[i,5]<-summary_SLA_W$coefficients[2,1]
}
colnames(slopes.rapid)[5]<-"SLA_Wet"

#SLA_Dry
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_SLA_D<-vis_SLA_D$fit
for (i in 1:12){
  fit_SLA_D_tmp<-fit_SLA_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_SLA_D<-lm(visregFit~Year, data=fit_SLA_D_tmp)
  summary_SLA_D<-summary(lm_SLA_D)
  slopes.rapid[i,6]<-summary_SLA_D$coefficients[2,1]
}
colnames(slopes.rapid)[6]<-"SLA_Dry"

#Stomatal Conductance_Dry
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y7)
vis_gs_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_gs_D<-vis_gs_D$fit
for (i in 1:12){
  fit_gs_D_tmp<-fit_gs_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_gs_D<-lm(visregFit~Year, data=fit_gs_D_tmp)
  summary_gs_D<-summary(lm_gs_D)
  slopes.rapid[i,7]<-summary_gs_D$coefficients[2,1]
}
colnames(slopes.rapid)[7]<-"Stomatal_Conductance_Dry"

#Gs Wet
vis_gs_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_gs_W<-vis_gs_W$fit
for (i in 1:12){
  fit_gs_W_tmp<-fit_gs_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_gs_W<-lm(visregFit~Year, data=fit_gs_W_tmp)
  summary_gs_W<-summary(lm_gs_W)
  slopes.rapid[i,8]<-summary_gs_W$coefficients[2,1]
}
colnames(slopes.rapid)[8]<-"Stomatal_Conductance_Wet"


#WaterContent_Dry
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_wc_D<-vis_gs_D$fit
for (i in 1:12){
  fit_wc_D_tmp<-fit_gs_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_D<-lm(visregFit~Year, data=fit_wc_D_tmp)
  summary_wc_D<-summary(lm_wc_D)
  slopes.rapid[i,9]<-summary_wc_D$coefficients[2,1]
}
colnames(slopes.rapid)[9]<-"Water_Content_Dry"

#WaterContent_Wet
#fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_wc_W<-vis_wc_W$fit
for (i in 1:12){
  fit_wc_W_tmp<-fit_gs_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_W<-lm(visregFit~Year, data=fit_wc_W_tmp)
  summary_wc_D<-summary(lm_wc_W)
  slopes.rapid[i,10]<-summary_wc_D$coefficients[2,1]
}
colnames(slopes.rapid)[10]<-"Water_Content_Wet"

#Assimilaton_Dry
fullmod.cmd.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block) + (1|Year), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y7)
vis_A_D<-visreg(fullmod.cmd.A, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_A_D<-vis_A_D$fit
for (i in 1:12){
  fit_A_D_tmp<-fit_A_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_A_D<-lm(visregFit~Year, data=fit_A_D_tmp)
  summary_A_D<-summary(lm_A_D)
  slopes.rapid[i,11]<-summary_A_D$coefficients[2,1]
}
colnames(slopes.rapid)[11]<-"Assimilation_Dry"

#Assimilation_Wet
vis_A_W<-visreg(fullmod.cmd.A, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_A_W<-vis_A_W$fit
for (i in 1:12){
  fit_A_W_tmp<-fit_A_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_A_W<-lm(visregFit~Year, data=fit_A_W_tmp)
  summary_A_W<-summary(lm_A_W)
  slopes.rapid[i,12]<-summary_A_W$coefficients[2,1]
}
colnames(slopes.rapid)[12]<-"Assimilation_Wet"

#Biomass_Dry
fullmod.cmd.bio <- lmer(Biomass ~ Site.Lat*Year*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y7)
vis_bio_D<-visreg(fullmod.cmd.bio, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_bio_D<-vis_bio_D$fit
for (i in 1:12){
  fit_bio_D_tmp<-fit_bio_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_bio_D<-lm(visregFit~Year, data=fit_bio_D_tmp)
  summary_bio_D<-summary(lm_bio_D)
  slopes.rapid[i,13]<-summary_bio_D$coefficients[2,1]
}
colnames(slopes.rapid)[13]<-"Biomass_Dry"

#Biomass_Wet
vis_bio_W<-visreg(fullmod.cmd.bio, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_bio_W<-vis_bio_W$fit
for (i in 1:12){
  fit_bio_W_tmp<-fit_bio_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_bio_W<-lm(visregFit~Year, data=fit_bio_W_tmp)
  summary_bio_W<-summary(lm_bio_W)
  slopes.rapid[i,14]<-summary_bio_W$coefficients[2,1]
}
colnames(slopes.rapid)[14]<-"Biomass_Wet"


write.csv(slopes.rapid,'Data/slopes.year.12.csv') #Export file
