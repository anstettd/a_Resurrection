#################
# Site*Year*Drought Mixed Models
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)

y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors

y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))

slopes.region<-distinct(y5, Region) #sets up site and site lat for slopes data frame
region.uni<-unique(y5$Region) # sets up a vector with each site code as one entry

####Calculate Slopes####

#SLA_Wet
final.mod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_W<-visreg(final.mod.SLA, xvar="Year", by="Region", cond=list(Drought="W"))
fit_SLA_W<-vis_SLA_W$fit
for (i in 1:3){
  fit_SLA_W_tmp<-fit_SLA_W %>% filter(Region==region.uni[i])
  lm_SLA_W<-lm(visregFit~Year, data=fit_SLA_W_tmp)
  summary_SLA_W<-summary(lm_SLA_W)
  slopes.region[i,2]<-summary_SLA_W$coefficients[2,1]
}
colnames(slopes.region)[2]<-"SLA_Wet"

#SLA_Dry
final.mod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_D<-visreg(final.mod.SLA, xvar="Year", by="Region", cond=list(Drought="D"))
fit_SLA_D<-vis_SLA_D$fit
for (i in 1:3){
  fit_SLA_D_tmp<-fit_SLA_D %>% filter(Region==region.uni[i])
  lm_SLA_D<-lm(visregFit~Year, data=fit_SLA_D_tmp)
  summary_SLA_D<-summary(lm_SLA_D)
  slopes.region[i,3]<-summary_SLA_D$coefficients[2,1]
}
colnames(slopes.region)[3]<-"SLA_Dry"

#FT_Wet
final.mod.FT <- lmer(Experiment_Date ~ Region*Drought + Region*Year + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_FT_W<-visreg(final.mod.FT, xvar="Year", by="Region", cond=list(Drought="W"))
fit_FT_W<-vis_FT_W$fit
for (i in 1:3){
  fit_FT_W_tmp<-fit_FT_W %>% filter(Region==region.uni[i])
  lm_FT_W<-lm(visregFit~Year, data=fit_FT_W_tmp)
  summary_FT_W<-summary(lm_FT_W)
  slopes.region[i,4]<-summary_FT_W$coefficients[2,1]
}
colnames(slopes.region)[4]<-"FT_Wet"

#FT_Dry
final.mod.FT <- lmer(Experiment_Date ~ Region*Drought + Region*Year + (1|Block) + (1|Year) + (1|Site.Lat),
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_FT_D<-visreg(final.mod.FT, xvar="Year", by="Region", cond=list(Drought="D"))
fit_FT_D<-vis_FT_D$fit
for (i in 1:3){
  fit_FT_D_tmp<-fit_FT_D %>% filter(Region==region.uni[i])
  lm_FT_D<-lm(visregFit~Year, data=fit_FT_D_tmp)
  summary_FT_D<-summary(lm_FT_D)
  slopes.region[i,5]<-summary_FT_D$coefficients[2,1]
}
colnames(slopes.region)[5]<-"FT_Dry"



