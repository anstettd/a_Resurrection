#################
# Slopes by Region 
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


region.vec<-c("North", "Center", "South")
slopes.region<-data.frame(region.vec) #sets up site and site lat for slopes data frame
colnames(slopes.region)[1]<-"Region"

#SLA Vs Year _ South
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame
Ref_SLA_filter<- Res_SLA_all %>% filter(Drought=="W")

Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="North")
lm_SLA_D_S<-lm(visregRes~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[1,2]<-summary_SLA_D$coefficients[2,1]

Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="Center")
lm_SLA_D_S<-lm(visregRes~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[2,2]<-summary_SLA_D$coefficients[2,1]

Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="South")
lm_SLA_D_S<-lm(visregRes~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[3,2]<-summary_SLA_D$coefficients[2,1]

colnames(slopes.region)[2]<-"SLA_Dry"







#SLA Setup
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame

# Slope SLA Vs Year - Wet
Ref_SLA_filter<- Res_SLA_all %>% filter(Drought=="W")

for (i in 1:3){
  Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="region.uni")
  lm_SLA_D<-lm(visregRes~Year, data=Ref_SLA_filter)
  summary_SLA_D<-summary(lm_SLA_D_S)
  slopes.region[i,2]<-summary_SLA_D$coefficients[2,1]  
}
colnames(slopes.region)[2]<-"SLA_Dry"




