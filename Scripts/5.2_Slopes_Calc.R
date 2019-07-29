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


#################### Slopes ####################
#Reponse variables scaled to allow for easier comparisons across variables.

slopes.rapid<-distinct(y3, Site, Site.Lat) #sets up site and site lat for slopes data frame
#Flowering_Dry
#re-run mixed 3-way model
fullmod.exp <- lmer(Experiment_Date.scaled ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y4)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) # plot only Drought treatment
fit_flower_D<-vis_flower_D$fit #put points representing line of best fit into new variable
flower_dry_pop<-unique(fit_flower_D$Site.Lat) # sets up a vector with each site code a one entry
#For each population run lm on points on line of fit, then graph the slope and place into approriate row
for (i in 1:12){
  fit_flower_D_tmp<-fit_flower_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_flower_D<-lm(visregFit~Year, data=fit_flower_D_tmp)
  summary_flower_D<-summary(lm_flower_D)
  slopes.rapid[i,3]<-summary_flower_D$coefficients[2,1]
}
colnames(slopes.rapid)[3]<-"Flowering_Dry" # lable new variable

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

#WaterContent_Dry
fullmod.wc <- lmer(Water_Content.scaled ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y4)
vis_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_wc_D<-vis_wc_D$fit
for (i in 1:12){
  fit_wc_D_tmp<-fit_wc_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_D<-lm(visregFit~Year, data=fit_wc_D_tmp)
  summary_wc_D<-summary(lm_wc_D)
  slopes.rapid[i,5]<-summary_wc_D$coefficients[2,1]
}
colnames(slopes.rapid)[5]<-"Water_Content_Dry"

#WaterContent_Wet
vis_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_wc_W<-vis_wc_W$fit
for (i in 1:12){
  fit_wc_W_tmp<-fit_wc_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_W<-lm(visregFit~Year, data=fit_wc_W_tmp)
  summary_wc_W<-summary(lm_wc_W)
  slopes.rapid[i,6]<-summary_wc_W$coefficients[2,1]
}
colnames(slopes.rapid)[6]<-"Water_Content_Wet"

#SLA_Dry
fullmod.SLA <- lmer(SLA.scaled ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y4)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_SLA_D<-vis_wc_D$fit
for (i in 1:12){
  fit_SLA_D_tmp<-fit_SLA_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_SLA_D<-lm(visregFit~Year, data=fit_SLA_D_tmp)
  summary_SLA_D<-summary(lm_SLA_D)
  slopes.rapid[i,7]<-summary_SLA_D$coefficients[2,1]
}
colnames(slopes.rapid)[7]<-"SLA_Dry"

#SLA_Wet
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_SLA_W<-vis_SLA_W$fit
for (i in 1:12){
  fit_SLA_W_tmp<-fit_SLA_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_SLA_W<-lm(visregFit~Year, data=fit_SLA_W_tmp)
  summary_SLA_W<-summary(lm_SLA_W)
  slopes.rapid[i,8]<-summary_SLA_W$coefficients[2,1]
}
colnames(slopes.rapid)[8]<-"SLA_Wet"

##Put stomatal conductance & Max_Photosythesis here


#Get historical CMD, MAT, MAP into slope data frame
slopes.rapid.clim<-left_join(slopes.rapid, wna,by=c("Site"="Site"))

#Sum anomaly from each year
site_vec<- unique(wna_all$Site) # get the population names
# For every population sum the yearly anomaly
for (i in 1:12){
  wna.temp<-wna_all%>% filter(Site==site_vec[i])
  sum.CMD.temp<-sum(wna.temp$CMD.anom.scaled)
  sum.MAT.temp<-sum(wna.temp$MAT.anom.scaled)
  sum.MAP.temp<-sum(wna.temp$MAP.anom.scaled)
  slopes.rapid.clim[i,12]<-sum.CMD.temp
  slopes.rapid.clim[i,13]<-sum.MAT.temp
  slopes.rapid.clim[i,14]<-sum.MAP.temp
}
colnames(slopes.rapid.clim)[12]<-"C_Anomaly.CMD" #Name colum headers
colnames(slopes.rapid.clim)[13]<-"C_Anomaly.MAT"
colnames(slopes.rapid.clim)[14]<-"C_Anomaly.MAP"

#corrections for S2 (2010-2014), S8 (2011-2014), S17 (2011-2016), S36 (2011-2016). Remove extra years
#set up sort fuction here to sort by Site
wna_all_order<-wna_all[order(wna_all$Site),] # sort by site

slopes.rapid.clim[1,12] <- sum(wna_all_order[1,21],wna_all_order[2,21],wna_all_order[3,21],wna_all_order[4,21],wna_all_order[5,21])
slopes.rapid.clim[1,13] <- sum(wna_all_order[1,22],wna_all_order[2,22],wna_all_order[3,22],wna_all_order[4,22],wna_all_order[5,22])
slopes.rapid.clim[1,14] <- sum(wna_all_order[1,23],wna_all_order[2,23],wna_all_order[3,23],wna_all_order[4,23],wna_all_order[5,23])
slopes.rapid.clim[3,12] <- sum(wna_all_order[16,21],wna_all_order[17,21],wna_all_order[18,21],wna_all_order[19,21])
slopes.rapid.clim[3,13] <- sum(wna_all_order[16,22],wna_all_order[17,22],wna_all_order[18,22],wna_all_order[19,22])
slopes.rapid.clim[3,14] <- sum(wna_all_order[16,23],wna_all_order[17,23],wna_all_order[18,23],wna_all_order[19,23])
slopes.rapid.clim[8,12] <- sum(wna_all_order[51,21],wna_all_order[52,21],wna_all_order[53,21],wna_all_order[54,21],wna_all_order[55,21],wna_all_order[56,21])
slopes.rapid.clim[8,13] <- sum(wna_all_order[51,22],wna_all_order[52,22],wna_all_order[53,22],wna_all_order[54,22],wna_all_order[55,22],wna_all_order[56,22])
slopes.rapid.clim[8,14] <- sum(wna_all_order[51,23],wna_all_order[52,23],wna_all_order[53,23],wna_all_order[54,23],wna_all_order[55,23],wna_all_order[56,23])
slopes.rapid.clim[12,12] <- sum(wna_all_order[79,21],wna_all_order[80,21],wna_all_order[81,21],wna_all_order[82,21],wna_all_order[83,21],wna_all_order[84,21])
slopes.rapid.clim[12,13] <- sum(wna_all_order[79,22],wna_all_order[80,22],wna_all_order[81,22],wna_all_order[82,22],wna_all_order[83,22],wna_all_order[84,22])
slopes.rapid.clim[12,14] <- sum(wna_all_order[79,23],wna_all_order[80,23],wna_all_order[81,23],wna_all_order[82,23],wna_all_order[83,23],wna_all_order[84,23])



# Scale historical climate for slopes.rapid.clim file
slopes.rapid.clim <- slopes.rapid.clim %>% mutate(MAT.clim.s = scale(MAT.clim),
                                                  MAP.clim.s = scale(MAP.clim),
                                                  CMD.clim.s = scale(CMD.clim))

#slope calc for one variable.
#fitS02_flower_D<-fit_flower_D %>% filter(Site.Lat=="32.9_S02") 
#lmS02_flower_D<-lm(visregFit~Year, data=fitS02_flower_D)
#summaryS02_flower_D<-summary(lmS02_flower_D)
#summaryS02_flower_D$coefficients[2,1]
#slopes.rapid[1,3]<-summaryS02_flower_D$coefficients[2,1]








