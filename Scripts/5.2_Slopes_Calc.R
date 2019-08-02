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

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
wna <- read.csv("Data/wna.csv", header=T) #Bring in historical climate data, one value per site
wna_all<- read.csv("Data/wna_all.csv", header=T) #Bring in anomaly data, scaled climate information

#################### Slopes ####################
slopes.rapid<-distinct(y3, Site, Site.Lat) #sets up site and site lat for slopes data frame
### Flowering_Dry ###
fullmod.exp <- lmer(Experiment_Date.s ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)# mixed 3-way model
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) # plot only Drought treatment
fit_flower_D<-vis_flower_D$fit #put points representing line of best fit into new variable
flower_dry_pop<-unique(fit_flower_D$Site.Lat) # sets up a vector with each site code a one entry
#For each population run lm on points on line of fit, then extract slope
for (i in 1:12){
  fit_flower_D_tmp<-fit_flower_D %>% filter(Site.Lat==flower_dry_pop[i]) #fliter Drought data set by accending site
  lm_flower_D<-lm(visregFit~Year, data=fit_flower_D_tmp) # take a lm of residuals
  summary_flower_D<-summary(lm_flower_D) #get summary of lm
  slopes.rapid[i,3]<-summary_flower_D$coefficients[2,1] #extract slope
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
fullmod.wc <- lmer(Water_Content.s ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
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
fullmod.SLA <- lmer(SLA.s ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
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

#Stomatal Conductance_Dry
fullmod.gs <- lmer(Stomatal_Conductance.s ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
vis_gs_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_gs_D<-vis_wc_D$fit
for (i in 1:12){
  fit_gs_D_tmp<-fit_gs_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_gs_D<-lm(visregFit~Year, data=fit_gs_D_tmp)
  summary_gs_D<-summary(lm_gs_D)
  slopes.rapid[i,9]<-summary_gs_D$coefficients[2,1]
}
colnames(slopes.rapid)[9]<-"Stomatal_Conductance_Dry"

#Stomatal Conductance_Wet
vis_gs_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_gs_W<-vis_gs_W$fit
for (i in 1:12){
  fit_gs_W_tmp<-fit_gs_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_gs_W<-lm(visregFit~Year, data=fit_gs_W_tmp)
  summary_gs_W<-summary(lm_gs_W)
  slopes.rapid[i,10]<-summary_gs_W$coefficients[2,1]
}
colnames(slopes.rapid)[10]<-"Stomatal_Conductance_Wet"

#Assimilaton_Dry
fullmod.A <- lmer(Assimilation.s ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), 
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_A_D<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_A_D<-vis_wc_D$fit
for (i in 1:12){
  fit_A_D_tmp<-fit_A_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_A_D<-lm(visregFit~Year, data=fit_A_D_tmp)
  summary_A_D<-summary(lm_A_D)
  slopes.rapid[i,11]<-summary_A_D$coefficients[2,1]
}
colnames(slopes.rapid)[11]<-"Assimilation_Dry"

#Assimilation_Wet
vis_A_W<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_A_W<-vis_SLA_W$fit
for (i in 1:12){
  fit_A_W_tmp<-fit_A_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_A_W<-lm(visregFit~Year, data=fit_A_W_tmp)
  summary_A_W<-summary(lm_A_W)
  slopes.rapid[i,12]<-summary_A_W$coefficients[2,1]
}
colnames(slopes.rapid)[12]<-"Assimilation_Wet"


#Get historical CMD, MAT, MAP into slope data frame
slopes.rapid.clim<-left_join(slopes.rapid, wna,by=c("Site"="Site")) #join slope data with per site historical climate
slopes.rapid.clim<-slopes.rapid.clim %>% select(-c(X)) #Remove repetative variable

#Sum anomaly from each year
site_vec<- unique(wna_all$Site) # get the population names
# For every population sum the yearly anomaly
for (i in 1:12){
  wna.temp<-wna_all%>% filter(Site==site_vec[i])
  sum.CMD.temp<-sum(wna.temp$CMD.anom)
  sum.MAT.temp<-sum(wna.temp$MAT.anom)
  sum.MAP.temp<-sum(wna.temp$MAP.anom)
  slopes.rapid.clim[i,16]<-sum.CMD.temp
  slopes.rapid.clim[i,17]<-sum.MAT.temp
  slopes.rapid.clim[i,18]<-sum.MAP.temp
}
colnames(slopes.rapid.clim)[16]<-"C_Anomaly.CMD" #Name colum headers
colnames(slopes.rapid.clim)[17]<-"C_Anomaly.MAT"
colnames(slopes.rapid.clim)[18]<-"C_Anomaly.MAP"
#corrections for S2 (2010-2014), S8 (2011-2014), S17 (2011-2016), S36 (2011-2016). Remove extra years from anomaly sum
#set up sort fuction here to sort by Site
wna_all_order<-wna_all[order(wna_all$Site),] # sort by site
slopes.rapid.clim[1,16] <- sum(wna_all_order[1,13],wna_all_order[2,13],wna_all_order[3,13],wna_all_order[4,13],wna_all_order[5,13])
slopes.rapid.clim[1,17] <- sum(wna_all_order[1,14],wna_all_order[2,14],wna_all_order[3,14],wna_all_order[4,14],wna_all_order[5,14])
slopes.rapid.clim[1,18] <- sum(wna_all_order[1,15],wna_all_order[2,15],wna_all_order[3,15],wna_all_order[4,15],wna_all_order[5,15])
slopes.rapid.clim[3,16] <- sum(wna_all_order[16,13],wna_all_order[17,13],wna_all_order[18,13],wna_all_order[19,13])
slopes.rapid.clim[3,17] <- sum(wna_all_order[16,14],wna_all_order[17,14],wna_all_order[18,14],wna_all_order[19,14])
slopes.rapid.clim[3,18] <- sum(wna_all_order[16,15],wna_all_order[17,15],wna_all_order[18,15],wna_all_order[19,15])
slopes.rapid.clim[8,16] <- sum(wna_all_order[51,13],wna_all_order[52,13],wna_all_order[53,13],wna_all_order[54,13],wna_all_order[55,13],wna_all_order[56,13])
slopes.rapid.clim[8,17] <- sum(wna_all_order[51,14],wna_all_order[52,14],wna_all_order[53,14],wna_all_order[54,14],wna_all_order[55,14],wna_all_order[56,14])
slopes.rapid.clim[8,18] <- sum(wna_all_order[51,15],wna_all_order[52,15],wna_all_order[53,15],wna_all_order[54,15],wna_all_order[55,15],wna_all_order[56,15])
slopes.rapid.clim[12,16] <- sum(wna_all_order[79,13],wna_all_order[80,13],wna_all_order[81,13],wna_all_order[82,13],wna_all_order[83,13],wna_all_order[84,13])
slopes.rapid.clim[12,17] <- sum(wna_all_order[79,14],wna_all_order[80,14],wna_all_order[81,14],wna_all_order[82,14],wna_all_order[83,14],wna_all_order[84,14])
slopes.rapid.clim[12,18] <- sum(wna_all_order[79,15],wna_all_order[80,15],wna_all_order[81,15],wna_all_order[82,15],wna_all_order[83,15],wna_all_order[84,15])

# Scale historical climate for slopes.rapid.clim file
slopes.rapid.clim <- slopes.rapid.clim %>% mutate(MAT.clim.s = scale(MAT.clim),
                                                  MAP.clim.s = scale(MAP.clim),
                                                  CMD.clim.s = scale(CMD.clim),
                                                  C_Anomaly.CMD.s = scale(C_Anomaly.CMD),
                                                  C_Anomaly.MAT.s = scale(C_Anomaly.MAT),
                                                  C_Anomaly.MAP.s = scale(C_Anomaly.MAP))

write.csv(slopes.rapid.clim,'Data/slopes.rapid.clim.csv') #Export file