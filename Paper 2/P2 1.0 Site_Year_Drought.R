#################
# P2
# Site*Year*Drought Mixed Models
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)


y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y55Family <- as.factor(y5$Family) # prep factors


######################################################################################################################
##### Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5) #3way interaction model
# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y5)
####### Report this result #########
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
####################################

#Date of Flowering Graphs
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Replace site names
Res_Flower_all$Site.Lat<- as.character(Res_Flower_all$Site.Lat)
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="32.9_S02"] <- "Site 01"
#Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02" Mill Creek removed
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="34.3_S07"] <- "Site 02"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="36.2_S10"] <- "Site 03"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="36.7_S08"] <- "Site 04"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="37.5_S32"] <- "Site 05"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="39.4_S29"] <- "Site 06"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="39.7_S18"] <- "Site 07"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="41.7_S17"] <- "Site 08"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="41.8_S16"] <- "Site 09"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="42.3_S36"] <- "Site 10"
Res_Flower_all$Site.Lat[Res_Flower_all$Site.Lat=="43.4_S15"] <- "Site 11"


#Set up site lables equating names to codes
Site_Labs<-c("W"="Wet", "D"="Dry")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Site.Lat, colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC"))+
  theme_classic()

Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
       strip.background = element_blank(), strip.text.x=element_text(size=10,face="bold",hjust=0.05,vjust=-1.7))


######################################################################################################################
##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
# drop 3way
no3way.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.wc, no3way.wc) # two-way model supported
# drop 2ways
noDxY.wc <- lmer(Water_Content ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y5)
lrtest(no3way.wc,noDxY.wc) #noDxY.wc supported
SxYD.wc<- lmer(Water_Content ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y5)
lrtest(noDxY.wc,SxYD.wc) #SxYD.wc supported
SxY.wc<- lmer(Water_Content ~ Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(noDxY.wc,SxY.wc) #SxYD.wc supported

#no interactions
nox.wc <- lmer(Water_Content ~ Site.Lat + Year + Drought + (1|Family) + (1|Block),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(SxYD.wc,nox.wc) # no interactions model significantly better.
noDrought.wc <- lmer(Water_Content ~ Site.Lat + Year + (1|Family) + (1|Block), data=y5)
lrtest(nox.wc, noDrought.wc) # no interactions model significantly better. Retain drought in model.
####### Selected Model #########
noYear.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y5)
################################
lrtest(nox.wc, noYear.wc) # no year model significantly supported

#Additional Tests
Drought.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block), data=y5)
lrtest(noYear.wc, Drought.wc) # no year model significantly supported over Drought.wc, Retain site and drought.
siteXdrought.wc <- lmer(Water_Content ~ Site.Lat*Drought + (1|Family) + (1|Block), data=y5)
lrtest(noYear.wc,siteXdrought.wc) # Main effect model with site and drought is best.
year.rad.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Year) + (1|Family) + (1|Block), data=y5)
lrtest(noYear.wc,year.rad.wc) # Main effect model with site and drought is best.

#Water Content Graphs
fullmod.exp <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_wc_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_wc_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_wc_D<-vis_wc_D$res ; Res_wc_W<-vis_wc_W$res # Extract residuals
Res_wc_all<-rbind(Res_wc_D, Res_wc_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_wc_all$Drought <- as.factor(Res_wc_all$Drought)
Res_wc_all$Drought <- factor(Res_wc_all$Drought, levels=c("W", "D"))

#Replace site names
Res_wc_all$Site.Lat<- as.character(Res_wc_all$Site.Lat)
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="32.9_S02"] <- "Site 01"
#Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02" Mill Creek removed
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="34.3_S07"] <- "Site 02"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="36.2_S10"] <- "Site 03"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="36.7_S08"] <- "Site 04"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="37.5_S32"] <- "Site 05"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="39.4_S29"] <- "Site 06"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="39.7_S18"] <- "Site 07"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="41.7_S17"] <- "Site 08"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="41.8_S16"] <- "Site 09"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="42.3_S36"] <- "Site 10"
Res_wc_all$Site.Lat[Res_wc_all$Site.Lat=="43.4_S15"] <- "Site 11"


#Set up site lables equating names to codes
Site_Labs<-c("W"="Wet", "D"="Dry")
Res_wc_all_plot<-ggplot(Res_wc_all, aes(Year, y=visregRes, fill=Site.Lat, colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  scale_x_discrete(limits = Res_wc_all$Year) +
  scale_y_continuous(name="Leaf Water Content", limits=c(0.1,0.4))+
  scale_color_manual(values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC"))+
  theme_classic()

Res_wc_all_plot <-Res_wc_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_wc_all_plot + facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=10,face="bold",hjust=0.05,vjust=-1.7))

######################################################################################################################
##### SLA ####
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
# drop 3way
no3way.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model

#SLA Graphs
fullmod.exp <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_sla_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))

#Replace site names
Res_sla_all$Site.Lat<- as.character(Res_sla_all$Site.Lat)
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="32.9_S02"] <- "Site 01"
#Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02" Mill Creek removed
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="34.3_S07"] <- "Site 02"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="36.2_S10"] <- "Site 03"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="36.7_S08"] <- "Site 04"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="37.5_S32"] <- "Site 05"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="39.4_S29"] <- "Site 06"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="39.7_S18"] <- "Site 07"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="41.7_S17"] <- "Site 08"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="41.8_S16"] <- "Site 09"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="42.3_S36"] <- "Site 10"
Res_sla_all$Site.Lat[Res_sla_all$Site.Lat=="43.4_S15"] <- "Site 11"


#Set up site lables equating names to codes
Site_Labs<-c("W"="Wet", "D"="Dry")
Res_sla_all_plot<-ggplot(Res_sla_all, aes(Year, y=visregRes, fill=Site.Lat, colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  scale_x_discrete(limits = Res_sla_all$Year) +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC"))+
  theme_classic()

Res_sla_all_plot <-Res_sla_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_sla_all_plot + facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=10,face="bold",hjust=0.05,vjust=-1.7))
######################################################################################################################
######## Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.gs, no3way.gs) #keep 3-way
# drop 2ways
noSxY.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Drought + Drought*Year+ (1|Family) + (1|Block), data=y5)
lrtest(no3way.gs,noSxY.gs) # Remove Site X Year
DxYS.gs<- lmer(Stomatal_Conductance ~ Drought*Year + Site.Lat +  (1|Family) + (1|Block), data=y5)
lrtest(noSxY.gs,DxYS.gs) # Remove Site X Drought
#no interactions
nox.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y5)
lrtest(DxYS.gs,nox.gs) # no interactions model significantly better.
noDrought.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Year + (1|Family) + (1|Block), data=y5)
lrtest(nox.gs, noDrought.gs) # Retain drought in model (retain nox.gs)
no.year.gs <- lmer(Stomatal_Conductance ~ Site.Lat + Drought + (1|Family) + (1|Block), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(nox.gs, no.year.gs ) # Simpler model supported. Remove Year effect.
####### Selected Model #########
Drought.gs <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block), data=y5)
lrtest(no.year.gs, Drought.gs) # Retain simpler model
###############################
no.main.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block), data=y5)
lrtest(Drought.gs, no.main.gs) # Drought better than nothing

#Stomatal Conductance Graphs
fullmod.exp <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_gs_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_gs_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_gs_D<-vis_gs_D$res ; Res_gs_W<-vis_gs_W$res # Extract residuals
Res_gs_all<-rbind(Res_gs_D, Res_gs_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_gs_all$Drought <- as.factor(Res_gs_all$Drought)
Res_gs_all$Drought <- factor(Res_gs_all$Drought, levels=c("W", "D"))

#Replace site names
Res_gs_all$Site.Lat<- as.character(Res_gs_all$Site.Lat)
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="32.9_S02"] <- "Site 01"
#Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02" Mill Creek removed
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="34.3_S07"] <- "Site 02"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="36.2_S10"] <- "Site 03"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="36.7_S08"] <- "Site 04"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="37.5_S32"] <- "Site 05"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="39.4_S29"] <- "Site 06"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="39.7_S18"] <- "Site 07"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="41.7_S17"] <- "Site 08"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="41.8_S16"] <- "Site 09"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="42.3_S36"] <- "Site 10"
Res_gs_all$Site.Lat[Res_gs_all$Site.Lat=="43.4_S15"] <- "Site 11"


#Set up site lables equating names to codes
Site_Labs<-c("W"="Wet", "D"="Dry")
Res_gs_all_plot<-ggplot(Res_gs_all, aes(Year, y=visregRes, fill=Site.Lat, colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  scale_x_discrete(limits = Res_gs_all$Year) +
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,0.9))+
  scale_color_manual(values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC"))+
  theme_classic()

Res_gs_all_plot <-Res_gs_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_gs_all_plot + facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=10,face="bold",hjust=0.05,vjust=-1.7))

######################################################################################################################
######## Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5) 
# drop 3way
no3way.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.A, no3way.A) # Significant 3-way interaction

#Date of Assimilation Graphs
fullmod.exp <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_a_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_a_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_a_D<-vis_a_D$res ; Res_a_W<-vis_a_W$res # Extract residuals
Res_a_all<-rbind(Res_a_D, Res_a_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_a_all$Drought <- as.factor(Res_a_all$Drought)
Res_a_all$Drought <- factor(Res_a_all$Drought, levels=c("W", "D"))

#Replace site names
Res_a_all$Site.Lat<- as.character(Res_a_all$Site.Lat)
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="32.9_S02"] <- "Site 01"
#Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02" Mill Creek removed
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="34.3_S07"] <- "Site 02"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="36.2_S10"] <- "Site 03"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="36.7_S08"] <- "Site 04"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="37.5_S32"] <- "Site 05"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="39.4_S29"] <- "Site 06"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="39.7_S18"] <- "Site 07"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="41.7_S17"] <- "Site 08"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="41.8_S16"] <- "Site 09"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="42.3_S36"] <- "Site 10"
Res_a_all$Site.Lat[Res_a_all$Site.Lat=="43.4_S15"] <- "Site 11"


#Set up site lables equating names to codes
Site_Labs<-c("W"="Wet", "D"="Dry")
Res_a_all_plot<-ggplot(Res_a_all, aes(Year, y=visregRes, fill=Site.Lat, colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  scale_x_discrete(limits = Res_a_all$Year) +
  scale_y_continuous(name="Assimilation", limits=c())+
  scale_color_manual(values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000", "Site 02"="#FF0000", "Site 03"="#FF6666",
                               "Site 04"="#FF9999", "Site 05" = "#FFCCCC", "Site 06"="#CCCCCC", "Site 07"="#99CCFF",
                               "Site 08"="#0099FF", "Site 09"="#0066FF", "Site 10"="#0000CC", "Site 11"="#6600CC"))+
  theme_classic()

Res_a_all_plot <-Res_a_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_a_all_plot + facet_wrap(.~Drought, labeller = labeller(Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=10,face="bold",hjust=0.05,vjust=-1.7))

