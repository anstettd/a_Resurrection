#################
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
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="", "34.3_S07"="", "36.2_S10"="","36.7_S08"="", "37.5_S32" = "", 
             "39.4_S29"="", "39.7_S18"="", "41.7_S17"="", "41.8_S16"="", "42.3_S36"="", "43.4_S15"="")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
                                          axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                          axis.text.y = element_text(size=12,face="bold"),
                                          axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

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
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="", "34.3_S07"="", "36.2_S10"="","36.7_S08"="", "37.5_S32" = "", 
             "39.4_S29"="", "39.7_S18"="", "41.7_S17"="", "41.8_S16"="", "42.3_S36"="", "43.4_S15"="")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

######################################################################################################################
##### SLA ####
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
# drop 3way
no3way.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model

#SLA Graphs
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="", "34.3_S07"="", "36.2_S10"="","36.7_S08"="", "37.5_S32" = "", 
             "39.4_S29"="", "39.7_S18"="", "41.7_S17"="", "41.8_S16"="", "42.3_S36"="", "43.4_S15"="")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

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
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="", "34.3_S07"="", "36.2_S10"="","36.7_S08"="", "37.5_S32" = "", 
             "39.4_S29"="", "39.7_S18"="", "41.7_S17"="", "41.8_S16"="", "42.3_S36"="", "43.4_S15"="")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Stomatal Conductance")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))



######################################################################################################################
######## Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5) 
# drop 3way
no3way.A <- lmer(Assimilation ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.A, no3way.A) # Significant 3-way interaction

#Date of Flowering Graphs
fullmod.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="", "34.3_S07"="", "36.2_S10"="","36.7_S08"="", "37.5_S32" = "", 
             "39.4_S29"="", "39.7_S18"="", "41.7_S17"="", "41.8_S16"="", "42.3_S36"="", "43.4_S15"="")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Assimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
