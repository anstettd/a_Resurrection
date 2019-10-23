#################
# Graphs for Site x CMD.anom X Drought
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
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block) + (1|Year), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_flower_D<-visreg(fullmod.exp, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(CMD.anom, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
#  geom_text()+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_flower_all_plot <- Res_flower_all_plot + theme(legend.position = "none",
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))





# Water Content
fullmod.wc <- lmer(Water_Content ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block) + (1|Year),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_wc_D<-visreg(fullmod.wc, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_wc_W<-visreg(fullmod.wc, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_wc_D<-vis_wc_D$res ; Res_wc_W<-vis_wc_W$res # Extract residuals
Res_wc_all<-rbind(Res_wc_D, Res_wc_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_wc_all_plot<-ggplot(Res_wc_all, aes(CMD.anom, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_wc_all_plot <- Res_wc_all_plot + theme(legend.position = "none",
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_wc_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
# SLA
fullmod.SLA <- lmer(SLA ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block), data=y3)
vis_SLA_D<-visreg(fullmod.SLA, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_SLA_all_plot<-ggplot(Res_SLA_all, aes(CMD.anom, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="SLA")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_SLA_all_plot <- Res_SLA_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

# Stomatal Conductence
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block) + (1|Year), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) 
vis_gs_D<-visreg(fullmod.gs, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_gs_W<-visreg(fullmod.gs, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_gs_D<-vis_gs_D$res ; Res_gs_W<-vis_gs_W$res # Extract residuals
Res_gs_all<-rbind(Res_gs_D, Res_gs_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_gs_all_plot<-ggplot(Res_gs_all, aes(CMD.anom, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="Stomatal Conductance")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_gs_all_plot <- Res_gs_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_gs_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

# Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block) + (1|Year), 
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_A_D<-visreg(fullmod.A, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_A_W<-visreg(fullmod.A, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_A_D<-vis_A_D$res ; Res_A_W<-vis_A_W$res # Extract residuals
Res_A_all<-rbind(Res_A_D, Res_A_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_A_all_plot<-ggplot(Res_A_all, aes(CMD.anom, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="Assimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_A_all_plot <- Res_A_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_A_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#########################################################################################################################
# Biomass
fullmod.bio <- lmer(Biomass ~ Site.Lat*CMD.anom*Drought + (1|Family) + (1|Block) + (1|Year), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_bio_D<-visreg(fullmod.bio, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_bio_W<-visreg(fullmod.bio, xvar="CMD.anom.s", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_bio_D<-vis_bio_D$res ; Res_bio_W<-vis_bio_W$res # Extract residuals
Res_bio_all<-rbind(Res_bio_D, Res_bio_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
Res_bio_all_plot<-ggplot(Res_bio_all, aes(CMD.anom.s, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_continuous(name= "Climate Moisture Deficit Anomaly") +
  scale_y_continuous(name="Biomass")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_bio_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

