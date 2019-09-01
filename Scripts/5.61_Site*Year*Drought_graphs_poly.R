#################
# Graphs for Site x Year X Drought
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
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_flower_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
# Water Content
fullmod.wc <- lmer(Water_Content ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_wc_D<-vis_wc_D$res ; Res_wc_W<-vis_wc_W$res # Extract residuals
Res_wc_all<-rbind(Res_wc_D, Res_wc_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_wc_all_plot<-ggplot(Res_wc_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
  scale_y_continuous(name="Water Content", limits=c(0.1,0.4))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_wc_all_plot <-Res_wc_all_plot + theme(legend.position = "none",
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_wc_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
# SLA
fullmod.SLA <- lmer(SLA ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_SLA_all_plot<-ggplot(Res_SLA_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_SLA_all_plot <- Res_SLA_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

# Stomatal Conductence
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_gs_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_gs_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_gs_D<-vis_gs_D$res ; Res_gs_W<-vis_gs_W$res # Extract residuals
Res_gs_all<-rbind(Res_gs_D, Res_gs_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_gs_all_plot<-ggplot(Res_gs_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,1))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_gs_all_plot <- Res_gs_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_gs_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

# Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_A_D<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_A_W<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_A_D<-vis_A_D$res ; Res_A_W<-vis_A_W$res # Extract residuals
Res_A_all<-rbind(Res_A_D, Res_A_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
             "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
Res_A_all_plot<-ggplot(Res_A_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
  scale_y_continuous(name="Assimilation")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_A_all_plot <- Res_A_all_plot + theme(legend.position = "none",
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_A_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

###############################
# Biomass
fullmod.bio <- lmer(Biomass ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_bio_D<-visreg(fullmod.bio, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_bio_W<-visreg(fullmod.bio, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_bio_D<-vis_bio_D$res ; Res_bio_W<-vis_bio_W$res # Extract residuals
Res_bio_all<-rbind(Res_bio_D, Res_bio_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
Res_bio_all_plot<-ggplot(Res_bio_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_bio_all$Year) +
  scale_y_continuous(name="Biomass")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_bio_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                        axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                        axis.text.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
# Flower Number
fullmod.num <- lmer(Flower_num ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_num_D<-visreg(fullmod.num, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_num_W<-visreg(fullmod.num, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_num_D<-vis_num_D$res ; Res_num_W<-vis_num_W$res # Extract residuals
Res_num_all<-rbind(Res_num_D, Res_num_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
Res_num_all_plot<-ggplot(Res_num_all, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_num_all$Year) +
  scale_y_continuous(name="Flower_num")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_num_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                         axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                         axis.text.y = element_text(size=12,face="bold"),
                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
