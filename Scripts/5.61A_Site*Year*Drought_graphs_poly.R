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
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
#Points
exp_site <- visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                   jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "Populations", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                                       "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()

exp_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Lines
exp_site <- visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,partial=FALSE,
                   jitter=TRUE,points=list(cex=5))+
  ylim(85,110)+
  scale_y_continuous(name="Date of Flowering",limits=c(87,110))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                            "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  
  theme_classic()

exp_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))



#Stomatal Conductance
#remove 2 sites
y3.10site<- y3 %>% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %>% droplevels()
# WET 
fullmod.gsw <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site)
#Points
gsw_site <- visreg(fullmod.gsw, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                   jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,1))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "Populations", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                                       "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()

gsw_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Lines
gsw_site <- visreg(fullmod.gsw, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE,
                   jitter=TRUE,points=list(cex=5))+
  ylim(85,110)+
  scale_y_continuous(name="Stomatal Conductance",limits=c(0.2,0.65))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                            "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  
  theme_classic()

gsw_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

#Stomatal Conductance
# DROUGHT
fullmod.gsd <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
#Points
gsd_site <- visreg(fullmod.gsw, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,
                   jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,1))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "Populations", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                                       "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()

gsd_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Lines
gsd_site <- visreg(fullmod.gsw, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,partial=FALSE,
                   jitter=TRUE,points=list(cex=5))+
  ylim(85,110)+
  scale_y_continuous(name="Stomatal Conductance",limits=c(0.2,0.65))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                            "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  
  theme_classic()

gsd_site + theme(legend.text = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                 axis.text.y = element_text(size=12,face="bold"),
                 axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                 axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Stomatal Conductance
#remove 2 sites
#y3.10site<- y3 %&% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %&% droplevels()
# WET 
fullmod.a <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
#Points
aw_site <- visreg(fullmod.a, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Assimilation")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "Populations", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                                       "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()

aw_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Lines
aw_site <- visreg(fullmod.a, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Assimilation", limits=c(6,17))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                            "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  
  theme_classic()

aw_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

#Assimilation
# DROUGHT

#Points
ad_site <- visreg(fullmod.a, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Assimilation")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "Populations", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                                       "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()

ad_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


#Lines
ad_site <- visreg(fullmod.a, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,partial=FALSE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Assimilaton", limits=c(6,16))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_discrete(name = "", labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                            "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  
  theme_classic()

ad_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
