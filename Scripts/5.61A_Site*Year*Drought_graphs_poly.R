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
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
exp_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
exp_site <- visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
exp_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


# Water Content
fullmod.wc <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
#Points
wc_site <- visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
#  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
#  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
wc_site + theme(legend.text = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
wc_site <- visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
wc_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

# Stomatal_Conductance _wet
y3.10site<- y3 %>% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %>% droplevels()

fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site)
#Points
gs_site <- visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
gs_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
gs_site <- visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
gs_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

# Stomatal_Conductance _dry
y3.10site<- y3 %>% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %>% droplevels()

fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site)
#Points
gs_site <- visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
gs_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
gs_site <- visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
gs_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


# Assimilation - Wet
y3.10site<- y3 %>% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %>% droplevels()

fullmod.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site)
#Points
A_site <- visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,
                  jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
A_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
A_site <- visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
A_site + theme(legend.text = element_text(size = 12, face = "bold"),
                axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                axis.text.y = element_text(size=12,face="bold"),
                axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))


# Assimilation - Drought
y3.10site<- y3 %>% filter(Site.Lat!="32.9_S02" & Site.Lat!="36.7_S08") %>% droplevels()

fullmod.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3.10site)
#Points
A_site <- visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,
                 jitter=TRUE,points=list(cex=5))+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  scale_linetype_manual(labels = c("Site 1","Site 2","Site 3","Site 4","Site 5","Site 6",
                                   "Site 7","Site 8","Site 9","Site 10","Site 11","Site 12"))+
  theme_classic()
A_site + theme(legend.text = element_text(size = 12, face = "bold"),
               axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
               axis.text.y = element_text(size=12,face="bold"),
               axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
               axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
#no points
A_site <- visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D"), overlay=TRUE,gg=TRUE,partial=FALSE)+
  scale_y_continuous(name="Water Content")+
  scale_x_continuous(limits=c(2010,2017))+
  #  scale_color_manual(values= c("D"="#FF7700", "W"="#006600")) +
  #  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
A_site + theme(legend.text = element_text(size = 12, face = "bold"),
               axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
               axis.text.y = element_text(size=12,face="bold"),
               axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
               axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))

