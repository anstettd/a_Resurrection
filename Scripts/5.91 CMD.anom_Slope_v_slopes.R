#################
# Rate of evolutionary change of one trait predicted by rate of change in another
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
library(egg)
slopes.rapid <- read.csv("Data/slopes.CMD.anom.csv", header=T) #Imports main dataset

############# Comparison of rates of change of evolution between different traits
#Flowerting date slope vs. water content slope Wet
lm.slope.wc.fl<-lm(Flowering_Wet~Water_Content_Wet, data = slopes.rapid)
summary(lm.slope.wc.fl)
Anova(lm.slope.wc.fl,type=c(3))
slope.wc.fl<-ggplot(slopes.rapid, aes(Water_Content_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.wc.fl<-slope.wc.fl + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=0, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=10,face="bold"),
                  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=11,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Water Content") +
  scale_y_continuous(name="Slope Date of Flowering")
# There is a marginal trend, but with variation in the centre


#Water Content vs. Assimilation slope Wet
lm.slope.wc.A<-lm(Water_Content_Wet~Assimilation_Wet, data = slopes.rapid)
summary(lm.slope.wc.A)
Anova(lm.slope.wc.A,type=c(3))
slope.wc.A<-ggplot(slopes.rapid, aes(Water_Content_Wet,Assimilation_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.wc.A  <- slope.wc.A + theme(legend.text = element_text(size = 12, face = "bold"),
                   axis.text.x = element_text(size=10, face="bold", angle=45,hjust=1),
                   axis.text.y = element_text(size=10,face="bold"),
                   axis.title.x = element_text(color="black", size=12, vjust = 0.5, face="bold"),
                   axis.title.y = element_text(color="black", size=12,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Water Content") +
  scale_y_continuous(name="Slope of Assimilation") 
# Not significant, trend line has lots of scatter.

ggarrange(slope.wc.fl, slope.wc.A,ncol=1,nrow=2)









#############################################################################################################################

#Water Content vs. SLA slope Wet
lm.slope.wc.SLA<-lm(Water_Content_Wet~SLA_Wet, data = slopes.rapid)
summary(lm.slope.wc.SLA)
Anova(lm.slope.wc.SLA,type=c(3))
slope_wet.wc.SLA<-ggplot(slopes.rapid, aes(Water_Content_Wet,SLA_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet.wc.SLA <-slope_wet.wc.SLA + theme(legend.text = element_text(size = 12, face = "bold"),
                                            axis.text.x = element_text(size=0, face="bold", angle=45,hjust=1),
                                            axis.text.y = element_text(size=10,face="bold"),
                                            axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                            axis.title.y = element_text(color="black", size=12,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Water_Content") +
  scale_y_continuous(name="Slope of SLA") 
#Strong negative association between water content and SLA rate of evolution.

#Flowerting date slope vs. SLA slope Wet
lm.slope.fl.SLA<-lm(Flowering_Wet~SLA_Wet, data = slopes.rapid)
summary(lm.slope.fl.SLA)
Anova(lm.slope.fl.SLA,type=c(3))
slope.fl.SLA<<-ggplot(slopes.rapid, aes(SLA_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.fl.SLA + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope SLA") +
  scale_y_continuous(name="Slope Date of Flowering") 
#outlier site is causing the trend to not be significant

#Flowerting date slope vs. Stomatal Conductance slope Wet
lm.slope.fl.gs<-lm(Flowering_Wet~Stomatal_Conductance_Wet, data = slopes.rapid)
summary(lm.slope.fl.gs)
Anova(lm.slope.fl.gs,type=c(3))
slope.fl.gs<-ggplot(slopes.rapid, aes(Stomatal_Conductance_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.fl.gs + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Stomatal_Conductance") +
  scale_y_continuous(name="Slope Date of Flowering") 
# Weak slope with lots of noise. Relationship did not get to stat significance. 


#Flowerting date slope vs. Assimilation slope Wet
lm.slope.fl.A<-lm(Flowering_Wet~Assimilation_Wet, data = slopes.rapid)
summary(lm.slope.fl.A)
Anova(lm.slope.fl.A,type=c(3))
slope.fl.A<-ggplot(slopes.rapid, aes(Assimilation_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.fl.A + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Assimilation") +
  scale_y_continuous(name="Slope Date of Flowering") 
#Relationship is marginally significant. Lots of variation.

#Water Content vs. Stomatal Conductance slope Wet
lm.slope.wc.gs<-lm(Water_Content_Wet~Stomatal_Conductance_Wet, data = slopes.rapid)
summary(lm.slope.wc.gs)
Anova(lm.slope.wc.gs,type=c(3))
slope.wc.gs<-ggplot(slopes.rapid, aes(Stomatal_Conductance_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.wc.gs + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Stomatal_Conductance") +
  scale_y_continuous(name="Slope Date of Water_Content") 
#Not significant. Lots of variation. Weak slope.



#Stomatal_Conductance vs. Assimilation slope Wet
lm.slope.gs.A<-lm(Stomatal_Conductance_Wet~Assimilation_Wet, data = slopes.rapid)
summary(lm.slope.gs.A)
Anova(lm.slope.gs.A,type=c(3))
slope.gs.A<-ggplot(slopes.rapid, aes(Stomatal_Conductance_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.gs.A + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Assimilation") +
  scale_y_continuous(name="Slope of Stomatal_Conductance") 
# Weak slope, not significant.


#Biomass vs. flowerting date slope Wet
lm.slope.fl.bio<-lm(Biomass_Wet~Flowering_Wet, data = slopes.rapid)
summary(lm.slope.fl.bio)
Anova(lm.slope.fl.bio,type=c(3))
slope.fl.bio<-ggplot(slopes.rapid, aes(Flowering_Wet,Biomass_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.fl.bio + theme(legend.text = element_text(size = 12, face = "bold"),
                    axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                    axis.text.y = element_text(size=14,face="bold"),
                    axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                    axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Date of Flowering") +
  scale_y_continuous(name="Slope of Biomass")
# Weak slope not significant.

#Biomass vs. Water_Content slope Wet
lm.slope.wc.bio<-lm(Biomass_Wet~Water_Content_Wet, data = slopes.rapid)
summary(lm.slope.wc.bio)
Anova(lm.slope.wc.bio,type=c(3))
slope.wc.bio<-ggplot(slopes.rapid, aes(Water_Content_Wet,Biomass_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.wc.bio + theme(legend.text = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Water Content") +
  scale_y_continuous(name="Slope of Biomass")
# Weak slope, not significant

#Biomass vs. Stomatal_Conductance slope Wet
lm.slope.gs.bio<-lm(Biomass_Wet~Stomatal_Conductance_Wet, data = slopes.rapid)
summary(lm.slope.gs.bio)
Anova(lm.slope.gs.bio,type=c(3))
slope.gs.bio<-ggplot(slopes.rapid, aes(Stomatal_Conductance_Wet,Biomass_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.gs.bio + theme(legend.text = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Stomatal_Conductance") +
  scale_y_continuous(name="Slope of Biomass")
# Flat line not significant

#Assimilation vs. Stomatal_Conductance slope Wet
lm.slope.A.bio<-lm(Biomass_Wet~Assimilation_Wet, data = slopes.rapid)
summary(lm.slope.A.bio)
Anova(lm.slope.A.bio,type=c(3))
slope.A.bio<-ggplot(slopes.rapid, aes(Assimilation_Wet,Biomass_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope.A.bio + theme(legend.text = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Assimilation") +
  scale_y_continuous(name="Slope of Biomass")
# Flat line not significant




