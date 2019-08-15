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
slopes.rapid.clim <- read.csv("Data/slopes.rapid.clim.csv", header=T) #Imports main dataset
attach(slopes.rapid.clim)
############# Comparison of rates of change of evolution between different traits
#Flowerting date slope vs. water content slope Wet
lm.slope_wet<-lm(Flowering_Wet~Water_Content_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Water_Content_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Water Content") +
  scale_y_continuous(name="Slope Date of Flowering") 

#Flowerting date slope vs. SLA slope Wet
lm.slope_wet<-lm(Flowering_Wet~SLA_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(SLA_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope SLA") +
  scale_y_continuous(name="Slope Date of Flowering") 
#negative slope of SLA (smaller leaf area per weight) associated with earlier flowering time.


#Flowerting date slope vs. Stomatal Conductance slope Wet
lm.slope_wet<-lm(Flowering_Wet~Stomatal_Conductance_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Stomatal_Conductance_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Stomatal_Conductance") +
  scale_y_continuous(name="Slope Date of Flowering") 
#evolution of less stomatal conductance (stomata open less) associated with evolution of earlier flowering time, but with lots of variation.



#Flowerting date slope vs. Assimilation slope Wet
lm.slope_wet<-lm(Flowering_Wet~Assimilation_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Assimilation_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Assimilation") +
  scale_y_continuous(name="Slope Date of Flowering") 
#evolution of smaller Assimilation value associated with evolution of earlier flowering time.

#Water Content vs. Stomatal Conductance slope Wet
lm.slope_wet<-lm(Water_Content_Wet~Stomatal_Conductance_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Stomatal_Conductance_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Stomatal_Conductance") +
  scale_y_continuous(name="Slope Date of Water_Content") 
#Maringally significant relationship with lower stomatla conductance evolution  associated with increased water content evolution

#Water Content vs. Assimilation slope Wet
lm.slope_wet<-lm(Water_Content_Wet~Assimilation_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Assimilation_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Assimilation") +
  scale_y_continuous(name="Slope Date of Water_Content") 
#Strong association between evolution of decreased assimilation  and increased water content evolution.

#Water Content vs. SLA slope Wet
lm.slope_wet<-lm(Water_Content_Wet~SLA_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(SLA_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of SLA") +
  scale_y_continuous(name="Slope Date of Water_Content") 
#Strong association between evolution of smaller SLA and evolution of increased water content.

#Stomatal_Conductance vs. Assimilation slope Wet
lm.slope_wet<-lm(Stomatal_Conductance_Wet~Assimilation_Wet)
summary(lm.slope_wet)
Anova(lm.slope_wet,type=c(3))
slope_wet<-ggplot(slopes.rapid.clim, aes(Stomatal_Conductance_Wet,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope of Assimilation") +
  scale_y_continuous(name="Slope of Stomatal_Conductance") 
#Weak association between evolution of decreased assimilation and increased Stomatal_Conductance





