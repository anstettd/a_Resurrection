#################
# Rate of evolutionary change (slopes) ca
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

####### Trait rate of change predicted by historical climate and anomaly ########

attach(slopes.rapid.clim)


############# Comparison of rates of change of evolution between different traits
#Ft vs wc slope Wet
lm.slope_wet<-lm(Water_Content_Wet~Flowering_Wet)
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

#Ft vs wc slope Dry
lm.slope_dry<-lm(Water_Content_Dry~Flowering_Dry)
summary(lm.slope_dry)
slope_dry<-ggplot(slopes.rapid.clim, aes(Water_Content_Dry,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)
slope_dry + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=12,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))

##### Include comparison between flowering time and stomatal conductence here





#Carry out multiple regression with CMD.clim, C_Anomaly.CMD, C_Anomaly.MAT, C_Anomaly.MAP
###### Slopes #####
#Slope flowering dry
#Slope flowering wet

#Slope Water_Content dry
#Slope Water_Content wet

#Slope SLA dry
#Slope SLA wet

#Slope Stomatal conductence dry
#Slope Stomatal conductence wet

########### Year/site means #############

#Begining Mean flowering dry
#Begining Mean flowering wet

#End Meanflowering dry
#End Mean flowering wet


#Slope Water_Content dry
#Slope Water_Content wet

#Slope SLA dry
#Slope SLA wet

#Slope Stomatal conductence dry
#Slope Stomatal conductence wet













##### Univariate comparisons ####
#Slope flowering dry
lm.flowering.clim<-lm(Flowering_Dry~CMD.clim)
summary(lm.flowering.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)

lm.flowering.anom<-lm(Flowering_Dry~Cumulative_Anomaly.CMD)
summary(lm.flowering.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly.CMD,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)

#Slope flowering wet
lm.flowering.clim<-lm(Flowering_Wet~CMD.clim)
summary(lm.flowering.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)

lm.flowering.anom<-lm(Flowering_Wet~Cumulative_Anomaly.CMD)
summary(lm.flowering.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly.CMD,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)

##
#Slope Water_Content dry
lm.Water_Content.clim<-lm(Water_Content_Dry~CMD.clim)
summary(lm.Water_Content.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Water_Content_Dry))+
  geom_point()+
  geom_smooth(method=lm)

lm.Water_Content.anom<-lm(Water_Content_Dry~Cumulative_Anomaly.CMD)
summary(lm.Water_Content.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly.CMD,Water_Content_Dry))+
  geom_point()+
  geom_smooth(method=lm)

#Slope Water_Content wet
lm.Water_Content.clim<-lm(Water_Content_Wet~CMD.clim)
summary(lm.Water_Content.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)

lm.Water_Content.anom<-lm(Water_Content_Wet~Cumulative_Anomaly.CMD)
summary(lm.Water_Content.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly.CMD,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)
#########################
