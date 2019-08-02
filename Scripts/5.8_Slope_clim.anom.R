#################
# Rate of evolutionary change predicted by climate and anomaly
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
library(MuMIn)
slopes.rapid.clim <- read.csv("Data/slopes.rapid.clim.csv", header=T) #Imports main dataset
####### Trait rate of change predicted by historical climate and anomaly ########
#Carry out multiple regression with CMD.clim, C_Anomaly.CMD, C_Anomaly.MAT, C_Anomaly.MAP

#Notes: I have gone water main effects only, and only plot graphs when resutls are at least marginally significant. 
#I could do all graphs, if we want that.

###Slope flowering wet
lm.flowering <- lm(Flowering_Wet~CMD.clim.s+C_Anomaly.CMD.s+C_Anomaly.MAT.s+C_Anomaly.MAP.s, #set up multiple regresion model
                   data=slopes.rapid.clim,na.action=na.fail) 
lm.flowering.d <- dredge(lm.flowering,beta=T,evaluate=T, rank=AIC) #Drege command
summary(model.avg(lm.flowering.d, subset= delta <=2)) #provide model averaging output
#lapply(get.models(lm.flowering.d, subset= delta <2), r.squaredGLMM) #if you want R^2 estimated
# Slope of flowering time evolution not significantly predicted by climate CMD & and anomalies under wet condition

####Slope Water Content
lm.flowering <- lm(Water_Content_Wet~CMD.clim.s+C_Anomaly.CMD.s+C_Anomaly.MAT.s+C_Anomaly.MAP.s, 
                   data=slopes.rapid.clim,na.action=na.fail) 
lm.flowering.d <- dredge(lm.flowering,beta=T,evaluate=T, rank=AIC) 
summary(model.avg(lm.flowering.d, subset= delta <=2))
# Slope of water content evolution weakly predicted by cumulative CMD anomaly Anomaly and historical CMD.

#CMD Anomaly predicting water content
slope_wet <- ggplot(slopes.rapid.clim, aes(C_Anomaly.CMD.s,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Cumulative CMD Anomaly") +
  scale_y_continuous(name="Slope of Flowering Date") 
#Historical CMD predicting water content
slope_wet <- ggplot(slopes.rapid.clim, aes(CMD.clim.s,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="CMD 1980-2010") +
  scale_y_continuous(name="Slope of Flowering Date") 

#Slope SLA
lm.flowering <- lm(SLA_Wet~CMD.clim.s+C_Anomaly.CMD.s+C_Anomaly.MAT.s+C_Anomaly.MAP.s, 
                   data=slopes.rapid.clim,na.action=na.fail) 
lm.flowering.d <- dredge(lm.flowering,beta=T,evaluate=T, rank=AIC) 
summary(model.avg(lm.flowering.d, subset= delta <=2))
# Slope of SLA evolution not significantly predicted by climate CMD & and anomalies under wet condition

#Slope Stomatal conductance wet
lm.flowering <- lm(Stomatal_Conductance_Wet~CMD.clim.s+C_Anomaly.CMD.s+C_Anomaly.MAT.s+C_Anomaly.MAP.s, 
                   data=slopes.rapid.clim,na.action=na.fail) 
lm.flowering.d <- dredge(lm.flowering,beta=T,evaluate=T, rank=AIC) 
summary(model.avg(lm.flowering.d, subset= delta <=2))
# Slope of stomatal conductance evolution strongly predicted by cumulative CMD anomaly Anomaly and historical MAT.

#CMD Anomaly predicting Stomatal Conductance
slope_wet <- ggplot(slopes.rapid.clim, aes(C_Anomaly.CMD.s,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Cumulative CMD Anomaly") +
  scale_y_continuous(name="Slope of Stomatal Conductance") 
#Historical CMD predicting water content
slope_wet <- ggplot(slopes.rapid.clim, aes(CMD.clim.s,Stomatal_Conductance_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="CMD 1980-2010") +
  scale_y_continuous(name="Slope of Stomatal Conductance")

#Slope Assimilation conductence wet
lm.flowering <- lm(Assimilation_Wet~CMD.clim.s+C_Anomaly.CMD.s+C_Anomaly.MAT.s+C_Anomaly.MAP.s, 
                   data=slopes.rapid.clim,na.action=na.fail) 
lm.flowering.d <- dredge(lm.flowering,beta=T,evaluate=T, rank=AIC) 
summary(model.avg(lm.flowering.d, subset= delta <=2)) 
# Slope of Assimilation evolution not significantly predicted by climate CMD & and anomalies under wet condition


