#################
# Plot CMD.anom over time per year
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
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
wna_anom <- read.csv("Data/wna_all.csv", header=T) #Imports climate dataset by site/year

#CMD.anom versus Year
anom.year <- ggplot(wna_anom, aes(Year,CMD.anom,color=Site))+
  geom_point(size=2)+
  geom_line()+
  theme_minimal()
#All in one Graph
anom.year + theme(legend.text = element_text(size = 12, face = "bold"), 
                                            axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                                            axis.text.y = element_text(size=14,face="bold"),
                                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) 
anom.year.f <- anom.year + facet_wrap( ~ Site, ncol=4)
anom.year.f + theme(legend.text = element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                                     axis.text.y = element_text(size=14,face="bold"),
                                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Climate Moisture Deficit Anomaly")


#CMD.weath versus Year
weath.year <- ggplot(wna_anom, aes(Year,CMD.weath,color=Site))+
  geom_point(size=2)+
  geom_line()+
  theme_minimal()
#All in one Graph
weath.year + theme(legend.text = element_text(size = 12, face = "bold"), 
                  axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) 
weath.year.f <- weath.year + facet_wrap( ~ Site, ncol=4)
weath.year.f + theme(legend.text = element_text(size = 12, face = "bold"),
                    axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                    axis.text.y = element_text(size=14,face="bold"),
                    axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                    axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Climate Moisture Deficit")






####### Correlations between climate and climate anomaly for CMD, MAP & MAT
# are historical climate and anomalies correlated?
y3.clim.amom.cor <- y3 %>% select(CMD.clim.s, MAT.clim.s, MAP.clim.s, CMD.anom.s, MAT.anom.s, MAP.anom.s) #Generate list; remember that MAP.s are on log scale
y3.clim.amom.cor.m<-as.matrix(y3.clim.amom.cor) # make into a matrix
rcorr(y3.clim.amom.cor.m) # get all correlation coeff



