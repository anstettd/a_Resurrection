#################
# Assessment of correlation between variables
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
library(Hmisc)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
#year.anom <- read.csv("Data/year.anom.csv", header=T)
slopes.rapid.clim <- read.csv("Data/slopes.rapid.clim.csv", header=T) #Imports main dataset

#Anomaly change over time
#Year vs CMD.anom
year.CMD.anom <- ggplot(y3, aes(Year,CMD.anom))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.CMD.anom + theme(legend.text = element_text(size = 12, face = "bold"),
axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                  axis.text.y = element_text(size=14,face="bold"),
                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="CMD Anomaly")
#Year vs CMD.weath
year.CMD.weath <- ggplot(y3, aes(Year,CMD.weath))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.CMD.weath + theme(legend.text = element_text(size = 12, face = "bold"),
                      axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                      axis.text.y = element_text(size=14,face="bold"),
                      axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                      axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="CMD Weather")
#Year vs MAT.anom
year.MAT.anom <- ggplot(y3, aes(Year,MAT.anom))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.MAT.anom + theme(legend.text = element_text(size = 12, face = "bold"),
                      axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                      axis.text.y = element_text(size=14,face="bold"),
                      axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                      axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="MAT Anomaly")
#Year vs MAT.weath
year.MAT.weath <- ggplot(y3, aes(Year,MAT.weath))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.MAT.weath + theme(legend.text = element_text(size = 12, face = "bold"),
                       axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                       axis.text.y = element_text(size=14,face="bold"),
                       axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                       axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="MAT Weather")
#Year vs MAP.anom
year.MAP.anom <- ggplot(y3, aes(Year,MAP.anom))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.MAP.anom + theme(legend.text = element_text(size = 12, face = "bold"),
                      axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                      axis.text.y = element_text(size=14,face="bold"),
                      axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                      axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="MAP Anomaly")
#Year vs CMD.weath
year.MAP.weath <- ggplot(y3, aes(Year,MAP.weath))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.MAP.weath + theme(legend.text = element_text(size = 12, face = "bold"),
                       axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                       axis.text.y = element_text(size=14,face="bold"),
                       axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                       axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="MAP Weather")




####### Correlations between climate and climate anomaly for CMD, MAP & MAT

# for whole dataset
y3.clim.amom.cor <- y3 %>% select(CMD.clim.s, MAT.clim.s, MAP.clim.s, CMD.anom.s, MAT.anom.s, MAP.anom.s) #Generate list 
y3.clim.amom.cor.m<-as.matrix(y3.clim.amom.cor) # make into a matrix
rcorr(y3.clim.amom.cor.m) # get all correlation coeff

#Population averages with cumulative anomaly
clim.amom.cor<-slopes.rapid.clim %>% select(CMD.clim.s,MAT.clim.s,MAP.clim.s, C_Anomaly.CMD.s,
                                            C_Anomaly.MAT.s, C_Anomaly.MAP.s) #Generate list 
clim.amom.cor.m<-as.matrix(clim.amom.cor) # make into a matrix
rcorr(clim.amom.cor.m) # get all correlation coeff
#MAT.clim and MAP.clm are too highly correlated to other variables and will not be used in multiple regression.
#Run multiple regression with CMD.clim.s , C_Anomaly.CMD.s, C_Anomaly.MAT.s, C_Anomaly.MAP.s.
#See 5.8


#Assess PCA relationships among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content", "Biomass",
                            "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (34%)")
# so, there are essentially 2 axes of variation that we are seeing with these 4 variables. flowering date and flower number are negatively correlated (makes sense) and sla and water content are also negatively correlated (structurally thicker leaves are also more succulent?)

