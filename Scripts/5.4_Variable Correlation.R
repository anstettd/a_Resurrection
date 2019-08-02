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

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
slopes.rapid.clim <- read.csv("Data/slopes.rapid.clim.csv", header=T) #Imports main dataset


#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content", "Biomass",
                            "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (34%)")

# so, there are essentially 2 axes of variation that we are seeing with these 4 variables. flowering date and flower number are negatively correlated (makes sense) and sla and water content are also negatively correlated (structurally thicker leaves are also more succulent?)


####### Correlations between climate and climate anomaly for CMD, MAP & MAT
#install.packages("Hmisc")
library("Hmisc")
clim.amom.cor<-slopes.rapid.clim %>% select(MAT.clim.s,MAP.clim.s, CMD.clim.s, C_Anomaly.CMD.s,
                                            C_Anomaly.MAT.s, C_Anomaly.MAP.s) #Generate list 
clim.amom.cor.m<-as.matrix(clim.amom.cor) # make into a matrix
rcorr(clim.amom.cor.m) # get all correlation coeff
#MAT.clim and MAP.clm are too highly correlated to other variables and will not be used in multiple regression.
#Run multiple regression with CMD.clim.s , C_Anomaly.CMD.s, C_Anomaly.MAT.s, C_Anomaly.MAP.s.
#See 5.8
