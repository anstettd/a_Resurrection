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


#Assess correlation among response variables
#pairs(~ Experiment_Date + Flower_num + log(SLA) + Water_Content)

pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content", "Biomass",
                            "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (34%)")

# so, there are essentially 2 axes of variation that we are seeing with these 4 variables. flowering date and flower number are negatively correlated (makes sense) and sla and water content are also negatively correlated (structurally thicker leaves are also more succulent?)


####### Correlations between climate and climate anomaly for CMD, MAP & MAT
#install.packages("Hmisc")
library("Hmisc")
clim.amom.cor<-slopes.rapid.clim %>% select(MAT.clim.s,MAP.clim.s, CMD.clim.s, C_Anomaly.CMD,
                                            C_Anomaly.MAT, C_Anomaly.MAP) #Generate list 
clim.amom.cor.m<-as.matrix(clim.amom.cor) # make into a matrix
rcorr(clim.amom.cor.m) # get all correlation coeff
