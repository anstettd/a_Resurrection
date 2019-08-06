#################
# Assessment of correlation between variables
#################
library(tidyverse)
library(ggfortify)
install.packages("ggfortify")

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content", "Biomass",
                            "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (34%)")

