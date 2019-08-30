#################
# Put site/year means into PCA coordinate system
#################
library(tidyverse)
library(ggfortify)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
trait.means.w <- read.csv("Data/trait.means.w.csv", header=T)
trait.means.d <- read.csv("Data/trait.means.d.csv", header=T)
trait.means.w <- trait.means.w %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))
trait.means.d <- trait.means.d %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))

#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (46%)", ylab="PC2 (21%)")

screeplot(pc1)

#convert wet means into pc axes
tmeans.w.redu <- trait.means.w %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")
pc.mean.values.w <- scale(tmeans.w.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.w<-trait.means.w %>% select("ID_Year","Site.Lat","Site","Year","Drought")
pc.means.w<-cbind(pc.means.w,pc.mean.values.w)
write.csv(pc.means.w,'Data/pc.means.w.csv') #Export file

#convert drought means into pc axes
tmeans.d.redu <- trait.means.d %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")
pc.mean.values.d <- scale(tmeans.d.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.d<-trait.means.d %>% select("ID_Year","Site.Lat","Site","Year","Drought")
pc.means.d<-cbind(pc.means.d,pc.mean.values.d)
write.csv(pc.means.d,'Data/pc.means.d.csv') #Export file