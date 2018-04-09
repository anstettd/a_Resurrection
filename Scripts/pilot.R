###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)

# setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Data")
# R projects don't need setwd if everything is kept within the project folder
# this is helpful for multiple users

Y <- read.csv("Data/pilot.csv", header=T) #specify relative paths within the project folder instead of using setwd

#Add climate data to pilot data
wna<-read.csv("Data/pilot_WNA_Normal_1961_1990Y.csv", header=T)
y1<-left_join(Y,wna,by=c("Site"="ID1"))
y<-subset(y1, Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D")
attach(y)
#CMD
lm1<-lm(Flower_Date~Drought+Treatment+CMD)
a1<-Anova(lm1,type=3)
a1
lm2<-lm(Flower~Drought+Treatment+CMD)
a2<-Anova(lm2,type=3)
a2
lm3<-lm(Biomass~Drought+Treatment+CMD)
a3<-Anova(lm3,type=3)
a3
lm4<-lm(Roots~Drought+Treatment+CMD)
a4<-Anova(lm4,type=3)
a4

#MAP
lm5<-lm(Flower_Date~Drought+Treatment+MAP)
a5<-Anova(lm5,type=3)
a5
lm6<-lm(Flower~Drought+Treatment+MAP)
a6<-Anova(lm6,type=3)
a6
lm7<-lm(Biomass~Drought+Treatment+MAP)
a7<-Anova(lm7,type=3)
a7
lm8<-lm(Roots~Drought+Treatment+MAP)
a8<-Anova(lm8,type=3)
a8


#MAT
lm9<-lm(Flower_Date~Drought+Treatment+MAT)
a9<-Anova(lm9,type=3)
a9
lm10<-lm(Flower~Drought+Treatment+MAT)
a10<-Anova(lm10,type=3)
a10
lm11<-lm(Biomass~Drought+Treatment+MAT)
a11<-Anova(lm11,type=3)
a11
lm12<-lm(Roots~Drought+Treatment+MAT)
a12<-Anova(lm12,type=3)
a12



