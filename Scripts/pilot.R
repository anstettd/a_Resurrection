###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)

# setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Data")
# R projects don't need setwd if everything is kept within the project folder
# this is helpful for multiple users

Y <- read.csv("Data/pilot.csv", header=T) #specify relative paths within the project folder instead of using setwd

#Add climate data to pilot data
wna<-read.csv("Data/pilot_WNA_Normal_1961_1990Y.csv", header=T)
y1<-left_join(Y,wna,by=c("Site"="ID1"))

#Discard floral foam treatments
y<-subset(y1, Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D") #this leaves the other treatment levels as ghosts, doesn't affect stats but could affect graphing
y <- y1 %>% 
  filter(Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D") %>% 
  droplevels() #this makes sure they are truly discarded (+ alternate pipes syntax)
attach(y)

#CMD
lm1.3way<-lm(Flower_Date~Drought*Treatment*CMD)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way # no support for 3-way interaction

lm1.2ways < -lm(Flower_Date~Drought + Treatment + CMD + Drought*Treatment + Treatment*CMD + Drought*CMD)
a1.2ways <- Anova(lm1.2ways, type=3)
a1.2ways # significant Drought x CMD interaction
visreg(lm1.2ways, xvar="CMD", by="Drought") # rapid evolution of earlier flowering only in dry sites

lm1.2way <- lm(Flower_Date~Drought + Treatment + CMD + Drought*CMD)
a1.2way <- Anova(lm1.2way, type=3)
a1.2way # significant Drought x CMD interaction, still no plasticity (no Treatment effect)

lm1.mains <- lm(Flower_Date~Drought+Treatment+CMD)
a1.mains <- Anova(lm1.mains, type=3)
a1.mains 


lm2.3way <- lm(Flower~Drought*Treatment*CMD)
a2.3way <- Anova(lm2.3way, type=3)
a2.3way # no support for 3-way interaction

lm2.2ways <- lm(Flower~Drought + Treatment + CMD + Drought*Treatment + Drought*CMD + CMD*Treatment)
a2.2ways <- Anova(lm2.2ways, type=3)
a2.2ways #Drought * CMD interaction
visreg(lm2.2ways, xvar="CMD", by="Drought") # reversal in fecundity after drought

lm2.2way <- lm(Flower~Drought + Treatment + CMD + Drought*CMD)
a2.2way <- Anova(lm2.2way, type=3)
a2.2way #Drought * CMD interaction, now a Treatment main effect
visreg(lm2.2way, xvar="CMD", by="Drought") # high-CMD populations no longer more fecund after drought
visreg(lm2.2way, xvar="Treatment")
       
lm2.mains <- lm(Flower~Drought+Treatment+CMD)
a2.mains <- Anova(lm2.mains, type=3)
a2.mains


lm3.3way <- lm(Biomass~Drought*Treatment*CMD)
a3.3way <- Anova(lm3.3way, type=3)
a3.3way # no support for 3-way

lm3.2ways <- lm(Biomass~Drought + Treatment + CMD + Drought*Treatment + Drought*CMD + CMD*Treatment)
a3.2ways <- Anova(lm3.2ways, type=3)
a3.2ways # no support for any 2-ways

lm3.mains <- lm(Biomass~Drought+Treatment+CMD)
a3.mains <- Anova(lm3.mains, type=3)
a3.mains


lm4.3way <- lm(Roots~Drought*Treatment*CMD)
a4.3way <- Anova(lm4.3way, type=3)
a4.3way # no support for 3-way

lm4.2ways <- lm(Roots~Drought + Treatment + CMD + Drought*Treatment + Drought*CMD + CMD*Treatment)
a4.2ways <- Anova(lm4.2ways, type=3)
a4.2ways # no support for any 2-ways

lm4.mains <- lm(Roots~Drought+Treatment+CMD)
a4.mains <- Anova(lm4.mains, type=3)
a4.mains

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



