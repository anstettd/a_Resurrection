###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)

### Data prep
Y <- read.csv("Data/pilot.csv", header=T) #specify relative paths within the project folder instead of using setwd

#Add climate data to pilot data
wna<-read.csv("Data/pilot_WNA_Normal_1961_1990Y.csv", header=T)
y1<-left_join(Y,wna,by=c("Site"="ID1"))

#Discard floral foam treatments
y<-subset(y1, Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D") 
#this leaves the other treatment levels as ghosts, doesn't affect stats but could affect graphing
y <- y1 %>% 
  filter(Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D") %>% 
  droplevels() #this makes sure they are truly discarded (+ alternate pipes syntax)
attach(y)

### Inspect candidate climate variables for collinearity
clim <- y %>% 
  select(CMD, MAP, MAT)
pairs(clim)
cor.test(clim$CMD, clim$MAP)
cor.test(clim$CMD, clim$MAT)
cor.test(clim$MAT, clim$MAP)
# they are all highly correlated, not much independent information across them

###CMD
# Flowering time
lm1.3way<-lm(Flower_Date~Drought*Treatment*CMD)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way # no support for 3-way interaction

lm1.2ways < -lm(Flower_Date~Drought + Treatment + CMD + Drought*Treatment + Treatment*CMD + Drought*CMD)
a1.2ways <- Anova(lm1.2ways, type=3)
a1.2ways # significant Drought x CMD interaction
visreg(lm1.2ways, xvar="CMD", by="Drought") # rapid evolution of earlier flowering only in wet sites

lm1.2way <- lm(Flower_Date~Drought + Treatment + CMD + Drought*CMD)
a1.2way <- Anova(lm1.2way, type=3)
a1.2way # significant Drought x CMD interaction, still no plasticity (no Treatment effect)

lm1.mains <- lm(Flower_Date~Drought+Treatment+CMD)
a1.mains <- Anova(lm1.mains, type=3)
a1.mains 

# Flower number
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

# Biomass
lm3.3way <- lm(Biomass~Drought*Treatment*CMD)
a3.3way <- Anova(lm3.3way, type=3)
a3.3way # no support for 3-way

lm3.2ways <- lm(Biomass~Drought + Treatment + CMD + Drought*Treatment + Drought*CMD + CMD*Treatment)
a3.2ways <- Anova(lm3.2ways, type=3)
a3.2ways # no support for any 2-ways

lm3.mains <- lm(Biomass~Drought+Treatment+CMD)
a3.mains <- Anova(lm3.mains, type=3)
a3.mains

# Roots
lm4.3way <- lm(Roots~Drought*Treatment*CMD)
a4.3way <- Anova(lm4.3way, type=3)
a4.3way # no support for 3-way

lm4.2ways <- lm(Roots~Drought + Treatment + CMD + Drought*Treatment + Drought*CMD + CMD*Treatment)
a4.2ways <- Anova(lm4.2ways, type=3)
a4.2ways # no support for any 2-ways

lm4.mains <- lm(Roots~Drought+Treatment+CMD)
a4.mains <- Anova(lm4.mains, type=3)
a4.mains


### MAP
# Flowering time
lm5.3way <- lm(Flower_Date~Drought*Treatment*MAP)
a5.3way <- Anova(lm5.3way, type=3)
a5.3way # marginal 3-way interaction

lm5.2ways <- lm(Flower_Date~Drought+Treatment+MAP + Drought*Treatment + Drought*MAP + MAP*Treatment)
a5.2ways <- Anova(lm5.2ways, type=3)
a5.2ways # strong Drought*MAP interaction
visreg(lm5.2ways, xvar="MAP", by="Drought") # earlier flowering only evolved in places with high MAP

lm5.2way <- lm(Flower_Date~Drought+Treatment+MAP + Drought*MAP)
a5.2way <- Anova(lm5.2way, type=3)
a5.2way # dropping NS 2-ways allows Treatment to be marginally significant

lm5.mains <- lm(Flower_Date~Drought+Treatment+MAP)
a5.mains <- Anova(lm5.mains, type=3)
a5.mains

# Flower number
lm6.3way<-lm(Flower~Drought*Treatment*MAP)
a6.3way<-Anova(lm6.3way,type=3)
a6.3way # no support for 3-way

lm6.2ways<-lm(Flower~Drought+Treatment+MAP + Drought*Treatment + Drought*MAP + MAP*Treatment)
a6.2ways<-Anova(lm6.2ways,type=3)
a6.2ways # Drought * MAP interaction
visreg(lm6.2ways, xvar="MAP", by="Drought")

lm6.mains<-lm(Flower~Drought+Treatment+MAP)
a6.mains<-Anova(lm6.mains,type=3)
a6.mains

# Biomass
lm7.3way<-lm(Biomass~Drought*Treatment*MAP)
a7.3way<-Anova(lm7.3way,type=3)
a7.3way # no support for 3-way

lm7.2ways<-lm(Biomass~Drought+Treatment+MAP + Drought*Treatment + Drought*MAP + MAP*Treatment)
a7.2ways<-Anova(lm7.2ways,type=3)
a7.2ways # no support for any 2-ways

lm7.mains<-lm(Biomass~Drought+Treatment+MAP)
a7.mains<-Anova(lm7.mains,type=3)
a7.mains

# Roots
lm8.3way<-lm(Roots~Drought*Treatment*MAP)
a8.3way<-Anova(lm8.3way,type=3)
a8.3way # no support for 3-way

lm8.2ways<-lm(Roots~Drought+Treatment+MAP + Drought*Treatment + Drought*MAP + MAP*Treatment)
a8.2ways<-Anova(lm8.2ways,type=3)
a8.2ways # no support for any 2-ways

lm8.mains<-lm(Roots~Drought+Treatment+MAP)
a8.mains<-Anova(lm8.mains,type=3)
a8.mains


###MAT
# Flowering time
lm9.3way<-lm(Flower_Date~Drought*Treatment*MAT)
a9.3way<-Anova(lm9.3way,type=3)
a9.3way # marginal 3-way interaction

lm9.2ways<-lm(Flower_Date~Drought+Treatment+MAT + Drought*Treatment + Drought*MAT + MAT*Treatment)
a9.2ways<-Anova(lm9.2ways,type=3)
a9.2ways # marginal Drought x MAT interaction

lm9.2way<-lm(Flower_Date~Drought+Treatment+MAT + Drought*MAT)
a9.2way<-Anova(lm9.2way,type=3)
a9.2way # significant Drought x MAT interaction when other 2-ways dropped
visreg(lm9.2way, xvar="MAT", by="Drought") # rapid evolution only in cooler sites

lm9.mains<-lm(Flower_Date~Drought+Treatment+MAT)
a9.mains<-Anova(lm9.mains,type=3)
a9.mains

# Flower number
lm10.3way<-lm(Flower~Drought*Treatment*MAT)
a10.3way<-Anova(lm10.3way,type=3)
a10.3way # no support for 3-way

lm10.2ways<-lm(Flower~Drought+Treatment+MAT + Drought*Treatment + Drought*MAT + MAT*Treatment)
a10.2ways<-Anova(lm10.2ways,type=3)
a10.2ways # no support for any 2-ways

lm10.mains<-lm(Flower~Drought+Treatment+MAT)
a10.mains<-Anova(lm10.mains,type=3)
a10.mains

# Biomass
lm11.3way<-lm(Biomass~Drought*Treatment*MAT)
a11.3way<-Anova(lm11.3way,type=3)
a11.3way # no support for 3-way

lm11.2ways<-lm(Biomass~Drought+Treatment+MAT + Drought*Treatment + Drought*MAT + MAT*Treatment)
a11.2ways<-Anova(lm11.2ways,type=3)
a11.2ways # no support for any 2-ways

lm11.mains<-lm(Biomass~Drought+Treatment+MAT)
a11.mains<-Anova(lm11.mains,type=3)
a11.mains

# Roots
lm12.3way<-lm(Roots~Drought*Treatment*MAT)
a12.3way<-Anova(lm12.3way,type=3)
a12.3way # no support for 3-way

lm12.2ways<-lm(Roots~Drought+Treatment+MAT + Drought*Treatment + Drought*MAT + MAT*Treatment)
a12.2ways<-Anova(lm12.2ways,type=3)
a12.2ways # no support for any 2-ways

lm12.mains<-lm(Roots~Drought+Treatment+MAT)
a12.mains<-Anova(lm12.mains,type=3)
a12.mains



