##########
#Other environmental varialbes (discarded)
##########

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
y <- y1 %>% 
  filter(Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D") %>% 
  droplevels() #this makes sure they are truly discarded (+ alternate pipes syntax)
attach(y)


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
