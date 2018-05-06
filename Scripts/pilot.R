###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)

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

### Inspect candidate climate variables for collinearity
clim <- y %>% 
  select(CMD, MAP, MAT)
pairs(clim)
cor.test(clim$CMD, clim$MAP)
cor.test(clim$CMD, clim$MAT)
cor.test(clim$MAT, clim$MAP)
# they are all highly correlated, not much independent information across them
# so its probably best to use CMD

########
###CMD Selected as the predictor enviornemntal variable 
########
# Flowering time
lm1.3way<-lm(Flower_Date~Drought*Treatment*CMD)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way # no support for 3-way interaction

lm1.2ways <- lm(Flower_Date ~ Drought + Treatment + CMD + Drought*Treatment + Treatment*CMD + Drought*CMD)
a1.2ways <- Anova(lm1.2ways, type=3)
a1.2ways # significant Drought x CMD interaction
vispred <- visreg(lm1.2ways, xvar="CMD", by="Drought") # rapid evolution of earlier flowering only in wet sites
visres <- as.data.frame(vispred$res)
preds <- ggeffect(lm1.2ways, terms=c("CMD", "Drought")) # rapid evolution of earlier flowering only in wet sites
ggplot(data=preds, aes(x=x, y=predicted, color=group)) +
  geom_line() + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.5) +
  #geom_point(data=y, aes(x=CMD, y=Flower_Date, color=Drought)) +
  geom_point(data=visres, aes(x=CMD, y=visregRes, color=Drought)) +
  xlab("Climatic moisture deficit") +
  ylab("Flowering time") +
  theme_classic()
ggsave(filename="Graphs/FlowerTime_CMDbyDrought.png", device="png")

lm1.lat <- lm(Flower_Date ~ Drought + Treatment + Lat + Drought*Treatment + Treatment*Lat + Drought*Lat)
a1.2ways <- Anova(lm1.2ways, type=3)
a1.2ways # significant Drought x Lat interaction
vispreds <- visreg(lm1.lat, xvar="Lat", by="Drought") # rapid evolution of earlier flowering only in north sites
visres <- as.data.frame(vispreds$res)
preds <- ggeffect(lm1.lat, terms=c("Lat", "Drought")) # rapid evolution of earlier flowering only in north
ggplot(data=preds, aes(x=x, y=predicted, color=group)) +
  geom_line() + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.5) +
  geom_point(data=visres, aes(x=Lat, y=visregRes, color=Drought)) +
  xlab("Latitude") +
  ylab("Flowering time") +
  theme_classic()
ggsave(filename="Graphs/FlowerTime_LatbyDrought.png", device="png")

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

