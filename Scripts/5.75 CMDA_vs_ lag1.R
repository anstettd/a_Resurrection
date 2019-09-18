#################
# CMDA * D versus lag_1 * D per site
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
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors



# Site 2
y4 <- y3 %>% filter(Site=="S02")
#Experiment Date
CMDA.D.S2.exp <- lmer(Experiment_Date ~ CMD.anom.s*Drought + (1|Family) + (1|Block), data=y4)
CMDA_1.D.S2.exp <- lmer(Experiment_Date ~ CMD.anom.1.s*Drought + (1|Family) + (1|Block), data=y4)
lrtest(CMDA.D.S2.exp,CMDA_1.D.S2.exp) #Select CMDA




# Site 11
y4 <- y3 %>% filter(Site=="S11") 



# Site 7
y4 <- y3 %>% filter(Site=="S07") 



# Site 10
y4 <- y3 %>% filter(Site=="S10") 


# Site 08
y4 <- y3 %>% filter(Site=="S08") 


# Site 32
y4 <- y3 %>% filter(Site=="S32") 

# Site 29
y4 <- y3 %>% filter(Site=="S29") 


# Site 18
y4 <- y3 %>% filter(Site=="S18") 


# Site 17
y4 <- y3 %>% filter(Site=="S17") 


# Site 16
y4 <- y3 %>% filter(Site=="S16") 


# Site 36
y4 <- y3 %>% filter(Site=="S36") 


# Site 15
y4 <- y3 %>% filter(Site=="S15") 
