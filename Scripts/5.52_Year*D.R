#################
# Year*Drought Mixed Models
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
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 11
y4 <- y3 %>% filter(Site=="S11") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 7
y4 <- y3 %>% filter(Site=="S07") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 10
y4 <- y3 %>% filter(Site=="S10") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 08
y4 <- y3 %>% filter(Site=="S08") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 32
y4 <- y3 %>% filter(Site=="S32") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 29
y4 <- y3 %>% filter(Site=="S29") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 18
y4 <- y3 %>% filter(Site=="S18") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 17
y4 <- y3 %>% filter(Site=="S17") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 16
y4 <- y3 %>% filter(Site=="S16") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic

# Site 36
y4 <- y3 %>% filter(Site=="S36") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic


# Site 15
y4 <- y3 %>% filter(Site=="S15") 
two.poly.exp <- lmer(Experiment_Date ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.exp <- lmer(Experiment_Date ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.exp,two.exp) #select quadratic
two.poly.wc <- lmer(Water_Content ~ poly(Year,2)*Drought + (1|Family) + (1|Block), data=y4)
two.wc <- lmer(Water_Content ~ Year*Drought + (1|Family) + (1|Block), data=y4)
lrtest(two.poly.wc,two.wc) #select quadratic



