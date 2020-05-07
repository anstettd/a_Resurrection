#################
# Site*Year*Drought Mixed Models
#################
library(tidyverse)
#library(lme4)
#library(lmtest)
library(car)
library(visreg)
library(cowplot)
library(lme4)
library(lmtest)


y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors

y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))

######################################################################################################################
##### SLA ####

fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.SLA <- lmer(SLA ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), 
                   data=y5)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model

######################################################################################################################
##### Date of Flowering

fullmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block)  + (1|Site.Lat), data=y5) #3way interaction model
# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.exp, no3way.exp) # No sat diff. Retain 2-way.

#### drop 2ways

## 1 ## Here I remove Region * Drought first
noRxD.exp <- lmer(Experiment_Date ~ Drought*Year+ Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.exp,noRxD.exp) #Full 2-way is supported


## 2 ## Remove Drought * Year
noDxY.exp <- lmer(Experiment_Date ~ Region*Drought + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.exp,noDxY.exp) #Test Model with no Drought * Year, with full 2-way. No difference, pick no Drought * Year
  # A # Remove Region * Drought
RY.D.exp<- lmer(Experiment_Date ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.exp,RY.D.exp) # Region*Drought + Region*Year significantlly better
  # B # Remove Region * Year
RD.Y.exp<- lmer(Experiment_Date ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.exp,RD.Y.exp) # No difference, select Region*Drought + Year (simpler model)
    # Remove all interactions
nox.exp<- lmer(Experiment_Date ~ Region + Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), 
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(RD.Y.exp,nox.exp) # Region*Drought + Year 


## 3 ## Remove Region * Year instead.
noRxY.exp <- lmer(Experiment_Date ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.exp,noRxY.exp) #noRxY.exp is equivalent to full 2-way model.
  # A # Remove Drought * Year
DY.R.exp<- lmer(Experiment_Date ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat), 
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(noRxY.exp,DY.R.exp) # No difference, select Region*Drought + Year (simpler model)
  # B # Remove Region * Drought
RD.Y.exp<- lmer(Experiment_Date ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxY.exp,RD.Y.exp) # No difference

######################################################################################################################
##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
# drop 3way
no3way.wc <- lmer(Water_Content ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.wc, no3way.wc) # two-way model supported

# drop 2ways
## 2 ## Remove Drought * Year
noDxY.wc <- lmer(Water_Content ~ Region*Drought + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(no3way.wc,noDxY.wc) #Simpler model significally supported
# A # Remove Region * Drought
RY.D.wc<- lmer(Water_Content  ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.wc,RY.D.wc) #Simpler model significally supported
# B # Remove Region * Year
RY.Y.wc<- lmer(Water_Content  ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.wc,RY.Y.wc) #Simpler model significally supported
# Remove all interactions
nox.wc<- lmer(Water_Content  ~ Region + Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(RY.Y.wc,nox.wc) # no interactions model significantly better.
lrtest(RY.D.wc,nox.wc) # no interactions model significantly better.

noR.wc<- lmer(Water_Content ~ Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat),
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
noD.wc<- lmer(Water_Content  ~ Region + Year + (1|Family) + (1|Block) + (1|Site.Lat), 
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
noY.wc<- lmer(Water_Content  ~ Region + Drought + (1|Family) + (1|Block) + (1|Site.Lat), 
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(nox.wc,noR.wc) # No difference
lrtest(nox.wc,noY.wc) # More complex model supported
lrtest(nox.wc,noD.wc) # More complex model supported

Y.wc<- lmer(Water_Content ~ Year + (1|Family) + (1|Block) + (1|Site.Lat), 
            control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
D.wc<- lmer(Water_Content ~ Drought + (1|Family) + (1|Block) + (1|Site.Lat), 
            control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)

lrtest(noR.wc,Y.wc) # Drought + Year supported
lrtest(noR.wc,D.wc) # Drought supported

none.wc<-lmer(Water_Content ~ (1|Family) + (1|Block) + (1|Site.Lat),
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(D.wc,none.wc) # Drought supported



### Other removals show same pattern
## 1 ## Here I remove Region * Drought first
noRxD.wc <- lmer(Water_Content ~ Drought*Year+ Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.wc,noRxD.wc) #Simpler model significally supported
# A # Remove Drought * Year
RY.D.wc<- lmer(Water_Content  ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxD.wc,RY.D.wc) #Simpler model significally supported
# B # Remove Region * Year
DY.R.wc<- lmer(Water_Content  ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxD.wc,RY.Y.wc) #Simpler model significally supported

## 3 ## Remove Region * Year instead.
noRxY.wc <- lmer(Water_Content ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.wc,noRxY.wc) #Simpler model significally supported 
noRxY.wc <- lmer(Water_Content  ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat), 
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.wc,noRxY.wc) #Simpler model significally supportedl. 
# A # Remove Region * Drought
lrtest(noRxY.wc,RY.D.wc) #Simpler model significally supported
# B # Remove Drought * Year
DY.R.wc<- lmer(Water_Content  ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxY.wc,DY.R.wc) #Simpler model significally supported)

######################################################################################################################
######## Assimilation
fullmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.A <- lmer(Assimilation ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.A, no3way.A) # No difference

# drop 2ways

## 1 ## Here I remove Region * Drought first
noRxD.A <- lmer(Assimilation ~ Drought*Year+ Region*Year + (1|Family) + (1|Block) + (1|Site.Lat),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(no3way.A,noRxD.A) #Full 2-way is supported

## 2 ## Remove Drought * Year
noDxY.A <- lmer(Assimilation ~ Region*Drought + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), 
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(no3way.A,noDxY.A) #No difference
  # A # Remove Region * Drought
RY.D.A<- lmer(Assimilation ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), 
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(noDxY.A,RY.D.A) # Region*Drought + Region*Year significantlly better
  # B # Remove Region * Year
RD.Y.A<- lmer(Assimilation ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.A,RD.Y.A) # No difference, select Region*Drought + Year (simpler model)
# Remove all interactions
nox.A<- lmer(Assimilation ~ Region + Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(RD.Y.A,nox.A) # Region*Drought + Year 

## 3 ## Remove Region * Year instead.
noRxY.A <- lmer(Assimilation ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.A,noRxY.A) #noRxY.A is equivalent to full 2-way model.
# A # Remove Drought * Year
DY.R.A<- lmer(Assimilation ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat),
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(noRxY.A,DY.R.A) # No difference, select Region*Drought + Year (simpler model)
# B # Remove Region * Drought
RD.Y.A<- lmer(Assimilation ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxY.A,RD.Y.A) # No difference

######################################################################################################################
######## Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
# drop 3way
no3way.gs <- lmer(Stomatal_Conductance ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.gs, no3way.gs) # two-way model supported

# drop 2ways
## 2 ## Remove Drought * Year
noDxY.gs <- lmer(Stomatal_Conductance ~ Region*Drought + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
lrtest(no3way.gs,noDxY.gs) #Simpler model significally supported
# A # Remove Region * Drought
RY.D.gs<- lmer(Stomatal_Conductance  ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.gs,RY.D.gs) #Simpler model significally supported
# B # Remove Region * Year
RY.Y.gs<- lmer(Stomatal_Conductance  ~ Region*Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.gs,RY.Y.gs) #Simpler model significally supported
# Remove all interactions
nox.gs<- lmer(Stomatal_Conductance  ~ Region + Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(RY.Y.gs,nox.gs) # no interactions model significantly better.
lrtest(RY.D.gs,nox.gs) # no interactions model significantly better.

noR.gs<- lmer(Stomatal_Conductance ~ Drought + Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
noD.gs<- lmer(Stomatal_Conductance  ~ Region + Year + (1|Family) + (1|Block) + (1|Site.Lat), 
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
noY.gs<- lmer(Stomatal_Conductance  ~ Region + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(nox.gs,noR.gs) # No difference
lrtest(nox.gs,noY.gs) # More complex model supported
lrtest(nox.gs,noD.gs) # More complex model supported

Y.gs<- lmer(Stomatal_Conductance ~ Year + (1|Family) + (1|Block) + (1|Site.Lat),data=y5)
D.gs<- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block) + (1|Site.Lat),
            control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)

lrtest(noR.gs,Y.gs) # Drought + Year supported
lrtest(noR.gs,D.gs) # Drought supported

none.gs<-lmer(Stomatal_Conductance ~ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(D.gs,none.gs) # Drought supported



### Other removals show same pattern
## 1 ## Here I remove Region * Drought first
noRxD.gs <- lmer(Stomatal_Conductance ~ Drought*Year+ Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.gs,noRxD.gs) #Simpler model significally supported
# A # Remove Drought * Year
RY.D.gs<- lmer(Stomatal_Conductance  ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxD.gs,RY.D.gs) #Simpler model significally supported
# B # Remove Region * Year
DY.R.gs<- lmer(Stomatal_Conductance  ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxD.gs,RY.Y.gs) #Simpler model significally supported

## 3 ## Remove Region * Year instead.
noRxY.gs <- lmer(Stomatal_Conductance ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.gs,noRxY.gs) #Simpler model significally supported 
noRxY.gs <- lmer(Stomatal_Conductance  ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat), 
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.gs,noRxY.gs) #Simpler model significally supportedl. 
# A # Remove Region * Drought
lrtest(noRxY.gs,RY.D.gs) #Simpler model significally supported
# B # Remove Drought * Year
DY.R.gs<- lmer(Stomatal_Conductance  ~ Drought*Year + Region + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noRxY.gs,DY.R.gs) #Simpler model significally supported)


##### Satterthwaite approx using lmertest package

#SLA
anova(fullmod.SLA)
summary(fullmod.SLA)
step(fullmod.SLA)

#Date of Flowering
anova(DY.R.exp)
summary(DY.R.exp)
step(fullmod.exp)

#Water_Content
anova(D.wc)
summary(D.wc)
step(fullmod.wc)

#Assimilation
anova(DY.R.A)
summary(DY.R.A)
step(fullmod.A)

#Stomatal Conductance
anova(D.gs)
summary(D.gs)
step(fullmod.gs)



