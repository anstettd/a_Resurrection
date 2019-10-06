#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Perform model selection for each vital rate for subsequent use in IPMs for southern regions (S1, S2)
############# Vital rates include survival, growth, flowering, and fruit count
############# Fixed effect: size; Random effects: site, year
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

#*******************************************************************************
#### This code performs model selection for each vital rate for subsequent use in IPMs
############ Vital rates include survival, growth, flowering, and fruit count
############ Fixed effect: size; Random effects: region, year; ideally site would also be a random effect
#*******************************************************************************

# remove objects and clear workspace
rm(list = ls(all=TRUE))

# Install GLMMADMB package following instructions here: http://glmmadmb.r-forge.r-project.org/
# install.packages("R2admb")
# install.packages("glmmADMB", repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),type="source")

#require packages
require(lme4)
require(glmmADMB)
require(MuMIn)
require(MASS)
require(pscl)
require(plyr)
require(dplyr)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*******************************************************************************
#### 1. run data_prep.R script to clean up data ###
#*******************************************************************************

source("R_scripts/data_prep.R")

# Variables are: 

# Site: population
# ID: unique identifier for each individual
# Region: latitudinal region that population is nested within
# Latitude: latitude of population
# Longitude: longitude of population
# Elevation: elevation of population
# Class: stage class (juvenile, adult, or NA) of plant at time = t (PY)
# Fec1: Total number of fruits per individual   
# logSize: total stem length of the individual
# ClassNext: stage class (juvenile, adult, dead, or NA) of plant at time = t+1 (CY)
# logSizeNext: same as "logSize" above, for t+1
# Surv: survival (1) or not (0) of individuals between time = t (PY) and time = t+1 (CY)
# Year: annual transition of the long-term data at time = t (2010-2013)
# Fec0: Probability of flowering (1 if Class=="A" for adult, 0 if Class=="J" for juvenile)
# RegionRank: ordinal rank of regions from south to north
# SeedCt: mean seed count, rounded to the nearest integer, for each site

# Create separate object for southern region
data=subset(data,Region=="S1"|Region=="S2")

#*******************************************************************************
#### 2. Survival ###
#*******************************************************************************

# fixed effects model w/ and w/out size
s1=glm(Surv~logSize,data=data,family=binomial)
s2=glm(Surv~1,data=data,family=binomial)
model.sel(s1,s2) # model w/ size is preferred

# A. random intercepts & random slopes for Site; random intercepts & constant slope for Year
s3=glmer(Surv~logSize+(logSize|Site)+(logSize|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# B. random intercepts & random slopes for Site; random intercepts & constant slope for Year
s4=glmer(Surv~logSize+(logSize|Site)+(1|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# C. random intercepts & constant slope for Site; random intercepts & random slopes for Year
s5=glmer(Surv~logSize+(1|Site)+(logSize|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# D. random intercepts & constant slope for Site; random intercepts & constant slope for Year
s6=glmer(Surv~logSize+(1|Site)+(1|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# Compare models
anova(s3, s4, s5, s6)

AICc(s3, s4, s5, s6) 

model.sel(s3, s4, s5, s6) 

# PREFERRED MODEL IS s5

# Save top survival model to .rda file
save(s5, file='R_output/temporal_demog_compensation_TableS7_FigS7/surv.reg_South.rda')   

#*******************************************************************************
#### 3. Growth ###
#*******************************************************************************

# fixed effects model w/ and w/out size
g1=glm(logSizeNext~logSize,data=data[!is.na(data$logSize),])
g2=glm(logSizeNext~1,data=data[!is.na(data$logSize),])
model.sel(g1,g2) # model w/ size is preferred

# A. random intercepts & random slopes for Site; random intercepts & constant slope for Year
g3=lmer(logSizeNext~logSize+(logSize|Site)+(logSize|Year),data=data,control=lmerControl(optimizer = "bobyqa")) 

# B. random intercepts & random slopes for Site; random intercepts & constant slope for Year
g4=lmer(logSizeNext~logSize+(logSize|Site)+(1|Year),data=data,control=lmerControl(optimizer = "bobyqa")) 

# C. random intercepts & constant slope for Site; random intercepts & random slopes for Year
g5=lmer(logSizeNext~logSize+(1|Site)+(logSize|Year),data=data,control=lmerControl(optimizer = "bobyqa")) 

# D. random intercepts & constant slope for Site; random intercepts & constant slope for Year
g6=lmer(logSizeNext~logSize+(1|Site)+(1|Year),data=data,control=lmerControl(optimizer = "bobyqa")) 

# Compare models
anova(g3, g4,g5,g6)

AICc(g3, g4,g5,g6) 

model.sel(g3, g4,g5,g6) 

# PREFERRED MODEL IS g4

# Save top growth model to .rda file
save(g4, file='R_output/temporal_demog_compensation_TableS7_FigS7/growth.reg_South.rda')   

#*******************************************************************************
#### 4. Flowering ###
#*******************************************************************************

# fixed effects model w/ and w/out size
fl1=glm(Fec0~logSize,data=data,family=binomial)
fl2=glm(Fec0~1,data=data,family=binomial)
model.sel(fl1,fl2) # model w/ size is preferred

# A. random intercepts & random slopes for Site; random intercepts & constant slope for Year
fl3=glmer(Fec0~logSize+(logSize|Site)+(logSize|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# B. random intercepts & random slopes for Site; random intercepts & constant slope for Year
fl4=glmer(Fec0~logSize+(logSize|Site)+(1|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# C. random intercepts & constant slope for Site; random intercepts & random slopes for Year
fl5=glmer(Fec0~logSize+(1|Site)+(logSize|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# D. random intercepts & constant slope for Site; random intercepts & constant slope for Year
fl6=glmer(Fec0~logSize+(1|Site)+(1|Year),data=data,family=binomial,control=glmerControl(optimizer = "bobyqa")) 

# Compare models
anova(fl3, fl4, fl5,fl6)

AICc(fl3, fl4, fl5,fl6) 

model.sel(fl3, fl4,fl5,fl6) 

# PREFERRED MODEL IS fl5

# Save top flowering model to .rda file
save(fl5, file='R_output/temporal_demog_compensation_TableS7_FigS7/flowering.reg_South.rda')   

#*******************************************************************************
#### 5. Fruit number ###
#*******************************************************************************

#*******************************************************************************
####5A. Fit fixed effects models (GLMs) only for initial exploratory model selection of variance structure (poisson vs. negative binomial vs. 0- inflation)
#*******************************************************************************

fr1=glm(Fec1~logSize,data=data,na.action=na.omit,family=poisson) # poisson without 0-inflation 
fr2=glm.nb(Fec1~logSize,data=data,na.action=na.omit) # negative binomial without 0-inflation
fr3=zeroinfl(Fec1~logSize,data=data,na.action=na.omit,dist="poisson") # poisson with 0-inflation
fr4=zeroinfl(Fec1~logSize,data=data,na.action=na.omit,dist="negbin") # negative binomial with 0-inflation
model.sel(fr1,fr2,fr3,fr4) 

# PREFERRED MODEL IS fr2 (negative binomial w/out 0-inflation)
# fixed effects model w/ and w/out size
fr5=glm.nb(Fec1~1,data=data,na.action=na.omit)
model.sel(fr2,fr5) # model w/ size is preferred

#*******************************************************************************
####5B. Model selection of random effects structure
####### NOTE: when I ran same models with glmmadmb() function and family="poisson" none of the models except for fr4 converged
####### NOTE: I also ran same models with glmer.nb; computationally faster but less widely used in literature
#*******************************************************************************

# A. random intercepts & random slopes for Site; random intercepts & constant slope for Year
fr6=glmmadmb(Fec1~logSize+(logSize|Site)+(logSize|Year),data=data[!is.na(data$Fec1),],family="nbinom",link="log") 

# B. random intercepts & random slopes for Site; random intercepts & constant slope for Year
fr7=glmmadmb(Fec1~logSize+(logSize|Site)+(1|Year),data=data[!is.na(data$Fec1),],family="nbinom",link="log") 

# C. random intercepts & constant slope for Site; random intercepts & random slopes for Year
fr8=glmmadmb(Fec1~logSize+(1|Site)+(logSize|Year),data=data[!is.na(data$Fec1),],family="nbinom",link="log") 

# D. random intercepts & constant slope for Site; random intercepts & constant slope for Year
fr9=glmmadmb(Fec1~logSize+(1|Site)+(1|Year),data=data[!is.na(data$Fec1),],family="nbinom",link="log")

# Compare models	
AICc(fr6,fr7,fr8,fr9) 

model.sel(fr6,fr7,fr8,fr9)

# PREFERRED MODEL IS fr7
		
# Save top fruit # model to .rda file because it takes a long time to run
save(fr7, file='R_output/temporal_demog_compensation_TableS7_FigS7/fruit.reg_South.rda')   
		
