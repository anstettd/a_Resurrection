#################
# M. cardinalis Rapid Evolution
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


### Data prep
Y <- read.csv("Data/drought1.csv", header=T)

#Add in flowering time data
flower1<-read.csv("Data/flower_date_ver2.csv", header=T)
  colnames(flower1)[1]<-"Order1"
  colnames(flower1)[5]<-"Flowering_Date"
flower1[,6]<-flower1[,5]-101
  colnames(flower1)[6]<-"Experiment_Date"
#y1<-left_join(Y,flower1,by=c("Order"="Order1"))
y1<-left_join(Y,flower1,by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment"))

#Add in other physical traits
rapid<-read.csv("Data/rapid.csv", header=T)
y2<-left_join(y1,rapid, by=c("Order"="Order2", "Family"="Family", "Block"="Block", "Drought"="Treatment"))
#Calculate SLA and & water content
y2[,19]<-y2[,18]/y2[,17]
y2[,20]<-y2[,17]/y2[,16]
  colnames(y2)[19]<-"SLA"
  colnames(y2)[20]<-"Water_Content"

#Make a categorical site variable that is ordered by latitude
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv") %>%
  select(ID_Year1,Latitude,Longitude) %>% #,Elevation,MAT,MAP,CMD 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_")
wna1$Site <- as.factor(wna1$Site)
wna1$Year <- as.numeric(wna1$Year)

y3 <- left_join(y2, wna1, by=c("Site", "Year"))
  
y3 <- y3 %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))  
attach(y3)


#Visually test for normality of data

#Experiment Date (Flowering Time)
qqnorm(Experiment_Date) #Aprox normal
ggplot(data=y3,aes(x=Experiment_Date))+
  geom_histogram()+theme_classic()

#Flower Number
qqnorm(Flower_num) # trucated left tail
ggplot(data=y3,aes(x=Flower_num))+
  geom_histogram()+theme_classic()

#Biomass
qqnorm(Biomass) # Approx normal
ggplot(data=y3,aes(x=Biomass))+
  geom_histogram()+theme_classic()

#SLA
qqnorm(SLA) #Not normal
ggplot(data=y3,aes(x=SLA))+
  geom_histogram()+theme_classic()

#log SLA
qqnorm(log(SLA)) #Use log
ggplot(data=y3,aes(x=log(SLA)))+
  geom_histogram()+theme_classic()

#Water Content
qqnorm(Water_Content) #Aprox normal
ggplot(data=y3,aes(x=Water_Content))+
  geom_histogram()+theme_classic()


#Assess correlation among response variables
pairs(~ Experiment_Date + Flower_num + log(SLA) + Water_Content)

pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content", "Biomass")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (34%)")

# so, there are essentially 2 axes of variation that we are seeing with these 4 variables. flowering date and flower number are negatively correlated (makes sense) and sla and water content are also negatively correlated (structurally thicker leaves are also more succulent?)


################ Mixed Models ####################
# prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
#y3$Year <- as.factor(y3$Year)

# Site, year, treatment

#####Experiment Date (flowering time since experiment start date)
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
summary(fullmod.exp)

# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y2)
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
Anova(no3way.exp, type = 3) # Drought site interaction, Site year interaction 
visreg(fullmod.exp, xvar="Year", by="Site.Lat") #Clear and drastically different results across populations
visreg(fullmod.exp, xvar="Drought", by="Site.Lat") #Some sites have plastic changes, other do not.

##### Flower_num #### 
#glmer did not converge. Try glmmTMB

fullmod.num <- glmmTMB(Flower_num ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#Still getting model convergence problems. We may need to simplify the model. 
#Should I take out (1|Family) or (1|Block)?


# drop 3way
#no3way.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(fullmod.num, no3way.num) 
# drop 2ways
#noDxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noDxY.num) # Drought x Year removal does not lead to a better model
#noSxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noSxY.num) # Site x Year removal does not lead to a better model
#noSxD.num <- glmer(Flower_num ~ Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
#lrtest(no3way.num,noSxD.num) # Site x Drought should not be removed
#Anova(no3way.num, type = 3) #all 2-way interactions have some support
#visreg(no3way.num, xvar="Year", by="Site.Lat") 



#####Above Ground Biomass
fullmod.bio <- lmer(Biomass ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.bio <- lmer(Biomass ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y2)
lrtest(fullmod.bio, no3way.bio) #three way marginally significant, no3way.bio has larger LogLik.

# drop 2ways
noDxY.bio <- lmer(Biomass ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.bio,noDxY.bio) #noDxY.bio signifcnatly better, has larger LogLik
SxYD.bio<- lmer(Biomass ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.bio,SxYD.bio) #noDxY.bio significantly better than SxYD.bio, had greater LogLik
#Therefore, retain Biomass ~ Site.Lat*Drought + Site.Lat*Year
Anova(noDxY.bio, type = 3) #Site X Drought, Site X Year
visreg(noDxY.bio, xvar="Year", by="Site.Lat") #some variaiblity in biomass over time. 
#Unclear how biologically meainingful it is.
visreg(noDxY.bio, xvar="Year", by="Drought") #trend to less biomass over time, less bioamss in Drought


##### log(SLA) ####
fullmod.SLA <- lmer(log(SLA) ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.SLA, no3way.SLA) #model without 3-way intraction substantially better

# drop 2ways
noDxY.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.SLA,noDxY.SLA) #noDxY.bio signifcnatly better, has larger LogLik
SxYD.SLA<- lmer(log(SLA) ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.SLA,SxYD.SLA) #No significant difference, retain simplier model (SxYD.SLA) with greater loglik

#no interactions
nox.SLA <- lmer(log(SLA) ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(SxYD.SLA,nox.SLA) # no interactions model significantly better.
noDrought.SLA <- lmer(log(SLA) ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.SLA, noDrought.SLA) # no interactions model significantly better. Retain this model.
Anova(nox.SLA, type = 3) # Year is not significant. Site and drought effect.
visreg(nox.SLA, xvar="Drought") # Unclear why its only showing some years.
visreg(nox.SLA, xvar="Site.Lat")


##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.wc, no3way.wc) # two-way model supported
# drop 2ways
noDxY.wc <- lmer(Water_Content ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(no3way.wc,noDxY.wc) #noDxY.wc supported
SxYD.wc<- lmer(Water_Content ~ Site.Lat*Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(noDxY.wc,SxYD.wc) #SxYD.wc supported

#no interactions
nox.wc <- lmer(Water_Content ~ Site.Lat + Year + Drought + (1|Family) + (1|Block), data=y3)
lrtest(SxYD.wc,nox.wc) # no interactions model significantly better.
noDrought.wc <- lmer(Water_Content ~ Site.Lat + Year + (1|Family) + (1|Block), data=y3)
lrtest(nox.wc, noDrought.wc) # no interactions model significantly better. Retain this model.
noYear.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.wc, noYear.wc) # no year model significantly supported
Drought.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc, Drought.wc) # no year model significantly supported over Drought.wc
Anova(noYear.wc, type = 3) # Site and drought main effect.
visreg(noYear.wc, xvar="Drought")
visreg(noYear.wc, xvar="Site.Lat")
visreg(noYear.wc, xvar="Site.Lat", by="Drought")


##### Structure #### 
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message
fullmod.str <- glm(Structure ~ Site.Lat*Year*Drought, family=binomial, data=y3) #runs

# drop 3way
no3way.str <- glmer(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), family=binomial, data=y3) # error message
no3way.str <- glm(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year, family=binomial, data=y3) # runs
lrtest(fullmod.str, no3way.str) #Models not significantly different, take simpler model??

#Drop 2way
noDxY.str <- glm(Structure ~ Site.Lat*Drought + Site.Lat*Year, family=binomial, data=y3) 
lrtest(no3way.str, noDxY.str) # Models not significnatly different Take simpler model.
SxYD.str<- glm(Structure ~ Site.Lat*Year + Drought, family=binomial, data=y3)
lrtest(noDxY.str,SxYD.str) # noDxY signifcantly better. Retain this model.
Anova(noDxY.str, type = 3) # Site X drought main effect. VisReg not useful with binomial data.

##### Wilted ####
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glmer(Wilted ~ Site.Lat*Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # errors - model is too complex
fullmod.wil <- glm(Wilted ~ Site.Lat*Year, family=binomial, data=y3) # runs

# drop 2way
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glmer(Wilted ~ Site.Lat + Year + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error
no2way.wil <- glm(Wilted ~ Site.Lat + Year, family=binomial, data=y3) # runs
lrtest(fullmod.wil, no2way.wil) # models not significantly different, take simpler model.

noYear.wil <- glm(Wilted ~ Site.Lat, family=binomial, data=y3)
lrtest(no2way.wil, noYear.wil) #Take simpler model
Anova(noYear.wil , type = 3) # Nothing significant, not suprising considering how few plants were non-wilted during assessment.




  
  