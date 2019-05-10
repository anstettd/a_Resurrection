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

pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Flower_num","SLA","Water_Content")]), scale=T)
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
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant
Anova(no3way.exp, type = 3)
visreg(fullmod.exp, xvar="Year", by="Site.Lat")


##### Flower_num #### Not currently working: Error in length(value <- as.numeric(value)) == 1L : 
#Downdated VtV is not positive definite. 
#This problem might be due to having all 0's or 1's in one subcatergory accoring to the internet.
fullmod.num <- glmer(Flower_num ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))

# drop 3way
no3way.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
lrtest(fullmod.num, no3way.num) #

# drop 2ways
noDxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
lrtest(no3way.num,noDxY.num) # Drought x Year removal does not lead to a better model
noSxY.num <- glmer(Flower_num ~ Site.Lat*Drought + Drought*Year + (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
lrtest(no3way.num,noSxY.num) # Site x Year removal does not lead to a better model
noSxD.num <- glmer(Flower_num ~ Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3, family=poisson(link = "log"))
lrtest(no3way.num,noSxD.num) # Site x Drought should not be removed
Anova(no3way.num, type = 3) #all 2-way interactions have some support
visreg(no3way.num, xvar="Year", by="Site.Lat") 


##### log(SLA) ####
fullmod.SLA <- lmer(log(SLA) ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.SLA <- lmer(log(SLA) ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.SLA, no3way.SLA) #model without 3-way intraction substantially better
Anova(fullmod.SLA , type = 3) # but 3-way interaction p=0.02
visreg(fullmod.SLA, xvar="Year", by="Site.Lat") # 


##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)

# drop 3way
no3way.wc <- lmer(Water_Content ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y3)
lrtest(fullmod.wc, no3way.wc) # model without 3-way interaction substantially better
Anova(fullmod.wc) # no support for 3-way but looks like some 2-ways might be supported

# carry forward with model simplification?


##### Structure #### 
fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message

fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Family), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message

fullmod.str <- glmer(Structure ~ Site.Lat*Year*Drought + (1|Block), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3) # error message

fullmod.str <- glm(Structure ~ Site.Lat*Year*Drought, family=binomial, data=y3) #runs

# drop 3way
no3way.str <- glmer(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), family=binomial, data=y3) # error message

no3way.str <- glm(Structure ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year, family=binomial, data=y3) # runs

lrtest(fullmod.str, no3way.str) # model with 2-way interaction has slightly higher likelihood but they are not that different
Anova(fullmod.str)

# carry forward with model simplification...


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

lrtest(fullmod.wil, no2way.wil) # model with 2-way interaction has slightly higher likelihood but they are not that different
Anova(fullmod.wil , type = 3) # Nothing significant, not suprising considering how few plants were non-wilted during assessment.




  
  