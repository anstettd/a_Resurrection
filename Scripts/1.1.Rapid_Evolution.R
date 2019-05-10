#################
# M. carldinalis Rapid Evolution
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
attach(y2)


#Visually test for normality of data

#Experiment Date (Flowering Time)
qqnorm(Experiment_Date) #Aprox normal
ggplot(data=y2,aes(x=Experiment_Date))+
  geom_histogram()+theme_classic()

#Flower Number
qqnorm(Flower_num) # trucated left tail
ggplot(data=y2,aes(x=Flower_num))+
  geom_histogram()+theme_classic()

#SLA
qqnorm(SLA) #Not normal
ggplot(data=y2,aes(x=SLA))+
  geom_histogram()+theme_classic()

#log SLA
qqnorm(log(SLA)) #Use log
ggplot(data=y2,aes(x=log(SLA)))+
  geom_histogram()+theme_classic()

#Water Content
qqnorm(Water_Content) #Aprox normal
ggplot(data=y2,aes(x=Water_Content))+
  geom_histogram()+theme_classic()


################ Mixed Models ####################
# prep factors
y2$Block <- as.factor(y1$Block)
y2$Family <- as.factor(y1$Family)
#y2$Year <- as.factor(y1$Year)

# Site, year, treatment

#####Experiment Date (flowering time since experiment start date)
fullmod.exp <- lmer(Experiment_Date ~ Site*Year*Drought + (1|Family) + (1|Block), data=y2)
summary(fullmod.exp)

# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site*Drought + Drought*Year + Site*Year+ (1|Family) + (1|Block), data=y2)
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant
Anova(fullmod.cmd, type = 3)
visreg(fullmod.exp, xvar="Year", by="Site")

##### Flower_num #### Not currently working: Error in length(value <- as.numeric(value)) == 1L : 
#Downdated VtV is not positive definite. 
#This problem might be due to having all 0's or 1's in one subcatergory accoring to the internet.
fullmod.num <- glmer(Flower_num ~ Site*Year*Drought + (1|Family) + (1|Block), 
                     data=y2, family=poisson(link = "log"))

# drop 3way
no3way.num <- glmer(Flower_num ~ Site*Drought + Drought*Year + Site*Year + (1|Family) + (1|Block), data=y2, family=poisson(link = "log"))
lrtest(fullmod.num, no3way.num) #3-way intraction not significantly better

# drop 2ways
noDxY.num <- glmer(Flower_num ~ Site*Drought + Site*Year+ (1|Family) + (1|Block), data=y2, 
                   family=poisson(link = "log"))
lrtest(no3way.num,noDxY.num) # Drought x Year removal does not lead to a better model
noSxY.num <- glmer(Flower_num ~ Site*Drought + Drought*Year + (1|Family) + (1|Block), data=y2,
                   family=poisson(link = "log"))
lrtest(no3way.num,noSxY.num) # Site x Year removal does not lead to a better model
noSxD.num <- glmer(Flower_num ~ Drought*Year + Site*Year+ (1|Family) + (1|Block), data=y2,
                   family=poisson(link = "log"))
lrtest(no3way.num,noSxD.num) # Site x Drought can be removed, model significant
Anova(noSxD.num, type = 3) #Year X Site effect significant, if we trust the p-value
visreg(noSxD.num, xvar="Site", by="Year", overlay=TRUE) # Unclear why not all years are plotted



##### log(SLA) ####
fullmod.SLA <- lmer(log(SLA) ~ Site*Year*Drought + (1|Family) + (1|Block), data=y2)

# drop 3way
no3way.SLA <- lmer(log(SLA) ~ Site*Drought + Drought*Year + Site*Year + (1|Family) + (1|Block), data=y2)
lrtest(fullmod.SLA, no3way.SLA) #3-way intraction significantly better
Anova(fullmod.SLA , type = 3) # Significant three way interaction
visreg(fullmod.SLA, xvar="Year", by="Site") # Unclear why not all years are plotted


##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Site*Year*Drought + (1|Family) + (1|Block), data=y2)

# drop 3way
no3way.wc <- lmer(Water_Content ~ Site*Drought + Drought*Year + Site*Year + (1|Family) + (1|Block), data=y2)
lrtest(fullmod.wc, no3way.wc) # Significant three way interaction
Anova(fullmod.wc , type = 3) # Year marginally significant
visreg(fullmod.SLA, xvar="Year", by="Site") # Unclear why not all years are plotted




##### Structure #### 
fullmod.str <- lmer(Structure ~ Site*Year*Drought + (1|Family) + (1|Block), data=y2) 

# drop 3way
no3way.str <- lmer(Structure ~ Site*Drought + Drought*Year + Site*Year + (1|Family) + (1|Block), data=y2)
lrtest(fullmod.str, no3way.str) # Significant three way interaction
Anova(fullmod.str , type = 3) # Not significant, a harder trait to measure.

##### Wilted ####
fullmod.wil <- lmer(Wilted ~ Site*Year + (1|Family) + (1|Block), data=y2) 

# drop 2way
no2way.wil <- lmer(Wilted ~ Site + Year + (1|Family) + (1|Block), data=y2) 
lrtest(fullmod.wil, no2way.wil) # Significant 2-way interaction
Anova(fullmod.wil , type = 3) # Not significant, not suprizing consideirng how few plants were non-wilted druing assessment.




  
  