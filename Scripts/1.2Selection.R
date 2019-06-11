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

# prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
#y3$Year <- as.factor(y3$Year)

###### Calculate relative finess 
mean_flower_num <- mean(Flower_num, na.rm=T)
y5 <- y3 %>% mutate(relative_fitness = Flower_num/mean_flower_num) 

y5 <- y5 %>% mutate(Experiment_Date.scaled = scale(Experiment_Date),
                    SLA.scaled = scale(SLA),
                    Water_Content.scaled = scale(Water_Content),
                    Structure.scaled = scale (Structure),
                    Wilted.scaled = scale(Wilted))

#Selection Differentials
#Date of Flowering
diff.exp <- lmer(relative_fitness ~ Experiment_Date + I(Experiment_Date^2) + (1|Block), data=y5)
diff.lin.exp <- lmer(relative_fitness ~ Experiment_Date + (1|Block), data=y5)
lrtest(diff.exp, diff.lin.exp) #Retain Squared coeff
Anova(diff.exp)
visreg(diff.lin.exp, "Experiment_Date") #Selection for earlier flowering time.
# Unsure why the line looks linear, unsure why error is not shown

# % Water Content
diff.wc <- lmer(relative_fitness ~ Water_Content + I(Water_Content^2) + (1|Block), data=y5)
diff.lin.wc <- lmer(relative_fitness ~ Water_Content + (1|Block), data=y5)
lrtest(diff.wc, diff.lin.wc) #Retain Squared coeff
Anova(diff.wc)
visreg(diff.lin.wc)

# SLA
diff.sla <- lmer(relative_fitness ~ SLA + I(SLA^2) + (1|Block), data=y5)
diff.lin.sla <- lmer(relative_fitness ~ SLA + (1|Block), data=y5)
lrtest(diff.sla, diff.lin.sla) #Retain Squared coeff
Anova(diff.sla)
visreg(diff.lin.sla)

#Structure
diff.str <- glmer(relative_fitness ~ Structure + (1|Block), family=binomial, data=y3) # will not run
diff.str <- glm(relative_fitness ~ Structure, family=binomial, data=y3) # will not run


#Selection Gradients
#Flowering date & Water Content
grad.quad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + I(Experiment_Date.scaled^2) + Water_Content.scaled + 
                  Water_Content.scaled^2 + (1|Block), data=y5)
grad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + Water_Content.scaled + (1|Block), data=y5)
lrtest(grad.quad.lmer, grad.lmer) #Remove squared coeff
Anova(ns.lmer)
visreg(grad.quad.lmer, xvar= "Experiment_Date.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.quad.lmer, xvar="Water_Content.scaled", gg=TRUE) +
  theme_classic()

#Flowering date & SLA
gSLA.quad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + I(Experiment_Date.scaled^2) + SLA.scaled + 
                         SLA.scaled^2 + (1|Block), data=y5)
gSLA.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + SLA.scaled + (1|Block), data=y5)
lrtest(gSLA.quad.lmer, gSLA.lmer) #Remove squared coeff
Anova(glmer)
visreg(gSLA.quad.lmer, xvar= "Experiment_Date.scaled", gg=TRUE) +
  theme_classic() 
visreg(gSLA.quad.lmer, xvar="SLA.scaled", gg=TRUE) +
  theme_classic()



