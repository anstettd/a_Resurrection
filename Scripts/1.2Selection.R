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

# Add in flowering time data
flower1<-read.csv("Data/flower_date_ver2.csv", header=T)
colnames(flower1)[1]<-"Order1"
colnames(flower1)[5]<-"Flowering_Date"
flower1[,6]<-flower1[,5]-101
colnames(flower1)[6]<-"Experiment_Date"
#y1<-left_join(Y,flower1,by=c("Order"="Order1"))
y1<-left_join(Y,flower1,by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment"))

# Add in other physical traits
rapid<-read.csv("Data/rapid.csv", header=T)
y2<-left_join(y1,rapid, by=c("Order"="Order2", "Family"="Family", "Block"="Block", "Drought"="Treatment"))

# Calculate SLA and & water content
y2[,19]<-y2[,18]/y2[,17]
y2[,20]<-y2[,17]/y2[,16]
colnames(y2)[19]<-"SLA"
colnames(y2)[20]<-"Water_Content"

# Make a categorical site variable that is ordered by latitude
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv") %>%
  select(ID_Year1,Latitude,Longitude) %>% #,Elevation,MAT,MAP,CMD 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_")
wna1$Site <- as.factor(wna1$Site)
wna1$Year <- as.numeric(wna1$Year)

y3 <- left_join(y2, wna1, by=c("Site", "Year"))

y3 <- y3 %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))  
attach(y3)

# Prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
y3$Structure <- as.factor(y3$Structure)
#y3$Year <- as.factor(y3$Year)

###### Calculate relative finess 
mean_flower_num <- mean(Flower_num, na.rm=T)
y5 <- y3 %>% mutate(relative_fitness = Flower_num/mean_flower_num) 

y5 <- y5 %>% mutate(Experiment_Date.scaled = scale(Experiment_Date),
                    SLA.scaled = scale(SLA),
                    Water_Content.scaled = scale(Water_Content))#,
                    #Structure.scaled = scale (Structure), #not for categorical variables
                    #Wilted.scaled = scale(Wilted))

### Selection Differentials
# Date of Flowering
diff.quad.exp <- lmer(relative_fitness ~ Experiment_Date.scaled + I(2*Experiment_Date.scaled^2) + (1|Block), data=y5)
Anova(diff.exp)
diff.lin.exp <- lmer(relative_fitness ~ Experiment_Date.scaled + (1|Block), data=y5)
lrtest(diff.quad.exp, diff.lin.exp) #drop squared coeff
visreg(diff.lin.exp, xvar="Experiment_Date.scaled", band=TRUE) 
# Selection for earlier flowering time.

# % Water Content
diff.quad.wc <- lmer(relative_fitness ~ Water_Content.scaled + I(2*Water_Content.scaled^2) + (1|Block), data=y5)
Anova(diff.wc)
diff.lin.wc <- lmer(relative_fitness ~ Water_Content.scaled + (1|Block), data=y5)
lrtest(diff.quad.wc, diff.lin.wc) # Retain wquared coeff
visreg(diff.quad.wc, xvar="Water_Content.scaled")
# Some stabilizing selection around intermediate water content, but mostly selection against low water content


# SLA
diff.quad.sla <- lmer(relative_fitness ~ SLA.scaled + I(2*SLA.scaled^2) + (1|Block), data=y5)
Anova(diff.sla)
diff.lin.sla <- lmer(relative_fitness ~ SLA.scaled + (1|Block), data=y5)
lrtest(diff.quad.sla, diff.lin.sla) # Retain squared coeff
visreg(diff.quad.sla, xvar="SLA.scaled")
# Selection against high SLA

# Structure
diff.str <- lmer(relative_fitness ~ Structure + (1|Block), data=y5) # the response variable is not binomial, this is just a categorical predictor with a normally distributed response, so no need for glm
Anova(diff.str)
visreg(diff.str, xvar="Structure")
# Selection for structure=1 (is this having rhizomes?) Daniel: yes 1 has multiyear sidebuds


### Selection Gradients
# All traits 
grad.all.quad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + I(2*Experiment_Date.scaled^2) + SLA.scaled + I(2*SLA.scaled^2) + Water_Content.scaled + I(2*Water_Content.scaled^2) + Structure + (1|Block), data=y5)
grad.all.lin.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled  + SLA.scaled + Water_Content.scaled + Structure + (1|Block), data=y5)
lrtest(grad.all.quad.lmer, grad.all.lin.lmer) #Remove squared coeff
Anova(grad.all.lin.lmer)
visreg(grad.all.lin.lmer, xvar= "Experiment_Date.scaled", gg=TRUE) +
  theme_classic() 
visreg(grad.all.lin.lmer, xvar="SLA.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.all.lin.lmer, xvar="Water_Content.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.all.lin.lmer, xvar="Structure", gg=TRUE) +
  theme_classic()

#Flowering date & Water Content & Structure
grad.wc.quad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + I(2*Experiment_Date.scaled^2) + Water_Content.scaled + I(2*Water_Content.scaled^2) + Structure + (1|Block), data=y5)
grad.wc.lin.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + Water_Content.scaled + Structure + (1|Block), data=y5)
lrtest(grad.wc.quad.lmer, grad.wc.lin.lmer) #Remove squared coeff
Anova(grad.wc.lin.lmer )
visreg(grad.wc.lin.lmer, xvar= "Experiment_Date.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.wc.lin.lmer, xvar="Water_Content.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.wc.lin.lmer, xvar="Structure", gg=TRUE) +
  theme_classic()

# Flowering date & SLA & Structure
grad.SLA.quad.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + I(2*Experiment_Date.scaled^2) + SLA.scaled + I(2*SLA.scaled^2) + Structure + (1|Block), data=y5)
grad.SLA.lin.lmer <- lmer(relative_fitness ~ Experiment_Date.scaled + SLA.scaled + Structure + (1|Block), data=y5)
lrtest(grad.SLA.quad.lmer, grad.SLA.lin.lmer) #Remove squared coeff
Anova(grad.SLA.lin.lmer)
visreg(grad.SLA.lin.lmer, xvar= "Experiment_Date.scaled", gg=TRUE) +
  theme_classic() 
visreg(grad.SLA.lin.lmer, xvar="SLA.scaled", gg=TRUE) +
  theme_classic()
visreg(grad.SLA.lin.lmer, xvar="Structure", gg=TRUE) +
  theme_classic()


### extract coefficients for differentials and gradients
diff.exp <- fixef(diff.quad.exp)
diff.wc <- fixef(diff.quad.wc)
diff.sla <- fixef(diff.quad.sla)
diff.str <- fixef(diff.str)
grads <- fixef(grad.all.quad.lmer) #you can see that these are much weaker due to trait correlations


