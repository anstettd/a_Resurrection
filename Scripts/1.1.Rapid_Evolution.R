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
y2[,19]<-y2[,18]/y2[,17] #better habit to do this by col names instead of col #s so that if you ever reorder anything the calculations won't be screwed up
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


################ Mixed Models using site, year & drought ####################
# prep factors
y3$Block <- as.factor(y3$Block)
y3$Family <- as.factor(y3$Family)
#y3$Year <- as.factor(y3$Year)

#####Experiment Date (flowering time since experiment start date)
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y3)
summary(fullmod.exp)

# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y3)
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
Anova(fullmod.exp, type = 3) # Drought site interaction, Site year interaction 
#plotting floweirng date vs time per Drought

visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)

#Code for D and W separate
visreg_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_D

visreg_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_flower_W

#For together see end of script
#visreg(fullmod.exp, xvar="Drought", by="Site.Lat") #Some sites have plastic changes, other do not.


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
no.year.SLA <- lmer(log(SLA) ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.SLA, no.year.SLA) # Year removed.
no.site.SLA <- lmer(log(SLA) ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(no.year.SLA, no.site.SLA) # Retain just drought

Anova(no.site.SLA, type = 3) # Year is not significant. Site and drought effect.
visreg(no.site.SLA, xvar="Drought") # Unclear why its only showing some years.



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
lrtest(nox.wc, noDrought.wc) # no interactions model significantly better. Retain drought in model.
noYear.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Family) + (1|Block), data=y3)
lrtest(nox.wc, noYear.wc) # no year model significantly supported
Drought.wc <- lmer(Water_Content ~ Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc, Drought.wc) # no year model significantly supported over Drought.wc, Retain site and drought.
siteXdrought.wc <- lmer(Water_Content ~ Site.Lat*Drought + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc,siteXdrought.wc) # Main effect model with site and drought is best.
year.rad.wc <- lmer(Water_Content ~ Site.Lat + Drought + (1|Year) + (1|Family) + (1|Block), data=y3)
lrtest(noYear.wc,year.rad.wc) # Main effect model with site and drought is best.



Anova(noYear.wc, type = 3) # Site and drought main effect.
#visreg(noYear.wc, xvar="Drought")
#visreg(noYear.wc, xvar="Site.Lat")
#visreg(noYear.wc, xvar="Site.Lat", by="Drought")

#plotting water content vs time per Drought
visreg_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_D

visreg_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"),jitter=TRUE, gg=TRUE)+
  facet_wrap(.~Site.Lat)+
  theme(panel.background=element_rect(fill="white"), strip.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="grey90"),
        panel.grid.minor=element_line(colour="grey90"), 
        axis.text.x=element_text(angle=45,hjust=1))
visreg_wc_W



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



####### Data Import Climate and Anomaly #########  
### Add in climate and weather covariates
wna <- read_csv("Climate/timeseries_lat_Normal_1981_2010Y.csv") %>% 
  select(Site=ID, MAT.clim=MAT,MAP.clim=MAP,CMD.clim=CMD)
wna$Site <- as.factor(wna$Site)

# Weather for the years 2010-2016; use these to calculate anomalies
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv")
wna2 <- wna1 %>% 
  select(ID_Year1,Latitude,Longitude,Elevation,MAT.weath=MAT,MAP.weath=MAP,CMD.weath=CMD) %>% 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_")
wna2$Site <- as.factor(wna2$Site)
wna2$Year <- as.numeric(wna2$Year)

# join climate and weather 
wna_all <- left_join(wna2, wna, by="Site") %>% 
  mutate(CMD.anom = CMD.clim-CMD.weath,
         MAT.anom = MAT.clim-MAT.weath,
         MAP.anom = log(MAP.clim)-log(MAP.weath),
         CMD.clim.scaled = as.vector(scale(CMD.clim)),
         MAT.clim.scaled = as.vector(scale(MAT.clim)),
         MAP.clim.scaled = as.vector(scale(MAP.clim)),
         CMD.weath.scaled = as.vector(scale(CMD.weath)),
         MAT.weath.scaled = as.vector(scale(MAT.weath)),
         MAP.weath.scaled = as.vector(scale(MAP.weath)),
         CMD.anom.scaled = as.vector(scale(CMD.anom)),
         MAT.anom.scaled = as.vector(scale(MAT.anom)),
         MAP.anom.scaled = as.vector(scale(MAP.anom)),)

# join all data into one frame
y4 <- left_join(y3, wna_all, by=c("Site"="Site", "Year"="Year"))

#Scale 
y4 <- y4 %>% mutate(Experiment_Date.scaled = scale(Experiment_Date),
                    SLA.scaled = scale(SLA),
                    Water_Content.scaled = scale(Water_Content),
                    Structure.scaled = scale (Structure),
                    Wilted.scaled = scale(Wilted))


#################### Slopes ####################
#Reponse variables scaled to allow for easier comparisons across variables.

slopes.rapid<-distinct(y3, Site, Site.Lat)
#Flowering_Dry
fullmod.exp <- lmer(Experiment_Date.scaled ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y4)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_flower_D<-vis_flower_D$fit
flower_dry_pop<-unique(fit_flower_D$Site.Lat)
for (i in 1:12){
  fit_flower_D_tmp<-fit_flower_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_flower_D<-lm(visregFit~Year, data=fit_flower_D_tmp)
  summary_flower_D<-summary(lm_flower_D)
  slopes.rapid[i,3]<-summary_flower_D$coefficients[2,1]
}
colnames(slopes.rapid)[3]<-"Flowering_Dry"
#Flowering_Wet
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_flower_W<-vis_flower_W$fit
for (i in 1:12){
  fit_flower_W_tmp<-fit_flower_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_flower_W<-lm(visregFit~Year, data=fit_flower_W_tmp)
  summary_flower_W<-summary(lm_flower_W)
  slopes.rapid[i,4]<-summary_flower_W$coefficients[2,1]
}
colnames(slopes.rapid)[4]<-"Flowering_Wet"

#WaterContent Dry
fullmod.wc <- lmer(Water_Content.scaled ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y4)
vis_wc_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D"))
fit_wc_D<-vis_wc_D$fit
for (i in 1:12){
  fit_wc_D_tmp<-fit_wc_D %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_D<-lm(visregFit~Year, data=fit_wc_D_tmp)
  summary_wc_D<-summary(lm_wc_D)
  slopes.rapid[i,5]<-summary_wc_D$coefficients[2,1]
}
colnames(slopes.rapid)[5]<-"Water_Content_Dry"

#Flowering_Wet
vis_wc_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W"))
fit_wc_W<-vis_wc_W$fit
for (i in 1:12){
  fit_wc_W_tmp<-fit_wc_W %>% filter(Site.Lat==flower_dry_pop[i])
  lm_wc_W<-lm(visregFit~Year, data=fit_wc_W_tmp)
  summary_wc_W<-summary(lm_wc_W)
  slopes.rapid[i,6]<-summary_wc_W$coefficients[2,1]
}
colnames(slopes.rapid)[6]<-"Water_Content_Wet"

#Get CMD climate into slope data frame
slopes.rapid.clim<-left_join(slopes.rapid, wna,by=c("Site"="Site"))
#Sum anomaly
site_vec<- unique(wna_all$Site)
for (i in 1:12){
  wna.temp<-wna_all%>% filter(Site==site_vec[i])
  sum.temp<-sum(wna.temp$CMD.anom)
  slopes.rapid.clim[i,10]<-sum.temp
}
colnames(slopes.rapid.clim)[10]<-"Cumulative_Anomaly"

#slope calc for one variable.
#fitS02_flower_D<-fit_flower_D %>% filter(Site.Lat=="32.9_S02") 
#lmS02_flower_D<-lm(visregFit~Year, data=fitS02_flower_D)
#summaryS02_flower_D<-summary(lmS02_flower_D)
#summaryS02_flower_D$coefficients[2,1]
#slopes.rapid[1,3]<-summaryS02_flower_D$coefficients[2,1]



####### Slope versus CMD.clim & Cumulative Anomaly plots ########
#All plots do not show much of a pattern.

attach(slopes.rapid.clim)
#Slope flowering dry
lm.flowering.clim<-lm(Flowering_Dry~CMD.clim)
summary(lm.flowering.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)

lm.flowering.anom<-lm(Flowering_Dry~Cumulative_Anomaly)
summary(lm.flowering.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)


#Slope flowering wet
lm.flowering.clim<-lm(Flowering_Wet~CMD.clim)
summary(lm.flowering.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)

lm.flowering.anom<-lm(Flowering_Wet~Cumulative_Anomaly)
summary(lm.flowering.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)



########
#Slope Water_Content dry
lm.Water_Content.clim<-lm(Water_Content_Dry~CMD.clim)
summary(lm.Water_Content.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Water_Content_Dry))+
  geom_point()+
  geom_smooth(method=lm)

lm.Water_Content.anom<-lm(Water_Content_Dry~Cumulative_Anomaly)
summary(lm.Water_Content.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly,Water_Content_Dry))+
  geom_point()+
  geom_smooth(method=lm)


#Slope Water_Content wet
lm.Water_Content.clim<-lm(Water_Content_Wet~CMD.clim)
summary(lm.Water_Content.clim)
ggplot(slopes.rapid.clim, aes(CMD.clim,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)

lm.Water_Content.anom<-lm(Water_Content_Wet~Cumulative_Anomaly)
summary(lm.Water_Content.anom)
ggplot(slopes.rapid.clim, aes(Cumulative_Anomaly,Water_Content_Wet))+
  geom_point()+
  geom_smooth(method=lm)

#Ft vs wc slope Wet
lm.slope_wet<-lm(Water_Content_Wet~Flowering_Wet)
summary(lm.slope_wet)
slope_wet<-ggplot(slopes.rapid.clim, aes(Water_Content_Wet,Flowering_Wet))+
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()
slope_wet + theme(legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Slope Water Content") +
  scale_y_continuous(name="Slope Date of Flowering") 

#Ft vs wc slope Dry
lm.slope_dry<-lm(Water_Content_Dry~Flowering_Dry)
summary(lm.slope_dry)
slope_dry<-ggplot(slopes.rapid.clim, aes(Water_Content_Dry,Flowering_Dry))+
  geom_point()+
  geom_smooth(method=lm)
slope_dry + theme(legend.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
      axis.text.y = element_text(size=12,face="bold"),
      axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
      axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))



################ Mixed Models using CMD.clim & Anomaly ####################


############## CMD & Anomaly ######################

#####Experiment Date
fullmod.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
summary(fullmod.cmd.exp)

# drop 3way
no3way.cmd.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.exp, no3way.cmd.exp) #no3way slightly lower likelihood but not by much. could select 3way b/c highest likelihood, or use parsimony to simplify whenever there's not support FOR retaining higher-order terms.
Anova(no3way.cmd.exp) #interactions involving clim not significant. Reducing complexity will likely lead to the best model.

# drop 2ways singly
noclimXd.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXd.exp) #same likelihood. can drop clim x drought.

noanomXD.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.exp, noanomXD.exp) #significant support for keeping anom x drought

noclimXanom.exp <- lmer(Experiment_Date ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.exp, noclimXanom.exp) #same likelihood. can drop clim x anom.

# test main effect of climate with background of anom x drought
anomxDclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + CMD.clim + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
anomxDnoclim.exp <- lmer(Experiment_Date ~ CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(anomxDclim.exp, anomxDnoclim.exp) # drop main effect of climate

# best model: anomaly x drought (anomxDnoclim.exp)
visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T)
anom.fl.graph<-visreg(anomxDnoclim.exp, xvar="CMD.anom.scaled", by="Drought", overlay=T, gg=TRUE)+
  theme_classic()
anom.fl.graph + 
  scale_x_continuous(name="CMD Anomaly") +
  scale_y_continuous(name="Date of Flowering") +
  theme(axis.text.x = element_text(color="black", size=14, face="bold", angle = 0, hjust=0.5, vjust = -1,),
      axis.text.y = element_text(color="black", size=14,face="bold"),
      axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))
# sites with the greatest CMD anomalies have less plasticity in response to drought and delay flowering less under wet conditions


##### % Water Content
fullmod.cmd.wc <- lmer(Water_Content ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
#summary(fullmod.exp)

# drop 3way
no3way.cmd.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(fullmod.cmd.wc, no3way.cmd.wc) # drop 3-way interaction (simpler model has significantly higher likelihood)

# drop 2ways singly
noclimXd.wc <- lmer(Water_Content ~ CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.wc, noclimXd.wc) # drop clim x drought

noanomXD.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.clim.scaled*CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(no3way.cmd.wc, noanomXD.wc) # drop anom x drought

noclimXanom.wc <- lmer(Water_Content ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(no3way.cmd.wc, noclimXanom.wc) # drop clim x anom

# main effects models
mains.wc <- lmer(Water_Content ~ CMD.clim.scaled + CMD.anom.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)

noD.wc <- lmer(Water_Content ~ CMD.clim.scaled + CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(mains.wc, noD.wc) # drop drought

noclim.wc <- lmer(Water_Content ~ CMD.anom.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
lrtest(mains.wc, noclim.wc) # drop climate 

noanom.wc <- lmer(Water_Content ~ CMD.clim.scaled + Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(mains.wc, noanom.wc) # drop anomaly

# test main effects alone
intercept.wc <- lmer(Water_Content ~ (1|Site/Family) + (1|Block) + (1|Year), data=y4)

drought.wc <- lmer(Water_Content ~ Drought + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, drought.wc) #drought not supported in background of other main effects, but it is favored to add drought by itself

clim.wc <- lmer(Water_Content ~ CMD.clim.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, clim.wc) #climate is worse than nothing

anom.wc <- lmer(Water_Content ~ CMD.anom.scaled + (1|Site/Family) + (1|Block) + (1|Year), data=y4)
lrtest(intercept.wc, anom.wc) #anomaly is worse than nothing

# best model: main effect of drought (drought.wc)
visreg(drought.wc)
# higher water content in wet treatment


##### % Above Ground Biomass
fullmod.cmd.bio <- lmer(Biomass ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
#summary(fullmod.exp)

# drop 3way
no3way.cmd.bio <- lmer(Biomass ~ CMD.clim.scaled*Drought + CMD.anom.scaled*Drought + CMD.clim.scaled*CMD.anom + (1|Site/Family) + (1|Block) + (1|Year), control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y4)
# warning about rank-deficiency. seems not to be a big deal https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
lrtest(fullmod.cmd.bio, no3way.cmd.bio) #retain 3-way

visreg(fullmod.cmd.bio, xvar="CMD.anom.scaled", by="CMD.clim.scaled", cond=list(Drought="D"))
visreg(fullmod.cmd.bio, xvar="CMD.anom.scaled", by="CMD.clim.scaled", cond=list(Drought="W"))
# in wet treatment, historically dry sites are more responsive to cmd anomalies than historically wet sites (greater biomass with wet anomaly, lower biomass with dry anomaly)
# in dry treatment, all sites insensitive to cmd anomalies 


####### Graphs for Site x Year X Drought

Res_flower_D<-vis_flower_D$res
Res_flower_W<-vis_flower_W$res

Res_Flower_all<-rbind(Res_flower_D, Res_flower_W)

Site_Labs<-c("32.9_S02"="Sweetwater", "34.1_S11"="Mill Creek", "34.3_S07"="WF Mojave", "36.2_S10"="NFMF Tule",
             "36.7_S08"="Red_Woods", "37.5_S32" = "Wawona", "39.4_S29"="Oregon Creek", "39.7_S18"="Little Jamison",
             "41.7_S17"="Deep Creek", "41.8_S16"="O'Neil Creek", "42.3_S36"="Deer Creek", "43.4_S15"="Rock Creek")
#Date of Flowering
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(x=Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.7)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_minimal()
Res_flower_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))

# Water Content
Res_wc_D<-vis_wc_D$res
Res_wc_W<-vis_wc_W$res
Res_wc_all<-rbind(Res_wc_D, Res_wc_W)
Res_wc_all_plot<-ggplot(Res_wc_all, aes(x=Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.7)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Year) +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF3300", "W"="#0099FF"))+
  theme_minimal()
Res_wc_all_plot + theme(legend.text = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                            axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))

