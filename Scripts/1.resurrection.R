###########
#Pilot Analysis
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
library(ggplot2)

### Data prep
Y <- read.csv("Data/drought1.csv", header=T) #specify relative paths within the project folder instead of using setwd

### Add in flowering time data
flower1 <- read.csv("Data/flower_date_ver2.csv", header=T)
colnames(flower1)[1] <- "Order1"
colnames(flower1)[5] <- "Flowering_Date"
y1 <- left_join(Y, flower1, by=c("Order"="Order1", "Family"="Family", "Block"="Block", "Drought"="Treatment"))

### Add in climate and weather covariates
# Long-term climate for hypothesis about propensity to evolve or be plastic
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
dat <- left_join(y1, wna_all, by=c("Site"="Site", "Year"="Year"))


###### Basic Graphing #######

##Select All Wet##
yWet <- dat %>% 
  filter(Drought=="W") %>% 
  droplevels()
#Box and Whisker Plot
ggplot(yWet, aes(Year, Flowering_Date)) +
  geom_boxplot(aes(group=Year)) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Wet_Whisker.png", width = 5, height = 5)
#Scatter plot
ggplot(yWet, aes(Year, Flowering_Date)) +
  geom_jitter(width = 0.10,size=0.3) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Wet_jitter.png", width = 5, height = 5)


###CMD###
#ggsave("Summary_Graphs/.png", width = 5, height = 5)
attach(yWet)
lmyWCMD<-lm(Flowering_Date~CMD)
summary(lmyWCMD)
visreg(lmyWCMD)

###MAP###
attach(yWet)
lmyMAP<-lm(Flowering_Date~MAP)
summary(lmyMAP)
visreg(lmyMAP) #Earlier flowering time in sites with lower precipitation! P<0.0001
# Amy note: because the climate values are specific to site x year, you can't interpret this solely as a site 
#effect. in other words, a recent year at a dry site might have a similar MAP as an early year at a wet site. 
#So, more precisely, the result is that earlier flowering time under conditions of low precipitation, 
#whether those arose in different sites or years. 
#this is why you need a full model that accounts for site and year.


###MAT### This is a bit strange, will leave out for now.
attach(yWet)
lmyMAT<-lm(Flowering_Date~MAT)
summary(lmyMAT)
visreg(lmyMAT) #Earlier floweringtime in sites with lower temperature? P<0.0001




###Select All Dry##
yDry <- dat %>% 
  filter(Drought=="D") %>% 
  droplevels()
#Box and Whisker Plot
ggplot(yDry, aes(Year, Flowering_Date)) +
  geom_boxplot(aes(group=Year)) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Dry_Whisker.png", width = 5, height = 5)
#Scatter plot
ggplot(yDry, aes(Year, Flowering_Date)) +
  geom_jitter(width = 0.10,size=0.3) +
  facet_wrap(~Site, ncol = 3) +
  theme_grey()
ggsave("Summary_Graphs/Dry_jitter.png", width = 5, height = 5)

###CMD###
attach(yDry)
lmyDCMD<-lm(Flowering_Date~CMD)
summary(lmyDCMD)
visreg(lmyDCMD)

###MAP###
attach(yDry)
lmyMAP<-lm(Flowering_Date~MAP)
summary(lmyMAP)
visreg(lmyMAP)

###MAT###
attach(yDry)
lmyMAT<-lm(Flowering_Date~MAT)
summary(lmyMAT)
visreg(lmyMAT)




################ Mixed Models ####################

## Full Models
# prep factors
dat$Block <- as.factor(dat$Block)
dat$Family <- as.factor(dat$Family)
dat$Year <- as.numeric(dat$Year)

# load packages
library(lme4) #for mixed models
library(lmerTest) #for LRT
library(visreg) # one way to visualize marginal effects (better for datapoints)
library(ggeffects) # another way to visualize marginal effects (better for CIs)
library(MuMIn)

### Categorical models
# Before exploring putative climatic/weather drivers, see if there are site and year effects
catmod <- lmer(Flowering_Date ~ Site*Year*Drought + (1|Site/Family) + (1|Block), data=dat, control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(catmod) 
anova(catmod)
visreg(catmod, xvar="Year", by="Site", overlay=T, cond=list(Drought="D"))

catmod.2way <- lmer(Flowering_Date ~ Site*Year + Site*Drought + Year*Drought + (1|Site/Family) + (1|Block), data=dat, control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(catmod.2way) 
anova(catmod.2way)
visreg(catmod.2way, xvar="Year", by="Site", overlay=T, cond=list(Drought="D"))
visreg(catmod.2way, xvar="Year", by="Site", overlay=T, cond=list(Drought="W"))

model.sel(catmod, catmod.2way)

### CMD: climatic moisture deficit (higher = drier)

# historical CMD in place of site
climmod.cmd <- lmer(Flowering_Date ~ CMD.clim.scaled*Year*Drought + (1|Site/Family) + (1|Block), data=dat)
summary(climmod.cmd) 
anova(climmod.cmd)

# CMD anomaly in place of year
weathmod.cmd <- lmer(Flowering_Date ~ Site*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block), data=dat)
summary(weathmod.cmd) 
anova(weathmod.cmd)
visreg(weathmod.cmd, xvar="CMD.anom.scaled", by="Site", cond=list(Drought="D"))
visreg(weathmod.cmd, xvar="CMD.anom.scaled", by="Site", cond=list(Drought="W"))

# climate and anomaly
fullmod.cmd <- lmer(Flowering_Date ~ CMD.clim.scaled*CMD.anom.scaled*Drought + (1|Site/Family) + (1|Block), data=dat)
summary(fullmod.cmd) 
anova(fullmod.cmd)

model.sel(climmod.cmd, weathmod.cmd, fullmod.cmd)

# drop 3way
no3way.cmd <- lmer(Flowering_Date ~ CMD.clim*Drought + Drought*Year + CMD.clim*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.cmd)
lrtest(fullmod.cmd, no3way.cmd) #3-way interaction highly significant

####Poster Candidate####
visreg(fullmod.cmd, xvar="CMD", by="Year") 
#2013 seems confounded by sampling, otherwise the cline seems to flatten towards more recent years
#visreg takes other variables into account and only shows the effects you want.
#Daniel: Given our three-way interaction, this will be quite helpful.
#Daniel: So areas with a greater moisture deficite had later flowering time, but this has leveled off. 
#The leveling off makes sense, as more areas were hit by drought. 
#However, I would expect areas with a higher CMD would initially have lower (earlier) flowering time.
#Perhaps I am not properly understanding what CMD means. 
#Since the measure is a deficite I am expecting a higher value would be less moister, and hencing a plants needs to flower earlier

visreg(fullmod.cmd, xvar="CMD", by="Drought", overlay=T) #no apparent differences in plasticity based on CMD
visreg(fullmod.cmd, xvar="Drought", by="Year")
visreg(fullmod.cmd, by="Drought", xvar="Year") #evolution of earlier flowering seen in both wet and dry treatments

preds.cmd <- ggeffect(fullmod.cmd, terms = c("CMD", "Drought", "Year"))
plot(preds.cmd) # Daniel: The lines seem more or less parallel overall. Cross overs fall within CI.

##### MAT #####
fullmod.mat <- lmer(Flowering_Date ~ MAT*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod.mat)

# drop 3way
no3way.mat <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.mat)
lrtest(fullmod.mat, no3way.mat) #3-way interaction not significant

# drop 2ways
noTxD.mat <- lmer(Flowering_Date ~ Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noTxD.mat) #MAT x Drought not significant
noDxY.mat <- lmer(Flowering_Date ~ MAT*Drought + MAT*Year+ (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noDxY.mat) # Drought x Year significant
noTxY.mat <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + (1|Site/Family) + (1|Block), data=y1)
lrtest(no3way.mat,noTxY.mat) # MAT x Year not significant


visreg(noDxY.mat, xvar="Drought", by="Year", overlay=T) # AmyI don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.
# I screwed up before and was graphing a model without the effect of interest. Use this one:
DxY.mat <- lmer(Flowering_Date ~ Drought*Year + MAT + (1|Site/Family) + (1|Block), data=y1)
#Poster? (perhaps breaking up into different years...)
visreg(DxY.mat, xvar="Drought", by="Year", overlay=T) # AmyI don't see much of an interaction between drought and year in this graph (later realization: yeah, because the model didn't have one! fixed now), but I really like how it shows plasticity and evolution in one picture.

#Daniel: I agree it shows a lot, but the image is also a bit of an eye sore. Perhaps if we remove the dots, 
#it can make it into the poster?
#Daniel: Does it matter that we are controlling for MAT and not something based on precipitation?
#Amy: i don't favor this one for the poster






###### MAP #####
fullmod.map <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Site/Family) + (1|Block), data=y1)
summary(fullmod.map)

# drop 3way
no3way.map <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Site/Family) + (1|Block), data=y1)
summary(no3way.map)
lrtest(fullmod.map, no3way.map) #3-way interaction highly significant

visreg(fullmod.map, xvar="MAP", by="Year") #2013 has poor sampling, otherwise the cline seems to flatten or even reverse in more recent years
#Again we are getting this initial tren of earlier flowering time under more precipitation, which is perahps
#due to latitudinal gradients and growing season rather than drough avoidance? 
#Its is probably not important for the poster anyways.

####Poster Candidate#### Second thought its good, but there are better
visreg(fullmod.map, xvar="MAP", by="Drought", overlay=T) #cline more apparent in dry treatment than in wet. populations (site x years) sampled from wet conditions have stronger plastic response to drought treatment.
#####This makes a lot of sense. Another candidate for the poster.
visreg(fullmod.map, xvar="Drought", by="Year")
visreg(fullmod.map, by="Drought", xvar="Year") #2013 is super wonky

preds.map <- ggeffect(fullmod.map, terms = c("MAP", "Drought", "Year"))
plot(preds.map) 


############ trying all again with 2013 excluded ############

fullmod.cmd.no2013 <- lmer(Flowering_Date ~ CMD*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.cmd.no2013)
no3way.cmd.no2013 <- lmer(Flowering_Date ~ CMD*Drought + Drought*Year + CMD*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.cmd.no2013)
lrtest(fullmod.cmd.no2013, no3way.cmd.no2013) #3-way interaction still highly significant

visreg(fullmod.cmd.no2013, xvar="CMD", by="Year") #cline flattens towards more recent years
visreg(fullmod.cmd.no2013, xvar="CMD", by="Drought", overlay=T) #no apparent differences in plasticity based on CMD
visreg(fullmod.cmd.no2013, xvar="Drought", by="Year")
visreg(fullmod.cmd.no2013, xvar="Drought", by="Year", overlay=T)
visreg(fullmod.cmd.no2013, by="Drought", xvar="Year") #evolution of earlier flowering seen in both wet and dry treatments

###################Poster Candidate 1##########################

### Amy votes for this one
### Using base R to make the changes. Less than ideal.
visreg(fullmod.cmd.no2013, by="Drought", xvar="Year", overlay=T,xlab="", 
       ylab="Flowering Date (Julian Days)", cex.lab=1.5,par(cex.axis=1.3),
       line=list(), points=list(size=5, pch=1))

#The moment I insert the "gg=TRUE command, I lose all my base R modifications. 
#I also can't seem to use gg landuage to code chanes in point size and axes
visreg(fullmod.cmd.no2013, by="Drought", xvar="Year", overlay=T,xlab="", 
       ylab="Flowering Date (Julian Days)", cex.lab=1.7,par(cex.axis=1.3),
       line=list(), points=list(size=5, pch=1),gg=TRUE)+
  theme(legend.position = "top")+
  theme(legend.text=element_text(size=10))+
  scale_fill_discrete(name = "Treatments",labels = c("Dry", "Wet"))+
  theme_classic()

###################Poster Candidate 1##########################
      

#evolution of earlier flowering seen in both wet and dry treatments

preds.cmd.no2013 <- ggeffect(fullmod.cmd.no2013, terms = c("CMD", "Drought", "Year"))
plot(preds.cmd.no2013) 


fullmod.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.mat.no2013)
no3way.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.mat.no2013)
lrtest(fullmod.mat.no2013, no3way.mat.no2013) #3-way interaction not significant
noTxD.mat.no2013 <- lmer(Flowering_Date ~ Drought*Year + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noTxD.mat.no2013) #MAT x Drought not significant
noDxY.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + MAT*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noDxY.mat.no2013) # Drought x Year significant
noTxY.mat.no2013 <- lmer(Flowering_Date ~ MAT*Drought + Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
lrtest(no3way.mat.no2013,noTxY.mat.no2013) # MAT x Year not significant

DxY.mat.no2013 <- lmer(Flowering_Date ~ Drought*Year + MAT + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
visreg(DxY.mat.no2013, xvar="Drought", by="Year", overlay=T) # I don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.
####Poster Candidate####
### This is my screw-up; needed to use the DxY model, not the noDxY model
visreg(noDxY.mat.no2013, xvar="Year", by="Drought", overlay=T) # I don't see much of an interaction between drought and year in this graph, but I really like how it shows plasticity and evolution in one picture.
#Daniel: I Think this captures it all. Basically same image as above.


###MAP no 2013

fullmod.map.no2013 <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(fullmod.map.no2013)
no3way.map.no2013 <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Site/Family) + (1|Block), data=filter(y1, Year != "2013"))
summary(no3way.map.no2013)
lrtest(fullmod.map.no2013, no3way.map.no2013) #3-way interaction still highly significant

###################Poster Candidate 2##########################

#ggplot lets me easily get everything on one linem but I can't apply gg or base R to change axes or ponts!
visreg(fullmod.map.no2013, xvar="MAP", by="Year",gg=TRUE)

#Points change, but not the axes. I can't seem to make them larger
#Also note the graph looks terrible. To see it properly save it with Hight =3, width = 12 under export.
visreg(fullmod.map.no2013, xvar="MAP", by="Year",gg=TRUE,xlab="Mean Annual Precipitation", 
       ylab="Flowering Date (Julian Days)", cex.lab=1,par(cex.axis=10),
       line=list(), points=list(size=2, pch=1))
  

# I don't know about the gg option in visreg, but i've saved the visreg outputs as objections and them brought them manually into ggplot, where you have full gg functionality. here is an example you can modify:

# # use visreg to partial out random effects
# vis <- visreg(mod, xvar="year_end_scaled", points="la_sa_std")
# visres <- data.frame(vis$res)
# for_mod_graph <- cbind(dat, visres)
# 
# ggplot(vis$fit, aes(x=year_end, y=visregFit)) +
#   geom_point(data=for_mod_graph, aes(x=year_end, y=visregRes), alpha=0.5) + #col=as.factor(la_sa_yn)
#   #geom_line(size=1) + 
#   xlab("Year of study") +  
#   ylab("Average local adaptation of metapopulation") +
#   geom_hline(yintercept=0) +
#   theme_classic() + 
#   theme(text=element_text(size=15))
# ggsave("plots/LAstdvsYear.png", width=6, height=5)



###################Poster Candidate 2##########################




#cline was negative in 2010 by switches to positive in more recent years (strongly so in 2015)
#DanielThe effects of drought are striking. 2015 is the year just after selection. I could see this being the final graph in the poster
# Amy: consider including as a second graph, but i could go take it or leave it 

visreg(fullmod.map.no2013, xvar="MAP", by="Drought", overlay=T) #populations sampled under high precip (sites x years) have stronger plastic response to experimental drought
visreg(fullmod.map.no2013, xvar="Drought", by="Year")
visreg(fullmod.map.no2013, xvar="Drought", by="Year", overlay=T) #greater spread among years apparent in drought treatment
visreg(fullmod.map.no2013, by="Drought", xvar="Year")
visreg(fullmod.map.no2013, by="Drought", xvar="Year", overlay=T)

preds.map.no2013 <- ggeffect(fullmod.map.no2013, terms = c("MAP", "Drought", "Year"))
plot(preds.map.no2013) 




######## Individual populations plotted ########

#Site 02
yS02<-y1 %>% 
  filter(Site=="S02") %>% 
  droplevels()
S02.FT <- ggplot(yS02, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#AF003B") +
  geom_point(color="#AF003B") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()
ggplot(yS02, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  #geom_smooth(method='lm',formula=Flowering_Date~MAP) + #regression line not working
  theme_grey() 

#Site 07  
yS07<-y1 %>% 
  filter(Site=="S07") %>% 
  droplevels()
ggplot(yS07, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS07, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S07.FT <- ggplot(yS07, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#DEA048") +
  geom_point(color="#DEA048") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 08  
yS08<-y1 %>% 
  filter(Site=="S08") %>% 
  droplevels()
ggplot(yS08, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
S08.FT <- ggplot(yS08, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#FFC964") +
  geom_point(color="#FFC964") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()
ggplot(yS08, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 

#Site 10  
yS10<-y1 %>% 
  filter(Site=="S10") %>% 
  droplevels()
S10.FT <- ggplot(yS10, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm) +
  geom_point() +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()
ggplot(yS10, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS10, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 

#Site 11  
yS11<-y1 %>% 
  filter(Site=="S11") %>% 
  droplevels()
ggplot(yS11, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  ylim(180, 220) +
  theme_grey()
ggplot(yS11, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S11.FT <- ggplot(yS11, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#F32E38") +
  geom_point(color="#F32E38") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 15  
yS15<-y1 %>% 
  filter(Site=="S15") %>% 
  droplevels()
ggplot(yS15, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS15, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S15.FT <- ggplot(yS15, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#6443A8") +
  geom_point(color="#6443A8") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 16  
yS16<-y1 %>% 
  filter(Site=="S16") %>% 
  droplevels()
ggplot(yS16, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS16, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S16.FT <- ggplot(yS16, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm) +
  geom_point() +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 17  
yS17<-y1 %>% 
  filter(Site=="S17") %>% 
  droplevels()
ggplot(yS17, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS17, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S17.FT <- ggplot(yS17, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#0099BB") +
  geom_point(color="#0099BB") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 18  
yS18<-y1 %>% 
  filter(Site=="S18") %>% 
  droplevels()
ggplot(yS18, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS18, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S18.FT <- ggplot(yS18, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#D7F78D") +
  geom_point(color="#D7F78D") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 29  
yS29<-y1 %>% 
  filter(Site=="S29") %>% 
  droplevels()
ggplot(yS29, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS29, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S29.FT <- ggplot(yS29, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#C1F095") +
  geom_point(color="#C1F095") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 32  
yS32<-y1 %>% 
  filter(Site=="S32") %>% 
  droplevels()
ggplot(yS32, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS32, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S32.FT <- ggplot(yS32, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#FEFEAD") +
  geom_point(color="#FEFEAD") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

#Site 36  
yS36<-y1 %>% 
  filter(Site=="S36") %>% 
  droplevels()
ggplot(yS36, aes(x=Year, y=Flowering_Date)) + 
  geom_boxplot() +
  theme_grey()
ggplot(yS36, aes(x=MAP, y=Flowering_Date, color=Year)) + 
  geom_point() +
  theme_grey() 
S36.FT <- ggplot(yS36, aes(x=Year, y=Flowering_Date)) + 
  geom_smooth(method=lm, color="#0099BB") +
  geom_point(color="#0099BB") +
  #ylim(180, 220) +
  xlab("") +
  ylab("") +
  theme_classic()

FT.201016 <- plot_grid(S02.FT + theme(legend.position = "none"), 
                        S11.FT + theme(legend.position = "none"), 
                        S07.FT + theme(legend.position = "none"),
                        # choose one of S08 or S10
                        S08.FT + theme(legend.position = "none"), 
                        # or S10
                        S32.FT + theme(legend.position = "none"),
                        #S18.poly + theme(legend.position = "none"),
                        S29.FT + theme(legend.position = "none"),
                        S17.FT + theme(legend.position = "none"),
                        S36.FT + theme(legend.position = "none"),
                        S15.FT + theme(legend.position = "none"),
                        nrow=3, ncol=3)
save_plot("Graphs/FT2010-2016_Multipanel.png", FT.201016, base_width=8, base_height=8)


fullmod.map.S2 <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Family) + (1|Block), data=yS02)
summary(fullmod.map.S2)
# drop 3way
no3way.map.S2 <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Family) + (1|Block), data=yS02)
summary(no3way.map.S2)
lrtest(fullmod.map.S2, no3way.map.S2) #3-way interaction highly significant

  
  
  
  
  
  


fullmod.map.S2 <- lmer(Flowering_Date ~ MAP*Drought*Year + (1|Family) + (1|Block), data=yS02)
summary(fullmod.map.S2)
# drop 3way
no3way.map.S2 <- lmer(Flowering_Date ~ MAP*Drought + Drought*Year + MAP*Year+ (1|Family) + (1|Block), data=yS02)
summary(no3way.map.S2)
lrtest(fullmod.map.S2, no3way.map.S2) #3-way interaction highly significant




#Not yet working
# Flowering time evolution
yWet<-y1 %>% 
  filter(Drought=="W") %>% 
  droplevels()
attach(yWet)
lme1.3way<-lme(Flower_Date~CMD*Site*Year, random = Block.x)
summary(lme1.3way)
a1.3way<-Anova(lm1.3way, type=3)
a1.3way 

#Plasticity in flowering time
attach(y1)
lme2.4way<-lme(Flower_Date~CMD*Site*Year*Drought, random = Block)
summary(lme2.4way)
a2.4way<-Anova(lm1.3way, type=3)
a2.4way 






##### Individual Sites #######

##Wet##
#WetS02
yWetS02<-y1 %>% 
  filter(Drought=="W",Site=="S02") %>% 
  droplevels()
attach(yWetS02)
lmyWS02<-lm(Flowering_Date~Year)
summary(lmyWS02)
visreg(lmyWS02) # significant evolution of earlier flowering time P<0.001

#WetS07
yWetS07<-y1 %>% 
  filter(Drought=="W",Site=="S07") %>% 
  droplevels()
attach(yWetS07)
lmyWS07<-lm(Flowering_Date~Year)
summary(lmyWS07)
visreg(lmyWS07) # significant evolution of later flowering time P=0.0425


#WetS08
yWetS08<-y1 %>% 
  filter(Drought=="W",Site=="S08") %>% 
  droplevels()
attach(yWetS08)
lmyWS08<-lm(Flowering_Date~Year)
summary(lmyWS08)
visreg(lmyWS08) # No significant pattern, yet....

#WetS10
yWetS10<-y1 %>% 
  filter(Drought=="W",Site=="S10") %>% 
  droplevels()
attach(yWetS10)
lmyWS10<-lm(Flowering_Date~Year)
summary(lmyWS10)
visreg(lmyWS10) # significant evolution of earlier flowering time P=0.03

#WetS11
yWetS11<-y1 %>% 
  filter(Drought=="W",Site=="S11") %>% 
  droplevels()
attach(yWetS11)
lmyWS11<-lm(Flowering_Date~Year)
summary(lmyWS11)
visreg(lmyWS11) # No signifiant pattern

#WetS15
yWetS15<-y1 %>% 
  filter(Drought=="W",Site=="S15") %>% 
  droplevels()
attach(yWetS15)
lmyWS15<-lm(Flowering_Date~Year)
summary(lmyWS15)
visreg(lmyWS15) # No signifiant pattern

#WetS16
yWetS16<-y1 %>% 
  filter(Drought=="W",Site=="S16") %>% 
  droplevels()
attach(yWetS16)
lmyWS16<-lm(Flowering_Date~Year)
summary(lmyWS16)
visreg(lmyWS16) # No signifiant pattern

#WetS17
yWetS17<-y1 %>% 
  filter(Drought=="W",Site=="S17") %>% 
  droplevels()
attach(yWetS17)
lmyWS17<-lm(Flowering_Date~Year)
summary(lmyWS17)
visreg(lmyWS17) # significant evolution of earlier flowering time 0.029

#WetS18
yWetS18<-y1 %>% 
  filter(Drought=="W",Site=="S18") %>% 
  droplevels()
attach(yWetS18)
lmyWS18<-lm(Flowering_Date~Year)
summary(lmyWS18)
visreg(lmyWS18) # significant evolution of later flowering time P=0.049

#WetS29
yWetS29<-y1 %>% 
  filter(Drought=="W",Site=="S29") %>% 
  droplevels()
attach(yWetS29)
lmyWS29<-lm(Flowering_Date~Year)
summary(lmyWS29)
visreg(lmyWS29) # No significant pattern, yett....

#WetS32
yWetS32<-y1 %>% 
  filter(Drought=="W",Site=="S32") %>% 
  droplevels()
attach(yWetS32)
lmyWS32<-lm(Flowering_Date~Year)
summary(lmyWS32)
visreg(lmyWS32) # Not significant

#WetS36
yWetS36<-y1 %>% 
  filter(Drought=="W",Site=="S36") %>% 
  droplevels()
attach(yWetS36)
lmyWS36<-lm(Flowering_Date~Year)
summary(lmyWS36)
visreg(lmyWS36) # Not significant


fullmod.pop <- lmer(Flowering_Date ~ Site*Drought*Year + (1|Family) + (1|Block), data=y1)
summary(fullmod.pop)
