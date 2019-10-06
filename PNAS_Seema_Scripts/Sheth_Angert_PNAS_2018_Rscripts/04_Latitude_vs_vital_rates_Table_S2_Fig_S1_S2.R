#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Perform linear regression models of each vital rate as a function of latitude
############# Vital rates include survival, growth, flowering, fruit count, probability of recruitment, and mean offspring size
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

# remove objects and clear workspace
rm(list = ls(all=TRUE))

#require packages
require(MuMIn)
require(plyr)
require(dplyr)
require(ggplot2)
require(cowplot)
require(RColorBrewer)

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
# Class: stage class (juvenile, adult, or NA) of plant at time = t 
# Fec1: Total number of fruits per individual   
# logSize: total stem length of the individual
# ClassNext: stage class (juvenile, adult, dead, or NA) of plant at time = t+1 
# logSizeNext: same as "logSize" above, for t+1
# Surv: survival (1) or not (0) of individuals between time = t and time = t+1
# Year: annual transition of the long-term data at time = t (2010-2013)
# Fec0: Probability of flowering (1 if Class=="A" for adult, 0 if Class=="J" for juvenile)
# RegionRank: ordinal rank of regions from south to north
# SeedCt: mean seed count, rounded to the nearest integer, for each site

#*******************************************************************************
#### 2. Summarize size classes by site ###
#*******************************************************************************

# Compute total # of individuals tracked in study (reported in Methods section of MS)
length(unique(data$ID)) # 11,244 individuals tracked across all sites & years
### 2 additional unique individuals were present at Canton Creek, but do not have unique IDs (ID=NA), so final # is 11,246


# Compute total # of individuals tracked per site (reported in Table S4)
N_indiv_site=summarise(group_by(data,Site), N = length(unique(ID))) %>% data.frame() 
### Final # for Canton Creek should be 145 instead of 143

# Create function to calculate standard error
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(x[!is.na(x)])) 

# Create a vector of evenly spaced latitude 
lat=seq(min(data$Latitude),max(data$Latitude),length.out=length(unique(data$Site)))

# Summarize site name and latitude into data frame and sort by decreasing Latitude
site.data=summarise(group_by(data,Site), Latitude=unique(Latitude)) %>% data.frame() %>% arrange(-Latitude)

# Calculate other size quantiles for each site
size.quantiles=summarise(group_by(data,Site), sm=quantile(logSize,0.2,na.rm=TRUE),low.mid=quantile(logSize,0.4,na.rm=TRUE),hi.mid=quantile(logSize,0.6,na.rm=TRUE),lg=quantile(logSize,0.8,na.rm=TRUE)) %>% data.frame()

# merge size quantiles with full dataset
data=join(data,size.quantiles)

# assign size category to each plant; Asm=small, Bmed=medium, Clg=large
data$sizeClass=ifelse(data$logSize<=data$sm,"Asm",ifelse(data$logSize > data$low.mid & data$logSize <= data$hi.mid, "Bmed",ifelse(data$logSize > data$lg,"Clg",NA)))

# obtain mean & SE of logSize for each size class at each site
size_class_summary=summarise(group_by(data,Latitude,sizeClass),mean=mean(logSize,na.rm=TRUE),se=stderr(logSize)) %>% data.frame() %>% filter(!is.na(sizeClass))

#*******************************************************************************
#### 3. Plot latitude vs. size, and indicate small, medium, and large quantiles for each site
#*******************************************************************************

# Remove data with no size recorded at year = t
data_sizePlot=filter(data,!is.na(sizeClass)) 

# Save plot as .pdf
pdf("Figures/FigS1_mean_size_classes_vs_lat.pdf",width=5,height=5)
ggplot(data_sizePlot, aes(Latitude,logSize)) + 
  geom_point(aes(color=factor(sizeClass)),alpha=0.1,size=0.5) +
  scale_color_manual(values=c("deepskyblue3","forestgreen","red4"),labels=c("sm","med","lg")) +
  xlab(expression(paste("Latitude (",degree~N,")"))) +
  ylab("ln (size at year = t)") +
  theme(legend.title = element_blank()) +
  geom_point(data=size_class_summary,aes(Latitude,mean,fill=factor(sizeClass)),shape=21,color="lightgrey",size=3) +
  geom_errorbar(data=size_class_summary,aes(x=Latitude,ymin=mean-se,ymax=mean+se),show.legend=FALSE,inherit.aes = FALSE) +
  scale_fill_manual(values=c("deepskyblue3","forestgreen","red4"),labels=c("sm","med","lg")) 
dev.off()
  
#*******************************************************************************
#### 4. Survival
#*******************************************************************************

  #*******************************************************************************
  ####4A. Estimate survival probability for each size class
  #*******************************************************************************

  # estimate observed survival probability for plants in lowest 20% size class at each site
  obs.surv.sm=data %>% group_by(Site) %>% 
    filter(logSize <= sm&!is.na(Surv)) %>% 
    summarise(obs.surv.sm=length(Surv[Surv==1])/length(Surv[!is.na(Surv)])) %>% data.frame() 

  # estimate observed survival probability for plants in middle 20% size class at each site
  obs.surv.med=data %>% group_by(Site) %>% 
    filter(logSize > low.mid&logSize<=hi.mid&!is.na(Surv)) %>% 
    summarise(obs.surv.med=length(Surv[Surv==1])/length(Surv[!is.na(Surv)])) %>% data.frame() 

  # estimate observed survival probability for plants in highest 20% size class at each site
  obs.surv.lg=data %>% group_by(Site) %>% 
    filter(logSize > lg&!is.na(Surv)) %>% 
    summarise(obs.surv.lg=length(Surv[Surv==1])/length(Surv[!is.na(Surv)])) %>% data.frame() 

  # merge all observed survival probabilities with site data
  site.data=site.data %>% join(obs.surv.sm) %>% join(obs.surv.med) %>% join(obs.surv.lg)

  #*******************************************************************************
  ####4B. Model survival at each size class as a function of latitude
  #*******************************************************************************
  
  ### SMALL 
  # small size class with linear & quadratic terms for latitude
  surv.mod.sm=lm(obs.surv.sm~poly(Latitude,2),data=site.data)
  summary(surv.mod.sm)
  
  # small size class with linear term for latitude
  surv.mod.sm.a=lm(obs.surv.sm~Latitude,data=site.data)
  summary(surv.mod.sm.a)
  AICc(surv.mod.sm,surv.mod.sm.a)

  ### MEDIUM 
  # medium size class with linear & quadratic terms for latitude
  surv.mod.med=lm(obs.surv.med~poly(Latitude,2),data=site.data)
  summary(surv.mod.med)
  
  # medium size class with linear term for latitude
  surv.mod.med.a=lm(obs.surv.med~Latitude,data=site.data)
  summary(surv.mod.med.a)
  AICc(surv.mod.med,surv.mod.med.a)

  ### LARGE
  # large size class with linear & quadratic terms for latitude
  surv.mod.lg=lm(obs.surv.lg~poly(Latitude,2),data=site.data)
  summary(surv.mod.lg)
  
  # large size class with linear term for latitude
  surv.mod.lg.a=lm(obs.surv.lg~Latitude,data=site.data)
  summary(surv.mod.lg.a)
  AICc(surv.mod.lg,surv.mod.lg.a)

  #*******************************************************************************
  ####4C.Plot latitude vs. survival
  #*******************************************************************************
  
pdf("Figures/FigS2_vital_rates_vs_lat.pdf",width=8,height=10)
par(las=1,bty="l",cex.lab=1.2,cex.axis=1,mfrow=c(3,2),oma = c(5,2,0,2) + 0.1,mar = c(0,2,1,2) + 0.1,xpd=NA)
plot(site.data$Latitude,site.data$obs.surv.sm,col="deepskyblue3",pch=19,xlab="",ylab="Prob (Survival)",ylim=c(0,1),xaxt="n")
points(site.data$Latitude,site.data$obs.surv.med,col="forestgreen",pch=19)
points(site.data$Latitude,site.data$obs.surv.lg,col="red4",pch=19)
lines(lat,predict(surv.mod.sm,newdata=data.frame(Latitude=lat),type="response"),col="deepskyblue3",lty=2,lwd=2)
lines(lat,predict(surv.mod.sm.poly,newdata=data.frame(Latitude=lat),type="response"),col="deepskyblue3",lty=2,lwd=2)
lines(lat,predict(surv.mod.med,newdata=data.frame(Latitude=lat),type="response"),col="forestgreen",lty=2,lwd=2)
lines(lat,predict(surv.mod.med.poly,newdata=data.frame(Latitude=lat),type="response"),col="forestgreen",lty=2,lwd=2)
lines(lat,predict(surv.mod.lg,newdata=data.frame(Latitude=lat),type="response"),col="red4",lwd=2)
mtext("A",side=3,adj=-0.1,font=2,line=0)

# add legend
legend(x=32.5,y=1.1,legend=c("sm","med","lg"),pch=19,col=c("deepskyblue3","forestgreen","red4"),bty="n")

#*******************************************************************************
#### 5. Growth measured as logSizeNext
#*******************************************************************************

  #*******************************************************************************
  ####5A. Estimate growth for each size class
  #*******************************************************************************

  # estimate observed mean Growth for plants in lowest 20% size class at each site
  obs.logSizeNext.sm=data %>% group_by(Site) %>% 
    filter(logSize <= sm) %>% 
    summarise(obs.logSizeNext.sm=mean(logSizeNext,na.rm=TRUE)) %>% data.frame() 

  # estimate observed mean Growth for plants in middle 20% size class at each site
  obs.logSizeNext.med=data %>% group_by(Site) %>% 
    filter(logSize > low.mid&logSize<=hi.mid) %>% 
    summarise(obs.logSizeNext.med=mean(logSizeNext,na.rm=TRUE)) %>% data.frame() 

  # estimate observed mean Growth for plants in highest 20% size class at each site
  obs.logSizeNext.lg=data %>% group_by(Site) %>% 
    filter(logSize > lg) %>% 
    summarise(obs.logSizeNext.lg=mean(logSizeNext,na.rm=TRUE)) %>% data.frame() 

  # merge all observed mean Growth with site data
  site.data=site.data %>% join(obs.logSizeNext.sm) %>% join(obs.logSizeNext.med) %>% join(obs.logSizeNext.lg)

  #*******************************************************************************
  ####5B. Model growth at each size class as a function of latitude
  #*******************************************************************************

  ### SMALL 
  # small size class with linear & quadratic terms for latitude
  growth.mod.sm=lm(obs.logSizeNext.sm~poly(Latitude,2),data=site.data)
  summary(growth.mod.sm)
  
  # small size class with linear term for latitude
  growth.mod.sm.a=lm(obs.logSizeNext.sm~Latitude,data=site.data)
  summary(growth.mod.sm.a)
  AICc(growth.mod.sm,growth.mod.sm.a)
  
  ### MEDIUM 
  # medium size class with linear & quadratic terms for latitude
  growth.mod.med=lm(obs.logSizeNext.med~poly(Latitude,2),data=site.data)
  summary(growth.mod.med)
  
  # medium size class with linear term for latitude
  growth.mod.med.a=lm(obs.logSizeNext.med~Latitude,data=site.data)
  summary(growth.mod.med.a)
  AICc(growth.mod.med,growth.mod.med.a)
  
  ### LARGE
  # large size class with linear & quadratic terms for latitude
  growth.mod.lg=lm(obs.logSizeNext.lg~poly(Latitude,2),data=site.data)
  summary(growth.mod.lg)
  
  # large size class with linear term for latitude
  growth.mod.lg.a=lm(obs.logSizeNext.lg~Latitude,data=site.data)
  summary(growth.mod.lg.a)
  AICc(growth.mod.lg,growth.mod.lg.a)
  
  #*******************************************************************************
  ####5C.Plot latitude vs. growth
  #*******************************************************************************
  
  plot(site.data$Latitude,site.data$obs.logSizeNext.sm,col="deepskyblue3",pch=19,xlab="",ylab="log (size at year = t+1)",ylim=c(min(site.data$obs.logSizeNext.sm,na.rm=T),max(site.data$obs.logSizeNext.lg,na.rm=T)),xaxt="n")
  points(site.data$Latitude,site.data$obs.logSizeNext.med,col="forestgreen",pch=19)
  points(site.data$Latitude,site.data$obs.logSizeNext.lg,col="red4",pch=19)
  lines(lat,predict(growth.mod.sm,newdata=data.frame(Latitude=lat),type="response"),col="deepskyblue3",lwd=2)
  lines(lat,predict(growth.mod.med.a,newdata=data.frame(Latitude=lat),type="response"),col="forestgreen",lwd=2)
  lines(lat,predict(growth.mod.lg,newdata=data.frame(Latitude=lat),type="response"),col="red4",lwd=2,lty=2)
  mtext("B",side=3,adj=-0.1,font=2,line=0)
  
#*******************************************************************************
#### 6. Flowering
#*******************************************************************************

  #*******************************************************************************
  ####6A. Estimate flowering probability for each size class
  #*******************************************************************************
  
  # estimate observed flowering probability for plants in lowest 20% size class at each site
  obs.fl.sm=data %>% group_by(Site) %>% 
    filter(logSize <= sm&!is.na(Fec0)) %>% 
    summarise(obs.fl.sm=length(Fec0[Fec0==1])/length(Fec0[!is.na(Fec0)])) %>% data.frame() 
  
  # estimate observed flowering probability for plants in middle 20% size class at each site
  obs.fl.med=data %>% group_by(Site) %>% 
    filter(logSize > low.mid&logSize<=hi.mid&!is.na(Fec0)) %>% 
    summarise(obs.fl.med=length(Fec0[Fec0==1])/length(Fec0[!is.na(Fec0)])) %>% data.frame() 
  
  # estimate observed flowering probability for plants in highest 20% size class at each site
  obs.fl.lg=data %>% group_by(Site) %>% 
    filter(logSize > lg&!is.na(Fec0)) %>% 
    summarise(obs.fl.lg=length(Fec0[Fec0==1])/length(Fec0[!is.na(Fec0)])) %>% data.frame() 
  
  # merge all observed flowering probabilities with site data
  site.data=site.data %>% join(obs.fl.sm) %>% join(obs.fl.med) %>% join(obs.fl.lg)
  
  #*******************************************************************************
  ####6B. Model flowering probability at each size class as a function of latitude
  #*******************************************************************************
  
  ### SMALL 
  # small size class with linear & quadratic terms for latitude
  fl.mod.sm=lm(obs.fl.sm~poly(Latitude,2),data=site.data)
  summary(fl.mod.sm)
  
  # small size class with linear term for latitude
  fl.mod.sm.a=lm(obs.fl.sm~Latitude,data=site.data)
  summary(fl.mod.sm.a)
  AICc(fl.mod.sm,fl.mod.sm.a)
  
  ### MEDIUM 
  # medium size class with linear & quadratic terms for latitude
  fl.mod.med=lm(obs.fl.med~poly(Latitude,2),data=site.data)
  summary(fl.mod.med)
  
  # medium size class with linear term for latitude
  fl.mod.med.a=lm(obs.fl.med~Latitude,data=site.data)
  summary(fl.mod.med.a)
  AICc(fl.mod.med,fl.mod.med.a)
  
  ### LARGE
  # large size class with linear & quadratic terms for latitude
  fl.mod.lg=lm(obs.fl.lg~poly(Latitude,2),data=site.data)
  summary(fl.mod.lg)
  
  # large size class with linear term for latitude
  fl.mod.lg.a=lm(obs.fl.lg~Latitude,data=site.data)
  summary(fl.mod.lg.a)
  AICc(fl.mod.lg,fl.mod.lg.a)

  #*******************************************************************************
  ####6C.Plot latitude vs. probability of flowering
  #*******************************************************************************
  
  plot(site.data$Latitude,site.data$obs.fl.sm,col="deepskyblue3",pch=19,xlab="",ylab="Prob (Flowering)",ylim=c(0,1),xaxt="n")
  points(site.data$Latitude,site.data$obs.fl.med,col="forestgreen",pch=19)
  points(site.data$Latitude,site.data$obs.fl.lg,col="red4",pch=19)
  lines(lat,predict(fl.mod.sm.a,newdata=data.frame(Latitude=lat),type="response"),col="deepskyblue3",lty=2,lwd=2)
  lines(lat,predict(fl.mod.med,newdata=data.frame(Latitude=lat),type="response"),col="forestgreen",lty=2,lwd=2)
  mtext("C",side=3,adj=-0.1,font=2,line=0)
  
#*******************************************************************************
#### 7. Fruit #
#*******************************************************************************

  #*******************************************************************************
  ####7A. Estimate fruit # for each size class
  #*******************************************************************************
  
  # estimate observed mean fruit # for plants in lowest 20% size class at each site
  obs.fr.sm=data %>% group_by(Site) %>% 
    filter(logSize <= sm) %>% 
    summarise(obs.fr.sm=mean(Fec1,na.rm=TRUE)) %>% data.frame() 
  
  # estimate observed mean fruit # for plants in middle 20% size class at each site
  obs.fr.med=data %>% group_by(Site) %>% 
    filter(logSize > low.mid&logSize<=hi.mid) %>% 
    summarise(obs.fr.med=mean(Fec1,na.rm=TRUE)) %>% data.frame() 
  
  # estimate observed mean fruit # for plants in highest 20% size class at each site
  obs.fr.lg=data %>% group_by(Site) %>% 
    filter(logSize > lg) %>% 
    summarise(obs.fr.lg=mean(Fec1,na.rm=TRUE)) %>% data.frame() 
  
  # merge all observed mean fruit # with site data
  site.data=site.data %>% join(obs.fr.sm) %>% join(obs.fr.med) %>% join(obs.fr.lg)
  
  # log-transform fruit #
  site.data$obs.fr.med=log(site.data$obs.fr.med)
  site.data$obs.fr.lg=log(site.data$obs.fr.lg)

  #*******************************************************************************
  ####7B. Model fruit # at each size class as a function of latitude
  #*******************************************************************************
  
  ### IGNORE SMALL BECAUSE SMALL PLANTS ONLY FLOWERED AT 3 SITES 
  ### IGNORE MEDIUM BECAUSE VERY FEW FRUITS WERE PRODUCED BY MEDIUM PLANTS
  
  ### LARGE
  # large size class with linear & quadratic terms for latitude
  fr.mod.lg=lm(obs.fr.lg~poly(Latitude,2),data=site.data)
  summary(fr.mod.lg)
  
  # large size class with linear term for latitude
  fr.mod.lg.a=lm(obs.fr.lg~Latitude,data=site.data)
  summary(fr.mod.lg.a)
  AICc(fr.mod.lg,fr.mod.lg.a)
  
  #*******************************************************************************
  ####7C. Plot latitude vs. fruit #
  #*******************************************************************************
  
  plot(site.data$Latitude,site.data$obs.fr.lg,col="red4",pch=19,xlab="",ylab="ln (fruit #)",xaxt="n")
  mtext("D",side=3,adj=-0.1,font=2,line=0)
  
#*******************************************************************************
#### 8. Recruitment probability
#*******************************************************************************

  #*******************************************************************************
  ####8A. Estimate fruit # for each size class
  #*******************************************************************************
  
  # Obtain number of new recruits per site
  recruit.number=data %>% group_by(Site) %>% 
    summarise(recruit.number=length(logSizeNext[is.na(logSize)])) %>% data.frame() 
  
  # Obtain total seed count per site (= # fruits per site * # seeds per fruit per site)
  total.seeds.per.site=site_fruit_count_data %>% group_by(Site) %>% summarise(total.seeds.per.site=sum(Fec1,na.rm=TRUE)*mean(SeedCt,na.rm=TRUE)) %>% data.frame()
  
  # Estimate recruitment probability as # of new recruits/# of seeds
  establishment.prob=recruit.number$recruit.number/total.seeds.per.site$total.seeds.per.site %>% data.frame()
  establishment.prob=rename(establishment.prob,establishment.prob=.)
  establishment.prob$Site=recruit.number$Site
  
  # merge per capita recruitment with site data
  site.data=site.data %>% join(establishment.prob)
  
  #*******************************************************************************
  ####8B. Model recruitment probability as a function of latitude
  #*******************************************************************************
  
  estab.mod.lat=lm(establishment.prob~poly(Latitude,2),data=site.data)
  summary(estab.mod.lat)
  
  # recruitment probability with linear term for latitude
  estab.mod.lat.a=lm(establishment.prob~Latitude,data=site.data)
  summary(estab.mod.lat.a)
  AICc(estab.mod.lat,estab.mod.lat.a)
  
  #*******************************************************************************
  ####8C. Plot latitude vs. probability of recruitment
  #*******************************************************************************
  
  plot(site.data$Latitude,site.data$establishment.prob,ylab="Prob (Recruitment)",pch=19,xaxt="n",cex.axis=0.8,xlab=expression(paste("Latitude (",degree~N,")")))
  lines(lat,predict(estab.mod.lat.a,newdata=data.frame(Latitude=lat),type="response"),lwd=2)
  mtext("E",side=3,adj=-0.1,font=2,line=0)

#*******************************************************************************
#### 9. Size distribution of offspring
#*******************************************************************************

  #*******************************************************************************
  ####9A. Obtain mean size of new recruits per site
  #*******************************************************************************
  
  recruit.size.mean=tapply(data$logSizeNext[is.na(data$logSize)],data$Site[is.na(data$logSize)],FUN="mean") %>% data.frame()
  recruit.size.mean$Site=rownames(recruit.size.mean)
  colnames(recruit.size.mean)=c("recruit.size.mean","Site")
  
  # merge per capita recruitment with site data
  site.data=site.data %>% join(recruit.size.mean)
  
  #*******************************************************************************
  ####9B. Model mean offspring size as a function of latitude
  #*******************************************************************************
  
  # mean size of new recruits with linear & quadratic terms for latitude
  recruit.mod.lat=lm(recruit.size.mean~poly(Latitude,2),data=site.data)
  summary(recruit.mod.lat)
  
  # mean size of new recruits with linear term for latitude
  recruit.mod.lat.a=lm(recruit.size.mean~Latitude,data=site.data)
  summary(recruit.mod.lat.a)
  AICc(recruit.mod.lat,recruit.mod.lat.a)
  
  #*******************************************************************************
  ####9C. Plot latitude vs. mean offspring size
  #*******************************************************************************
  
  plot(site.data$Latitude,site.data$recruit.size.mean,xlab=expression(paste("Latitude (",degree~N,")")),ylab="log (offspring size)",pch=19)
  lines(lat,predict(recruit.mod.lat,newdata=data.frame(Latitude=lat),type="response"),lwd=2)
  mtext("F",side=3,adj=-0.1,font=2,line=0)
  
  dev.off()
