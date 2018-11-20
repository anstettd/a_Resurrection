###########
#Population Density
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
library(ggplot2)
library(lme4) 
library(lmtest) 
library(visreg)
library(ggeffects)

### Import raw data from Access
Ac1 <- read.csv("Data/BG_Areas.csv", header=T)
Ac2 <- read.csv("Data/plots.csv", header=T) %>% 
  select(Plot, SubPlot, SiteID, ID)
Y <- left_join(Ac1,Ac2,by=c("PlotID"="ID"))

# Filter to only include 12 timeseries sites and wanted variables
y1<-Y %>%
  select(SiteID, Year, PlotID, Area, StArea,Count,SiteID) %>% 
  filter(SiteID %in% c("2","7","8","10","11","15","16","17","18","29","32","36")) %>%
  filter(Year>2009) %>%
  droplevels()
y1<-arrange(y1, SiteID, Year)

y1$PlotID <- paste("P", y1$PlotID, sep="_")

#Exclue all plot/year combinations with area missing or less than zero
#There are still subtantial areas missing for plots S08 2016, S10 mostly 2017, 
#S11 2011 & 2014, S15 2011,2013,2017, S17 most years, S32 2011,2012,2017
y2<-y1 %>%
  filter(Area>0.000001)

#Generate density
y2[,7]<-y2[,6]/y2[4]

#Plots of all sites by plot ID

#Sweetwater
yS02<-y2 %>% 
  filter(SiteID=="2") %>% 
  droplevels()
yS02$Area.1<-log(yS02$Area.1+1)
ggplot(yS02, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS02, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS02, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

S02.M <- lmer(Area.1 ~ Year + (Year|PlotID), data = yS02)
summary(S02.M)
visreg(S02.M)


#WF Mojave
yS07<-y2 %>% 
  filter(SiteID=="7") %>% 
  droplevels()
ggplot(yS07, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS07, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS07, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

S07.M <- lmer(Area.1 ~ Year + (Year|PlotID), data = yS07)
summary(S07.M)
visreg(S07.M)



#Redwoods
yS08<-y2 %>% 
  filter(SiteID=="8") %>% 
  droplevels()
ggplot(yS08, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS08, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS08, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

S08.M <- lmer(Area.1 ~ Year + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M)




#NFMF Tule
yS10<-y2 %>% 
  filter(SiteID=="10") %>% 
  droplevels()
ggplot(yS10, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS10, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS10, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

S08.M <- lmer(Area.1 ~ Year + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M)


#Mill Creek
yS11<-y2 %>% 
  filter(SiteID=="11") %>% 
  droplevels()
ggplot(yS11, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS11, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS11, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

#Rock Creek
yS15<-y2 %>% 
  filter(SiteID=="15") %>% 
  droplevels()
ggplot(yS15, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS15, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS15, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

#O'Neil Creek
yS16<-y2 %>% 
  filter(SiteID=="16") %>% 
  droplevels()
ggplot(yS16, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS16, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS16, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

#Deep Creek
yS17<-y2 %>% 
  filter(SiteID=="17") %>% 
  droplevels()
ggplot(yS17, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS17, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS17, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()

#Little Jamison
yS18<-y2 %>% 
  filter(SiteID=="18") %>% 
  droplevels()
ggplot(yS18, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS18, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS18, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()


#Oregon Creek
yS29<-y2 %>% 
  filter(SiteID=="29") %>% 
  droplevels()
ggplot(yS29, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS29, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS29, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()


#Wawona
yS32<-y2 %>% 
  filter(SiteID=="32") %>% 
  droplevels()
ggplot(yS32, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS32, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()+
  ylim(0, 5)
ggplot(yS32, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()+
  ylim(0, 5)

#Deer Creek
yS36<-y2 %>% 
  filter(SiteID=="36") %>% 
  droplevels()
ggplot(yS36, aes(x=Year, y=Area.1)) +
  geom_point() +
  geom_smooth()
ggplot(yS36, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_line()
ggplot(yS36, aes(x=Year, y=Area.1,color=PlotID)) +
  geom_point()


