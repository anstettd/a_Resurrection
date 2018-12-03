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
library(lmerTest) 
library(visreg)
library(ggeffects)
library(cowplot)

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

#Exclude all plot/year combinations with area missing or less than zero
#There are still subtantial areas missing for plots S08 2016, S10 mostly 2017, 
#S11 2011 & 2014, S15 2011,2013,2017, S17 most years, S32 2011,2012,2017
y2<-y1 %>%
  filter(Area>0.000001) 

#Generate density
y2$Dens<-y2[,6]/y2[,4]

#Generate relative density
y2 <- y2 %>% 
  group_by(SiteID,PlotID) %>% 
  mutate(RelDens = Dens/max(Dens)) %>% 
  ungroup()

y2$SiteID <- as.factor(y2$SiteID)

#Plots of all sites by plot ID

#Sweetwater
yS02<-y2 %>% 
  filter(SiteID=="2") %>% 
  droplevels()
yS02$Dens<-log(yS02$Dens+1)
ggplot(yS02, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS02, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth() 
ggplot(yS02, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS02, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS02, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S02.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS02)
summary(S02.M)
visreg(S02.M, xvar="Year")

S02.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS02)
summary(S02.M)
visreg(S02.M, xvar="Year")

S02.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS02)
summary(S02.M)
visreg(S02.M, xvar="Year")

S02.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS02)
summary(S02.M)
visreg(S02.M, xvar="Year")

# look at slope of decline during drought only
S02.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS02, Year<=2014))
summary(S02.M)
visreg(S02.M, xvar="Year")

# look at slope of recovery post drought only
S02.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS02, Year>=2014))
summary(S02.M)
visreg(S02.M, xvar="Year")

#WF Mojave
yS07<-y2 %>% 
  filter(SiteID=="7") %>% 
  droplevels()
ggplot(yS07, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS07, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS07, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS07, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS07, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S07.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS07)
summary(S07.M)
visreg(S07.M, xvar="Year")

S07.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS07)
summary(S07.M)
visreg(S07.M, xvar="Year")

S07.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS07)
summary(S07.M)
visreg(S07.M, xvar="Year")

S07.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS07)
summary(S07.M)
visreg(S07.M, xvar="Year")

# look at slope of decline during drought only
S07.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS07, Year<=2014))
summary(S07.M)
visreg(S07.M, xvar="Year")

# look at slope of recovery post drought only
S07.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS07, Year>=2014))
summary(S07.M)
visreg(S07.M, xvar="Year")

#Redwoods
yS08<-y2 %>% 
  filter(SiteID=="8") %>% 
  droplevels()
ggplot(yS08, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS08, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS08, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S08.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

# look at slope of decline during drought only
S08.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS08, Year<=2014))
summary(S08.M)
visreg(S08.M, xvar="Year")

# look at slope of recovery post drought only
S08.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS08, Year>=2014))
summary(S08.M)
visreg(S08.M, xvar="Year")

#NFMF Tule
yS10<-y2 %>% 
  filter(SiteID=="10") %>% 
  droplevels()
ggplot(yS10, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS10, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS10, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS10, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS10, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S08.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

S08.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS08)
summary(S08.M)
visreg(S08.M, xvar="Year")

# look at slope of decline during drought only
S08.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = filter(yS08, Year<=2014))
summary(S08.M)
visreg(S08.M, xvar="Year")

# look at slope of recovery post drought only
S08.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = filter(yS08, Year>=2014))
summary(S08.M)
visreg(S08.M, xvar="Year")

#Mill Creek
yS11<-y2 %>% 
  filter(SiteID=="11") %>% 
  droplevels()
ggplot(yS11, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS11, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS11, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS11, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS11, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S11.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS11)
summary(S11.M)
visreg(S11.M, xvar="Year")

S11.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS11)
summary(S11.M)
visreg(S11.M, xvar="Year")

S11.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS11)
summary(S11.M)
visreg(S11.M, xvar="Year")

S11.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS11)
summary(S11.M)
visreg(S11.M, xvar="Year")

# look at slope of decline during drought
S11.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS11,Year<=2014))
summary(S11.M)
visreg(S11.M, xvar="Year")

# look at slope of recovery post drought
S11.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS11,Year>=2014))
summary(S11.M)
visreg(S11.M, xvar="Year")

#Rock Creek
yS15<-y2 %>% 
  filter(SiteID=="15") %>% 
  droplevels()
ggplot(yS15, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS15, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS15, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS15, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS15, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S15.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS15)
summary(S15.M)
visreg(S15.M, xvar="Year")

S15.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS15)
summary(S15.M)
visreg(S15.M, xvar="Year")

S15.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS15)
summary(S15.M)
visreg(S15.M, xvar="Year")

S15.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS15)
summary(S15.M)
visreg(S15.M, xvar="Year")

# slope of decline during drought
S15.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS15,Year<=2014))
summary(S15.M)
visreg(S15.M, xvar="Year")

# slope of recovery post drought
S15.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS15,Year>=2014))
summary(S15.M)
visreg(S15.M, xvar="Year")

#O'Neil Creek
yS16<-y2 %>% 
  filter(SiteID=="16") %>% 
  droplevels()
ggplot(yS16, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS16, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS16, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS16, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS16, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S16.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS16)
summary(S16.M)
visreg(S16.M, xvar="Year")

S16.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS16)
summary(S16.M)
visreg(S16.M, xvar="Year")

S16.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS16)
summary(S16.M)
visreg(S16.M, xvar="Year")

S16.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS16)
summary(S16.M)
visreg(S16.M, xvar="Year")

# slope of decline during drought
S16.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS16,Year<=2014))
summary(S16.M)
visreg(S16.M, xvar="Year")

# slope of recovery post drought
S16.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS16,Year>=2014))
summary(S16.M)
visreg(S16.M, xvar="Year")

#Deep Creek
yS17<-y2 %>% 
  filter(SiteID=="17") %>% 
  droplevels()
ggplot(yS17, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS17, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS17, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS17, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS17, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S17.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS17)
summary(S17.M)
visreg(S17.M, xvar="Year")

S17.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS17)
summary(S17.M)
visreg(S17.M, xvar="Year")

S17.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS17)
summary(S17.M)
visreg(S17.M, xvar="Year")

S17.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS17)
summary(S17.M)
visreg(S17.M, xvar="Year")

# slope of decline during drought
S17.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS17,Year<=2014))
summary(S17.M)
visreg(S17.M, xvar="Year")

# slope of recovery post drought
S17.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS17,Year>=2014))
summary(S17.M)
visreg(S17.M, xvar="Year")

#Little Jamison
yS18<-y2 %>% 
  filter(SiteID=="18") %>% 
  droplevels()
ggplot(yS18, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS18, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS18, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS18, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS18, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S18.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS18)
summary(S18.M)
visreg(S18.M, xvar="Year")

S18.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS18)
summary(S18.M)
visreg(S18.M, xvar="Year")

S18.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS18)
summary(S18.M)
visreg(S18.M, xvar="Year")

S18.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS18)
summary(S18.M)
visreg(S18.M, xvar="Year")

# slope of decline during drought
S18.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS18,Year<=2014))
summary(S18.M)
visreg(S18.M, xvar="Year")

# slope of recovery after drought
S18.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS18,Year>=2014))
summary(S18.M)
visreg(S18.M, xvar="Year")

#Oregon Creek
yS29<-y2 %>% 
  filter(SiteID=="29") %>% 
  droplevels()
ggplot(yS29, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS29, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS29, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS29, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS29, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S29.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS29)
summary(S29.M)
visreg(S29.M, xvar="Year")

S29.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS29)
summary(S29.M)
visreg(S29.M, xvar="Year")

S29.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS29)
summary(S29.M)
visreg(S29.M, xvar="Year")

S29.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS29)
summary(S29.M)
visreg(S29.M, xvar="Year")

# slope of decline during drought
S29.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS29,Year<=2014))
summary(S29.M)
visreg(S29.M, xvar="Year")

# slope of recovery after drought
S29.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS29,Year>=2014))
summary(S29.M)
visreg(S29.M, xvar="Year")

#Wawona
yS32<-y2 %>% 
  filter(SiteID=="32") %>% 
  droplevels()
ggplot(yS32, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS32, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS32, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()+
  ylim(0, 5)
ggplot(yS32, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS32, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()+
  ylim(0, 5)

S32.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS32)
summary(S32.M)
visreg(S32.M, xvar="Year")

S32.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS32)
summary(S32.M)
visreg(S32.M, xvar="Year")

S32.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS32)
summary(S32.M)
visreg(S32.M, xvar="Year")

S32.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS32)
summary(S32.M)
visreg(S32.M, xvar="Year")

# slope of decline during drought
S32.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS32,Year<=2014))
summary(S32.M)
visreg(S32.M, xvar="Year")

# slope of recovery after drought
S32.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS32,Year>=2014))
summary(S32.M)
visreg(S32.M, xvar="Year")

#Deer Creek
yS36<-y2 %>% 
  filter(SiteID=="36") %>% 
  droplevels()
ggplot(yS36, aes(x=Year, y=Dens)) +
  geom_point() +
  geom_smooth()
ggplot(yS36, aes(x=Year, y=RelDens)) +
  geom_point() +
  geom_smooth()
ggplot(yS36, aes(x=Year, y=Dens,color=PlotID)) +
  geom_line()
ggplot(yS36, aes(x=Year, y=RelDens,color=PlotID)) +
  geom_line()
ggplot(yS36, aes(x=Year, y=Dens,color=PlotID)) +
  geom_point()

S36.M <- lmer(Dens ~ poly(Year,2) + (Year|PlotID), data = yS36)
summary(S36.M)
visreg(S36.M, xvar="Year")

S36.M <- lmer(Dens ~ poly(Year,2) + (1|PlotID), data = yS36)
summary(S36.M)
visreg(S36.M, xvar="Year")

S36.M <- lmer(RelDens ~ poly(Year,2) + (Year|PlotID), data = yS36)
summary(S36.M)
visreg(S36.M, xvar="Year")

S36.M <- lmer(RelDens ~ poly(Year,2) + (1|PlotID), data = yS36)
summary(S36.M)
visreg(S36.M, xvar="Year")

# slope of decline during drought
S36.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS36, Year<=2014))
summary(S36.M)
visreg(S36.M, xvar="Year")

# slope of recovery after drought
S36.M <- lmer(RelDens ~ Year + (1|PlotID), data = filter(yS36, Year>=2014))
summary(S36.M)
visreg(S36.M, xvar="Year")


# statistical comparisons of populations

Sall <- lmer(RelDens ~ poly(Year,2)*SiteID + (1|PlotID), data=y2)
lmerTest::summary(Sall)
anova(Sall)
visreg(Sall, xvar="Year", by="SiteID")

Sall.decline <- lmer(RelDens ~ Year*SiteID + (1|PlotID), data=filter(y2,Year<=2014))
summary(Sall.decline)
visreg(Sall.decline, xvar="Year", by="SiteID", overlay=T)
visreg(Sall.decline, xvar="Year", by="SiteID")

Sall.recovery <- lmer(RelDens ~ Year*SiteID + (1|PlotID), data=filter(y2,Year>=2014))
summary(Sall.recovery)
visreg(Sall.recovery, xvar="Year", by="SiteID", overlay=T)
visreg(Sall.recovery, xvar="Year", by="SiteID")

params = c()
params$site=unique(y2$SiteID)
params$glob.int = fixef(Sall.decline)[1]
params$year.slope = fixef(Sall.decline)[2]
params$site.int = fixef(Sall.decline)[3:13]
params$site.slope = fixef(Sall.decline)[14:24]
params

# graphs south to north, linear decline during drought
#sweetwater
S02.decline = ggplot(filter(yS02,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#AF003B") +
  geom_smooth(method=lm, color="#AF003B") +
  xlab("") + 
  ylab("") +
  ylim(0,1.19) +
  xlim(2010,2014)
# mill
S11.decline = ggplot(filter(yS11,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#F32E38") +
  geom_smooth(method=lm, color="#F32E38") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# wf mojave
S07.decline = ggplot(filter(yS07,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="darkorange") +
  geom_smooth(method=lm, color="darkorange") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# nfmf tule
S10.decline = ggplot(filter(yS10,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#FFB853") +
  geom_smooth(method=lm, color="#FFB853") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# redwood
S08.decline = ggplot(filter(yS08,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#FFC964") +
  geom_smooth(method=lm, color="#FFC964") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# wawona
S32.decline = ggplot(filter(yS32,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#FEFEAD") +
  geom_smooth(method=lm, color="#FEFEAD") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# little jamison
S18.decline = ggplot(filter(yS18,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#D7F78D") +
  geom_smooth(method=lm, color="#D7F78D") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# 29 oregon
S29.decline = ggplot(filter(yS29,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#C1F095") +
  geom_smooth(method=lm, color="#C1F095") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# deep
S17.decline = ggplot(filter(yS17,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#0099BB") +
  geom_smooth(method=lm, color="#0099BB") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# deer
S36.decline = ggplot(filter(yS36,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#0099BB") +
  geom_smooth(method=lm, color="#0099BB") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)
# rock
S15.decline = ggplot(filter(yS15,Year<=2014), aes(x=Year, y=RelDens)) +
  geom_point(color="#6443A8") +
  geom_smooth(method=lm, color="#6443A8") +
  xlab("") + 
  ylab("") + 
  ylim(0,1.19) +
  xlim(2010,2014)

all.201014 <- plot_grid(S02.decline + theme(legend.position = "none"), 
                     S11.decline + theme(legend.position = "none"), 
                     S07.decline + theme(legend.position = "none"),
                     S08.decline + theme(legend.position = "none"), 
                     # or S10
                     S32.decline + theme(legend.position = "none"),
                     S18.decline + theme(legend.position = "none"),
                     # orS29
                     S17.decline + theme(legend.position = "none"),
                     S36.decline + theme(legend.position = "none"),
                     S15.decline + theme(legend.position = "none"),
                     nrow=3, ncol=3)
save_plot("Graphs/DensityDeclines.png", all.201014, base_width=8, base_height=8)

# graphs south to north, polynomial trajectories
#sweetwater
S02.poly = ggplot(yS02, aes(x=Year, y=RelDens)) +
  geom_point(color="#AF003B") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#AF003B") +
  xlab("") + 
  ylab("") +
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) + 
  xlim(2010,2017)
# mill
S11.poly = ggplot(yS11, aes(x=Year, y=RelDens)) +
  geom_point(color="#F32E38") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#F32E38") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# wf mojave
S07.poly = ggplot(yS07, aes(x=Year, y=RelDens)) +
  geom_point(color="#DEA048") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#DEA048") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# nfmf tule
S10.poly = ggplot(yS10, aes(x=Year, y=RelDens)) +
  geom_point(color="#FFB853") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#FFB853") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# redwood
S08.poly = ggplot(yS08, aes(x=Year, y=RelDens)) +
  geom_point(color="#FFC964") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#FFC964") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# wawona
S32.poly = ggplot(yS32, aes(x=Year, y=RelDens)) +
  geom_point(color="#FEFEAD") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#FEFEAD") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# little jamison
S18.poly = ggplot(yS18, aes(x=Year, y=RelDens)) +
  geom_point(color="#D7F78D") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#D7F78D") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# 29 oregon
S29.poly = ggplot(yS29, aes(x=Year, y=RelDens)) +
  geom_point(color="#C1F095") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#C1F095") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# deep
S17.poly = ggplot(yS17, aes(x=Year, y=RelDens)) +
  geom_point(color="#0099BB") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#0099BB") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# deer
S36.poly = ggplot(yS36, aes(x=Year, y=RelDens)) +
  geom_point(color="#0099BB") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#0099BB") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)
# rock
S15.poly = ggplot(yS15, aes(x=Year, y=RelDens)) +
  geom_point(color="#6443A8") +
  geom_smooth(method=lm, formula=y~poly(x,2), color="#6443A8") +
  xlab("") + 
  ylab("") + 
  scale_y_continuous(limits=c(-0.1,1.2),breaks=c(0,0.5,1)) +
  xlim(2010,2017)

all.201017 <- plot_grid(S02.poly + theme(legend.position = "none"), 
                        S11.poly + theme(legend.position = "none"), 
                        S07.poly + theme(legend.position = "none"),
                        # choose one of S08 or S10
                        S08.poly + theme(legend.position = "none"), 
                        # or S10
                        S32.poly + theme(legend.position = "none"),
                        #S18.poly + theme(legend.position = "none"),
                        S29.poly + theme(legend.position = "none"),
                        S17.poly + theme(legend.position = "none"),
                        S36.poly + theme(legend.position = "none"),
                        S15.poly + theme(legend.position = "none"),
                        nrow=3, ncol=3)
save_plot("Graphs/DensityUshape.png", all.201017, base_width=8, base_height=8)
