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

### Import raw data from access
Ac1 <- read.csv("Data/BG_Areas.csv", header=T)
Ac2 <- read.csv("Data/plots.csv", header=T)
Y <- left_join(Ac1,Ac2,by=c("PlotID"="Plot"))

# Filter to only include 12 timeseries sites and wanted variables
y1<-Y %>%
  select(SiteID, Year, PlotID, Area, StArea,Count,SiteID) %>% 
  filter(SiteID %in% c("2","7","8","10","11","15","16","17","18","29","32","36")) %>%
  filter(Year>2009) %>%
  droplevels()
y1<-arrange(y1, SiteID, Year)










