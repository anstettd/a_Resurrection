#################
# Get weather data into Oct 1 to Sept 31 format
#################
# Also assessment of normality
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

#Import datasets and add year_actual variable
weather_2009 <- read.csv("Climate/timeseries_monthly_2009csv", header=T)



#Select data starting with "Tave", "PPT", "CMD"




