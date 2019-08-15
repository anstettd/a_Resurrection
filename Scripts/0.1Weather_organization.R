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

wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv")

impact_all <- wna1 %>% select(ID_Year1,Year,Elevation)
impact_all_sum <- wna1 %>% select(ID_Year1,Year,Elevation)

#Import datasets and add year_actual variable
weather_2009 <- read.csv("Climate/timeseries_monthly_2009.csv", header=T)
weather_2010 <- read.csv("Climate/timeseries_monthly_2010.csv", header=T)
weather_2011 <- read.csv("Climate/timeseries_monthly_2011.csv", header=T)
weather_2012 <- read.csv("Climate/timeseries_monthly_2012.csv", header=T)
weather_2013 <- read.csv("Climate/timeseries_monthly_2013.csv", header=T)
weather_2014 <- read.csv("Climate/timeseries_monthly_2014.csv", header=T)
weather_2015 <- read.csv("Climate/timeseries_monthly_2015.csv", header=T)
weather_2016 <- read.csv("Climate/timeseries_monthly_2016.csv", header=T)

# Oct1 to Dec 31 from previous year + Jan1 to Set 30 current year

#weather_2010 = eval(parse(text=(paste("weather",i,sep="_"))))
#weather_2009 = eval(parse(text=(paste("weather",i-1,sep="_"))))


# For 2010 year (example to understand how the for loop works)
impact_summary <- data.frame()

for(i in 2010:2016){
  impact<- eval(parse(text=(paste("weather",i-1,sep="_")))) %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
  #MAT  
  impact_aT <- eval(parse(text=(paste("weather",i-1,sep="_")))) %>% select(Tave10,Tave11,Tave12)
  impact_bT <- eval(parse(text=(paste("weather",i,sep="_"))))%>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                                                        Tave06,Tave07,Tave08,Tave09)
  impact_T <- cbind(impact_bT,impact_aT)
  MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
  impact <- cbind(impact,MAT)
  #MAP  
  impact_aT <- eval(parse(text=(paste("weather",i-1,sep="_")))) %>% select(PPT10,PPT11,PPT12)
  impact_bT <- eval(parse(text=(paste("weather",i,sep="_"))))%>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
  impact_T <- cbind(impact_bT,impact_aT)
  MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
  impact <- cbind(impact,MAP)
  #CMD  
  impact_aT <- eval(parse(text=(paste("weather",i-1,sep="_")))) %>% select(CMD10,CMD11,CMD12)
  impact_bT <- eval(parse(text=(paste("weather",i,sep="_"))))%>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
  impact_T <- cbind(impact_bT,impact_aT)
  CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
  impact <- cbind(impact,CMD)
  impact_summary <- rbind(impact_summary,impact)
}
impact_all <- cbind(impact_all,impact_summary)
write.csv(impact_all,'Data/weather.csv') #Export file



###############################################################################################################################

#Check the for loop one year at at time
impact_sum <- data.frame()
#2010
impact<- weather_2009 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2009 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2010 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                                                      Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2009 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2010 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2009 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2010 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)


#One year at at time
#2011
impact<- weather_2010 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2010 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2011 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2010 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2011 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2010 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2011 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)


#One year at at time
#2012
impact<- weather_2011 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2011 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2012 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2011 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2012 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2011 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2012 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)

#One year at at time
#2013
impact<- weather_2012 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2012 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2013 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2012 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2013 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2012 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2013 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)

#One year at at time
#2014
impact<- weather_2013 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2013 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2014 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2013 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2014 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2013 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2014 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)

#One year at at time
#2015
impact<- weather_2014 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2014 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2015 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2014 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2015 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2014 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2015 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)

#One year at at time
#2016
impact<- weather_2015 %>% select(ID,ID2,Latitude,Longitude)
#  impact<-cbind(impact,c(rep(i,12)))
#MAT  
impact_aT <- weather_2015 %>% select(Tave10,Tave11,Tave12)
impact_bT <- weather_2016 %>% select(Tave01,Tave02,Tave03,Tave04,Tave05,
                                     Tave06,Tave07,Tave08,Tave09)
impact_T <- cbind(impact_bT,impact_aT)
MAT <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAT)
#MAP  
impact_aT <- weather_2015 %>% select(PPT10,PPT11,PPT12)
impact_bT <-  weather_2016 %>% select(PPT01,PPT02,PPT03,PPT04,PPT05,PPT06,PPT07,PPT08,PPT09)
impact_T <- cbind(impact_bT,impact_aT)
MAP <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,MAP)
#CMD  
impact_aT <- weather_2015 %>% select(CMD10,CMD11,CMD12)
impact_bT <-  weather_2016 %>% select(CMD01,CMD02,CMD03,CMD04,CMD05,CMD06,CMD07,CMD08,CMD09)
impact_T <- cbind(impact_bT,impact_aT)
CMD <- rowMeans(impact_T, na.rm = FALSE, dims = 1)
impact <- cbind(impact,CMD)
impact_sum <- rbind(impact_sum,impact)

impact_all_sum <- cbind(impact_all_sum,impact_sum)
