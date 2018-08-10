###########
#Correct Year
###########
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
### Data prep
Y <- read.csv("Data/drought.csv", header=T) #specify relative paths within the project folder instead of using setwd
year_rosetta<-read.csv("Data/timeseries.roseta.csv", header=T)
year_r<- year_rosetta %>% select(Site_Family,Year)
y1<-left_join(Y,year_r,by=c("Site_Family1"="Site_Family"))
Y3<-left_join(Y,wna2,by=c("ID_Site2"="ID_Year1"))
write.csv(y1,"Data/drought1.csv")