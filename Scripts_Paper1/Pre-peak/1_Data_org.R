#################
# Reduce data to chosen/site year combinations
#################
library(tidyverse)
library(gdata)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#Remove sit year comibnations that don't follow the climate pattern we are testing
y5 <- filter(y3, ID_Year!="S02_2010" & ID_Year!="S07_2010" & ID_Year!="S07_2015" & ID_Year!="S07_2016")  #S02, S07
y5 <- filter(y5, ID_Year!="S08_2014" & ID_Year!="S10_2010" & ID_Year!="S10_2015" & ID_Year!="S10_2016") #S08, S10
y5 <- filter(y5, Site!="S11") # S11
y5 <- filter(y5, ID_Year!="S15_2016" & ID_Year!= "S17_2016" & ID_Year!="S18_2016" & ID_Year!="S29_2016") #S15, S17, S18, S29
y5 <- filter(y5, ID_Year!="S32_2015" & ID_Year!="S32_2016" & ID_Year!="S36_2016") #S32, S36

write.csv(y5,'Data/y5.csv') #Export file



