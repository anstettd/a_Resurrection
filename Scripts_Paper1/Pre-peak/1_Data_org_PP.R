#################
# Reduce data to chosen/site year combinations
#################
library(tidyverse)
library(gdata)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#Remove sit year comibnations that don't follow the climate pattern we are testing
y6 <- filter(y3, ID_Year=="S02_2011" | ID_Year=="S02_2014" |  #Site 1 _ S02
               ID_Year=="S07_2011" | ID_Year=="S07_2014" | #Site 2 _ S07
               ID_Year=="S10_2011" | ID_Year=="S10_2014" | #Site 3 _ S10               
               ID_Year=="S08_2011" | ID_Year=="S08_2013" | #Site 4 _ S08                
               ID_Year=="S32_2010" | ID_Year=="S32_2014" | #Site 5 _ S32                
               ID_Year=="S29_2010" | ID_Year=="S29_2015" | #Site 6 _ S29                
               ID_Year=="S18_2010" | ID_Year=="S18_2014" | #Site 7 _ S18                
               ID_Year=="S17_2011" | ID_Year=="S17_2015" | #Site 8 _ S17 
               ID_Year=="S16_2010" | ID_Year=="S16_2016" | #Site 9 _ S16                
               ID_Year=="S36_2011" | ID_Year=="S36_2015" | #Site 10 _ S36                
               ID_Year=="S15_2010" | ID_Year=="S15_2015")  #Site 11 _ S15                
write.csv(y6,'Data/y6.csv') #Export file






