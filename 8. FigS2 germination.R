#################
# Germination graphs per site
#################

library(tidyverse)

m_year<-read.csv('Data/Germination_output.csv', header=T) #Imports germination data

ger_year <-ggplot(m_year , aes(x=Year, y=Percent_Germination))+ 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(2010, 2016, 2))+
  facet_wrap(.~Site)
ger_year


ger_year <-ggplot(m_year , aes(x=Year, y=Percent_Germination))+ 
  geom_line()+
  geom_point()+
  xlm(0,100)
  facet_wrap(.~Site)
ger_year