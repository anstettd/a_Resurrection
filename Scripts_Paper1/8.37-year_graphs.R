#################
# Generate 37 year trendline figure
#################

library(tidyverse)
library(cowplot)


m_year<-read.csv('Data/m_year.csv', header=T) #Imports 1980-2009 climate data
m_year<- m_year %>% select(ID,ID2,hist_year,Latitude,CMD,MAT,MAP)
colnames(m_year)[3]<-"Year"

weather<-read.csv('Data/weather.csv', header=T) #Imports 2009-2016 climate data
weather<- weather %>% select(ID,ID2,Year,Latitude,CMD,MAT,MAP)

clim_time<-bind_rows(m_year,weather)
clim_time<-clim_time %>% filter(ID!="S11")
clim_time<-clim_time %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                          ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) 
clim_time$Region<-as.factor(clim_time$Region) #Make Region appear in logical order
clim_time$Region<-factor(clim_time$Region,levels=c("North","Centre","South"))
#clim_time<-clim_time %>% mutate(Site=ID)
clim_time<-clim_time %>% mutate(Site = ifelse(ID =="S02", 1, 
  ifelse((ID=="S07"), 2, 
         ifelse((ID=="S10"), 3,
                ifelse((ID=="S08"), 4,
                       ifelse((ID=="S32"), 5,
                              ifelse((ID=="S29"), 6,
                                     ifelse((ID=="S18"), 7,
                                            ifelse((ID=="S17"), 8,
                                                   ifelse((ID=="S16"), 9,
                                                          ifelse((ID=="S36"), 10,11)))))))))))

#Amy's Site Names

all_CMD <-ggplot(clim_time , aes(x=Year, y=CMD))+ 
  geom_line()+
  facet_wrap(.~ID)
all_CMD
ggsave("CMD.pdf", width = 7, height = 12, units = "in")

all_MAT <-ggplot(clim_time , aes(x=Year, y=MAT))+ 
  geom_line()+
  facet_wrap(.~ID)
all_MAT
ggsave("MAT.pdf", width = 7, height = 12, units = "in")

all_MAP <-ggplot(clim_time , aes(x=Year, y=MAP))+ 
  geom_line()+
  facet_wrap(.~ID)
all_MAP
ggsave("MAP.pdf", width = 7, height = 12, units = "in")


#Ascending Site Names

all_CMD <-ggplot(clim_time , aes(x=Year, y=CMD))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
all_CMD 
ggsave("Fig S4.pdf", width = 12, height = 7, units = "in")

all_MAT <-ggplot(clim_time , aes(x=Year, y=MAT))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
  all_MAT
ggsave("Fig S5.pdf", width = 12, height = 7, units = "in")

all_MAP <-ggplot(clim_time , aes(x=Year, y=MAP))+ 
  geom_line()+
  facet_wrap(.~Site)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))
  all_MAP
ggsave("Fig S6.pdf", width = 12, height = 7, units = "in")
  

