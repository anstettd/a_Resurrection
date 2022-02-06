##########################################################################################################
## Importing of SPEI data
## Standardised Precipitation-Evapotranspiration Index
## Author Daniel Anstett
## 
##
## Last Modified June 4, 2021
##########################################################################################################

# Clear environment
rm(list = ls())

##########################################################################################################
#Import libraries
library(tidyverse)
##########################################################################################################
#Import & arrange files
spei_pop <- read.csv("Data/spei_pop.csv", header=T)
#colnames(spei_pop) <- c("Site","ID","ID2","Site_Lat","Lat","Long","Elevation","Lat_spei","Long_spei",
#                        "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
spei_gather <- spei_pop %>% gather(Year, SPEI,SPEI_2007:SPEI_2016)
spei_gather$Year <- gsub("SPEI_","",spei_gather$Year)
write_csv(spei_gather,"Data/spei_gather.csv")


##########################################################################################################
#spei vs year

Site_Labs<-c("32.89928"="Site 1", "34.28425"="Site 2", "36.69096"="Site 4", "36.20081"="Site 3", 
             "34.07808"="Site 12", "43.37876"="Site 11", "41.80979"="Site 9", "41.66546"="Site 8",
             "39.74298"="Site 7", "39.39442"="Sites 6", "37.539"="Site 5",   "42.27411"="Site 10")
#Make Graph
weath.year <- ggplot(spei_gather, aes(Year,SPEI,group=Site))+
  geom_point(size=3)+
  geom_line()+
  theme_minimal()
weath.year <- weath.year + facet_wrap( ~ Lat, ncol=4, labeller=labeller(Lat=Site_Labs))
weath.year + theme(legend.text = element_text(size = 12, face = "bold"),
                   legend.title = element_text(size=14, face="bold"),
                   axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                   axis.text.y = element_text(size=14,face="bold"),
                   axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                   axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"),
                   strip.text = element_text(size = 14, face="bold"))
ggsave("Paper3_graphs/SPEI_year.pdf",width=15,height=8)



