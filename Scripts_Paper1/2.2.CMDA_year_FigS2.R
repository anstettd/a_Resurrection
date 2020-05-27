#################
# CMDA, MATA, MAPA versus Year per site
#################
library(tidyverse)


wna_anom <- read.csv("Data/wna_graph.csv", header=T) #Imports climate dataset by site/year
wna_anom<-wna_anom %>% mutate(Site.Lat=paste(Latitude, Site, sep="_"))
wna_anom

Site_Labs<-c("32.89928_S02"="Site 1", "34.28425_S07"="Site 2", "36.69096_S08"="Site 4", "36.20081_S10"="Site 3", 
             "34.07808_S11"="Site 12", "43.37876_S15"="Site 11", "41.80979_S16"="Site 9", "41.66546_S17"="Site 8",
             "39.74298_S18"="Site 7", "39.39442_S29"="Sites 6", "37.539_S32"="Site 5",   "42.27411_S36"="Site 10")
#CMDA versus Year
weath.year <- ggplot(wna_anom, aes(Year,CMD.anom))+
  geom_point(size=3, aes(colour=Decision))+
  geom_line()+
  theme_minimal()
#All in one Graph
weath.year <- weath.year + facet_wrap( ~ Site.Lat, ncol=4, labeller=labeller(Site.Lat=Site_Labs))+
  scale_color_manual(values= c("Included"="black", "Remove"="red", "Unavailable"="blue"))
weath.year + theme(legend.text = element_text(size = 12, face = "bold"),
                   legend.title = element_text(size=14, face="bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"),
                   strip.text = element_text(size = 14, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Climatic Moisture Deficit Anomaly")




#MATA versus Year
weath.year <- ggplot(wna_anom, aes(Year,MAT.anom,color=Site))+
  geom_point(size=2)+
  geom_line()+
  theme_minimal()
#All in one Graph
weath.year + theme(legend.text = element_text(size = 12, face = "bold"), 
                   axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                   axis.text.y = element_text(size=14,face="bold"),
                   axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                   axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) 
weath.year.f <- weath.year + facet_wrap( ~ Site, ncol=4)
weath.year.f + theme(legend.text = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean Annual Temp Anomaly°C")

#MAPA versus Year
weath.year <- ggplot(wna_anom, aes(Year,MAP.anom,color=Site))+
  geom_point(size=2)+
  geom_line()+
  theme_minimal()
#All in one Graph
weath.year + theme(legend.text = element_text(size = 12, face = "bold"), 
                   axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                   axis.text.y = element_text(size=14,face="bold"),
                   axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                   axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) 
weath.year.f <- weath.year + facet_wrap( ~ Site, ncol=4)
weath.year.f + theme(legend.text = element_text(size = 12, face = "bold"),
                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                     axis.text.y = element_text(size=14,face="bold"),
                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="log Mean Annual Precipitation Anomaly")


####### Correlations between climate and climate anomaly for CMD, MAP & MAT
# are historical climate and anomalies correlated?
y3.clim.amom.cor <- y3 %>% select(CMD.clim.s, MAT.clim.s, MAP.clim.s, CMD.anom.s, MAT.anom.s, MAP.anom.s) #Generate list; remember that MAP.s are on log scale
y3.clim.amom.cor.m<-as.matrix(y3.clim.amom.cor) # make into a matrix
rcorr(y3.clim.amom.cor.m) # get all correlation coeff






















