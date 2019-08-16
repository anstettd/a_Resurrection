#################
# Site and dought maps for California & Oregon, not raster drought file
#################
library(tidyverse)
library(ggeffects)
library(maps)
library(ggplot2)
library(ggmap)
library(rgdal)
library(raster)
library(RColorBrewer)
library(sf)
library(ggrepel)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(rCarto)

#Import California & Oregon Map
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#Import Site Data

site.lat.long <- read.csv("Data/trait.means.w.csv", header=T)
site.lat.long <- site.lat.long %>% dplyr::select(ID_Year,Site,Year,Latitude,Longitude)

#Map Making
base_map <- ggplot(cali_or) + geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) + 
  geom_point(data=site.lat.long, aes(x=Longitude,y=Latitude,colour=Site),size=4)+
  scale_colour_manual(values=c("S02"="#990000","S11"="#CC0000","S07"="#FF0000","S10"="#FF6666","S08"="#FF9999","S32"="#FFCCCC",
                      "S29"="#CCCCCC","S18"="#99CCFF","S17"="#0099FF","S16"="#0066FF","S36"="#0000CC","S15"="#6600CC"))+
theme_nothing()
base_map
base_map + facet_wrap( ~ Year, ncol=7)

#Site Labled Map
base_map <- ggplot(cali_or) + geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) + 
  geom_point(data=site.lat.long, aes(x=Longitude,y=Latitude,colour=Site),size=2.5)+
  scale_colour_manual(values=c("S02"="#990000","S11"="#CC0000","S07"="#FF0000","S10"="#FF6666","S08"="#FF9999","S32"="#FFCCCC",
                               "S29"="#CCCCCC","S18"="#99CCFF","S17"="#0099FF","S16"="#0066FF","S36"="#0000CC","S15"="#6600CC"))+
  geom_text_repel(data=site.lat.long, aes(Longitude, Latitude, label = Site), size = 4.5) +
  theme_nothing()
base_map + facet_wrap( ~ Year, ncol=7)





