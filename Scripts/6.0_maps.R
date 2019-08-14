#################
# Site and dought maps for California & Oregon
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
#display.brewer.all()

#Import Rasters
setwd("Data/USDM_20100706_M")
d.2010 <- readOGR("USDM_20100706.shp")
setwd("~/")
setwd("Maps/USDM_20110705_M")
d.2011 <- readOGR("USDM_20110705.shp")
setwd("Maps/USDM_20120703_M")
d.2012 <- readOGR("USDM_20120703.shp")
setwd("Maps/USDM_20130702_M")
d.2013 <- readOGR("USDM_20130702.shp")
setwd("Maps/USDM_20140701_M")
d.2014 <- readOGR("USDM_20140701.shp")
setwd("Maps/USDM_20150707_M")
d.2015 <- readOGR("USDM_20150707.shp")
setwd("Maps/USDM_20160705_M")
d.2016 <- readOGR("USDM_20160705.shp")

prj.wgs <- "+proj=longlat +ellps=WGS84" # Set up WGS 1984 projection

#Import state data
states<- map_data("state")
#states.wgs <- spTransform(states, CRS=CRS(prj.wgs))
states.wgs <- sf::st_transform(states, CRS=CRS(prj.wgs))
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))


# clip the maps with a certain range
sp.extb <- as(extent(-125, -118, 30, 50),"SpatialPolygons") # read your own range file
proj4string(sp.extb) <- proj4string(d.2010)
sp.extb.wgs <- sp_Transform(sp.extb, CRS=CRS(prj.wgs))

#Set up drought rasters
d.2010.sub <- raster::intersect(d.2010, sp.extb)
d.2011.sub <- raster::intersect(d.2011, sp.extb)
d.2012.sub <- raster::intersect(d.2012, sp.extb)
d.2013.sub <- raster::intersect(d.2013, sp.extb)
d.2014.sub <- raster::intersect(d.2014, sp.extb)
d.2015.sub <- raster::intersect(d.2015, sp.extb)
d.2016.sub <- raster::intersect(d.2016, sp.extb)

d.2010.sub.wgs <- raster::intersect(d.2010.wgs, sp.extb.wgs)
d.2011.sub.wgs <- raster::intersect(d.2011.wgs, sp.extb.wgs)
d.2012.sub.wgs <- raster::intersect(d.2012.wgs, sp.extb.wgs)
d.2013.sub.wgs <- raster::intersect(d.2013.wgs, sp.extb.wgs)
d.2014.sub.wgs <- raster::intersect(d.2014.wgs, sp.extb.wgs)
d.2015.sub.wgs <- raster::intersect(d.2015.wgs, sp.extb.wgs)
d.2016.sub.wgs <- raster::intersect(d.2016.wgs, sp.extb.wgs)



#Import Site Data
site.lat.long <- read.csv("Data/trait.means.w.csv", header=T)
site.lat.long <- site.lat.long %>% dplyr::select(ID_Year,Site,Year,Latitude,Longitude)

#Map Making
base_map <- ggplot(cali_or) + geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) + 
  geom_point(data=site.lat.long, aes(x=Longitude,y=Latitude),colour="red")+
#  scale_color_manual(values = col=brewer.pal(n = 12, name = "RdBu"))+
  theme_nothing()
base_map
base_map + facet_wrap( ~ Year, ncol=7)

                       




