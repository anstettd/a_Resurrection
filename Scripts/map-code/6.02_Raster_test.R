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
library(ggrepel)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(rCarto)



#Define equal area projection
prj.aea = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0"
prj.wgs = "+proj=longlat"


#Import California & Oregon Map
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#cali_or.aea <- sp_Transform(cali_or, (prj.aea))

#Import Rasters
d.2010 <- readOGR("Maps/USDM_20100706_M/USDM_20100706.shp")
d.2011 <- readOGR("Maps/USDM_20110705_M/USDM_20110705.shp")
d.2012 <- readOGR("Maps/USDM_20120703_M/USDM_20120703.shp")
d.2013 <- readOGR("Maps/USDM_20130702_M/USDM_20130702.shp")
d.2014 <- readOGR("Maps/USDM_20140701_M/USDM_20140701.shp")
d.2015 <- readOGR("Maps/USDM_20150707_M/USDM_20150707.shp")
d.2016 <- readOGR("Maps/USDM_20160705_M/USDM_20160705.shp")



#Set up drought rasters
d.2010.sub <- raster::intersect(d.2010, prj.wgs)
d.2011.sub <- raster::intersect(d.2011, sp.extb)
d.2012.sub <- raster::intersect(d.2012, sp.extb)
d.2013.sub <- raster::intersect(d.2013, sp.extb)
d.2014.sub <- raster::intersect(d.2014, sp.extb)
d.2015.sub <- raster::intersect(d.2015, sp.extb)
d.2016.sub <- raster::intersect(d.2016, sp.extb)





#Import state data
#states<- map_data("state")
#states.wgs <- spTransform(states, CRS=CRS(prj.wgs))
#states.wgs <- sf::st_transform(states, CRS=CRS(prj.wgs))
#cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))


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




