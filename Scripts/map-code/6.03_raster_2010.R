#################
# Site and dought maps for California & Oregon
#################
library(tidyverse)
library(rgdal)    # to read in the shapefile
library(sp)       # for Spatial* classes and coordinate projections`
library(gstat)     # to support geostatistical stuff
library(raster)
library(sf)
library(FRK)
library(rgeos)

#Two possible projections I can use. aea, the conical equal area one will like be more useful/necessary for some calculations.
prj.aea = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0"
prj.wgs = "+proj=longlat +ellps=WGS84"
#These next two lines would allow for a large, US-wide shape file holding drought severity data to be shruken down to just Western USA. 
#This was helpful in previous version of the code. Now however, I want to grab only the data that intersects with California and Oregon.
#Being able to do this will make the maps we make much more focused on the actual study region.
#sp.extb <- as(extent(-125, -118, 30, 50),"SpatialPolygons") # read your own range file
#proj4string(sp.extb) <- proj4string(d.2010)

# Colors I will evetually use for each of the different parts of the drought severity shpape file.
#DM.col <- c("#FFFF00","#FCD37F","#FFAA00","#E60000","#730000")


#Import California & Oregon Map
#Grap state information for both California and Oregon. This makes a dataframe plotable in ggplot2. 
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#Make polygon from lat-long dataframe and project into "aea"
cali_or.p <- df_to_SpatialPolygons(cali_or,"region",c("long","lat"), CRS(prj.aea))
class(cali_or.p) #above code generate a Spatial Polygons file 
#gets ride of small overlaps between states (messes up other calcs)
cali_or.g <- gBuffer(cali_or.p, byid=TRUE, width=0) 
#Turn outline of Cali and Oregon from "Spatial Polygons" file to SpatialPolygonsDataFrame", which makes it the same as drought data
cali_or.spdf <- SpatialPolygonsDataFrame(cali_or.g, 
              data.frame(id = unique(cali_or$region), row.names = unique(cali_or$region)),match.ID = F)
class(cali_or.spdf) #Is now "SpatialPolygonsDataFrame"
#Import drought shape file from 2010
d.2010 <- readOGR("Maps/USDM_20100706_M/USDM_20100706.shp")
d.2010.p <- spTransform(d.2010,CRS(prj.aea)) #project into aea

#Attempt to clip drought specific data with Cali+Oregon polygon. Both are same class of files, should work according to stack overflow
d.2010.clip <- gIntersection(d.2010.p, cali_or.spdf, byid = c(TRUE,FALSE), drop_lower_td = TRUE) 
class(d.2010.clip) #Class is null indicating a SpatialPolygonsDataFrame did not come out the other end. 

#This is where I am wright now. I also explored the idea of turning the drought area into a raster file, then try the clipping.
#This is easier said that done. There are lot of specifics required, stuff for which ArcGIS just put in defaults. 
#I could try to figure it out if its the only way.
#Overall I'm wondering if it would be easier/faster just to use ArcGIS. These tasks would take me <1 hour to do. 



#cali_or.p <- SpatialPolygons(cali_or,"region",c("long","lat"), CRS(prj.aea)) #ignore for now.

