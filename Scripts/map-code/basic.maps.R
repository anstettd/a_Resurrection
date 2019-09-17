library(rgdal)
library(maps)
library(sp)
library(rgeos)
library(maptools)
library(raster)
library(rCarto)
library(tidyverse)


prj.wgs = "+proj=longlat +ellps=WGS84"
prj.aea = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0"

states <- get(load("Maps/states.RData")) # North America Map
states.wgs <- spTransform(states, CRS=CRS(prj.wgs))
states.aea <- spTransform(states, CRS=CRS(prj.aea))

#Set up Lat/long for time series and baseline data as projected spatial objects#
pops<-read.csv("Maps/Baseline&Timeseries_pops.csv") # population samples
timeseries<-pops %>% 
  filter(Baseline.Timesieries=="Timeseries") %>% 
  droplevels()
baseline<-pops %>% 
  filter(Baseline.Timesieries=="Baseline") %>% 
  droplevels()
coordinates(timeseries) <- ~Long+Lat
coordinates(baseline) <- ~Long+Lat
proj4string(timeseries) <- CRS(prj.wgs)
proj4string(baseline) <- CRS(prj.wgs)
time.aea <- spTransform(timeseries, CRS=CRS(prj.aea))
base.aea <- spTransform(baseline, CRS=CRS(prj.aea))


setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20100706_M")
d.2010 <- readOGR("USDM_20100706.shp")
d.2010.wgs <- spTransform(d.2010, CRS=CRS(prj.wgs))
d.2010.aea <- spTransform(d.2010, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20110705_M")
d.2011 <- readOGR("USDM_20110705.shp")
d.2011.wgs <- spTransform(d.2011, CRS=CRS(prj.wgs))
d.2011.aea <- spTransform(d.2011, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20120703_M")
d.2012 <- readOGR("USDM_20120703.shp")
d.2012.wgs <- spTransform(d.2012, CRS=CRS(prj.wgs))
d.2012.aea <- spTransform(d.2012, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20130702_M")
d.2013 <- readOGR("USDM_20130702.shp")
d.2013.wgs <- spTransform(d.2013, CRS=CRS(prj.wgs))
d.2013.aea <- spTransform(d.2013, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20140701_M")
d.2014 <- readOGR("USDM_20140701.shp")
d.2014.wgs <- spTransform(d.2014, CRS=CRS(prj.wgs))
d.2014.aea <- spTransform(d.2014, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20150707_M")
d.2015 <- readOGR("USDM_20150707.shp")
d.2015.wgs <- spTransform(d.2015, CRS=CRS(prj.wgs))
d.2015.aea <- spTransform(d.2015, CRS=CRS(prj.aea))

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Maps/USDM_20160705_M")
d.2016 <- readOGR("USDM_20160705.shp")
d.2016.wgs <- spTransform(d.2016, CRS=CRS(prj.wgs))
d.2016.aea <- spTransform(d.2016, CRS=CRS(prj.aea))

# readOGR will read the .prj file if it exists, compared to readShapePoly
d.2010@data # check data.frame of the polygon

# set your own color, same order as test@data
DM.col <- c("#FFFF00","#FCD37F","#FFAA00","#E60000","#730000")

# clip the maps with a certain range
sp.extb <- as(extent(-125, -118, 30, 50),"SpatialPolygons") # read your own range file
proj4string(sp.extb) <- proj4string(d.2010)
sp.extb.wgs <- spTransform(sp.extb, CRS=CRS(prj.wgs))
sp.extb.aea <- spTransform(sp.extb, CRS=CRS(prj.aea))

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

d.2010.sub.aea <- raster::intersect(d.2010.aea, sp.extb.aea)
d.2011.sub.aea <- raster::intersect(d.2011.aea, sp.extb.aea)
d.2012.sub.aea <- raster::intersect(d.2012.aea, sp.extb.aea)
d.2013.sub.aea <- raster::intersect(d.2013.aea, sp.extb.aea)
d.2014.sub.aea <- raster::intersect(d.2014.aea, sp.extb.aea)
d.2015.sub.aea <- raster::intersect(d.2015.aea, sp.extb.aea)
d.2016.sub.aea <- raster::intersect(d.2016.aea, sp.extb.aea)


#state lines
states.sub <- raster::intersect(states, sp.extb)
states.sub.wgs <- raster::intersect(states.wgs, sp.extb.wgs)
states.sub.aea <- raster::intersect(states.aea, sp.extb.aea)

#gridlines
grd.sub.wgs = gridlines(states.sub.wgs, ndiscr=200)
grd.sub.aea = spTransform(grd.sub.wgs, CRS=CRS(prj.aea))
gridat <- gridat(states.sub.wgs, side="EN")
gridat.aea = spTransform(gridat, CRS=CRS(prj.aea), side="EN")
lab.all = parse(text=as.character(gridat.aea$labels))
coord.all = coordinates(gridat.aea)


# Timeseires Map
quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='black')
#plot(plot(d.2010.sub, add =T, col=DM.col)
#title("2010")
points(baseline, pch=21, cex=1, col="black", bg="red")
points(timeseries, pch=21, cex=, col="black", bg="red")







quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2011.sub, add =T, col=DM.col)
points(pops.2011, cex=pops.2011$X2011/10)
title("2011")
dev.off()

quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2012.sub, add =T, col=DM.col)
points(pops.2012, cex=pops.2012$X2012/10)
title("2012")

quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2013.sub, add =T, col=DM.col)
points(pops.2013, cex=pops.2013$X2013/10)
title("2013")

quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2014.sub, add =T, col=DM.col)
points(pops.2014, cex=pops.2014$X2014/10)
title("2014")

quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2015.sub, add =T, col=DM.col)
points(pops.2015, cex=pops.2015$X2015/10)
title("2015")

quartz()
plot(sp.extb, border = 'white')
plot(states, add=T, border='gray')
plot(d.2016.sub, add =T, col=DM.col)
points(pops.2016, cex=pops.2016$X2016/10)
title("2016")


#multi-panel fig
quartz()
setwd("/Users/amyangert/Desktop/Figure 1")
pdf(file="Figure1.pdf", width=8.5, height=5)
par(mfrow=c(1,4))#, omi=c(0.2,0.2,0.2,0.2))

par(mai=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
plot(sp.extb.aea, border='white')
plot(grd.sub.aea, add=T, col='gray')
plot(states.sub.aea, add=T, border='gray')
plot(d.2010.sub.aea, add=T, col=DM.col)
points(pops.2010.aea, pch=21, cex=1.5, col="gray", bg="black")
title("2010")
text(coord.all, labels=lab.all, pos=c(2,2,4,4,4), col="black") 

par(mai=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
plot(sp.extb.aea, border='white')
plot(grd.sub.aea, add=T, col='gray')
plot(states.sub.aea, add=T, border='gray')
plot(d.2012.sub.aea, add=T, col=DM.col)
points(pops.time.2012.aea, pch=21, cex=1.5, col="gray", bg="black")
title("2012")
text(coord.all, labels=lab.all, pos=c(2,2,4,4,4), col="black") 

par(mai=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
plot(sp.extb.aea, border='white')
plot(grd.sub.aea, add=T, col='gray')
plot(states.sub.aea, add=T, border='gray')
plot(d.2014.sub.aea, add=T, col=DM.col)
points(pops.time.2014.aea, pch=21, cex=1.5, col="gray", bg="black")
title("2014")
text(coord.all, labels=lab.all, pos=c(2,2,4,4,4), col="black") 

par(mai=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
plot(sp.extb.aea, border='white')
plot(grd.sub.aea, add=T, col='gray')
plot(states.sub.aea, add=T, border='gray')
plot(d.2016.sub.aea, add=T, col=DM.col)
points(pops.time.2016.aea, pch=21, cex=1.5, col="gray", bg="black")
title("2016")
text(coord.all, labels=lab.all, pos=c(2,2,4,4,4), col="black") 

dev.off()

