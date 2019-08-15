###### Extract drought severity info for timeseries #######

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(ggplot2)

s<-shapefile("GIS/USDM_20100831_I/Drought_Impacts_US.shp")
s@data$id<-rownames(s@data)
droughtPoints <- fortify(s, region = "id")
drought <- merge(droughtPoints, s@data, by = "id")
print(drought)





s<-shapefile("GIS/USDM_20100831_M/USDM_20100831.shp")
s@data$id<-rownames(s@data)
droughtPoints <- fortify(s, region = "id")
drought <- merge(droughtPoints, s@data, by = "id")

ggDrought <- ggplot(data=drought, aes(x=long, y=lat, group = group)) +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_hue(l = 40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(),
        axis.text = element_blank() )

print(ggDrought)