##### Extract informtion from flower rate raster
#Libraries
#install.packages("ncdf4")
library("raster")
library("ncdf4")
#Import ".nc" file
setwd("/Users/daniel_anstett/Dropbox/aaUBC/Flow_layer")
flow <- raster ("FLO1K.ts.1960.2015.qav.nc")
proj4string(flow)=CRS("+init=EPSG:4326")





#