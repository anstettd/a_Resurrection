##########################################################################################################
## Find lat long inputs for SPEI data
## Author Daniel Anstett
## Modified from Scott Robenson
##
## Last Modified June 3, 2021
##########################################################################################################


##########################################################################################################
#Import libraries
library(ncdf4)
library(tidyverse)
##########################################################################################################
#Import Files
ncin <- nc_open("/Users/daniel_anstett/Dropbox/aaUBC/large_files/spei12.nc")  
print(ncin)
lat_long <- read.csv("Data/site_lat.csv", header=T)
lat_long <- as.data.frame(lat_long)

##########################################################################################################
# Setup to select California & Oregon
startlon <- 110
countlon <- 28
startlat <- 245
countlat <- 30
lon <- ncvar_get(ncin,"lon", start=startlon, count=countlon)
lat <- ncvar_get(ncin,"lat", start=startlat, count=countlat)
time <- ncvar_get(ncin,"time")
# make a grid from lat and lon vectors and visualize
lonlatg <- expand.grid(lon=lon, lat=lat)








