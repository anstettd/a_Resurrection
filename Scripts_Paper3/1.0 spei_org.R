##########################################################################################################
## Importing of SPEI data
## Author Daniel Anstett
## Modified from Scott Robenson
##
## Last Modified May 19, 2021
##########################################################################################################

# Clear environment
rm(list = ls())

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
#Make function to calculate spei for given site for water year, 2007 to 2016

spei_water <- function(latitude,longitude){
  # "start" and "count" determine which part of the netCDF files is read
  # I chose these to zoom in on CA and OR (after some experimenting)
  startlon <- 110 ; countlon <- 28
  startlat <- 245 ; countlat <- 30
  lon <- ncvar_get(ncin,"lon", start=startlon, count=countlon)
  lat <- ncvar_get(ncin,"lat", start=startlat, count=countlat)
  time <- ncvar_get(ncin,"time")
  
  # more set-up
  nlat <- nrow(lat) ; nlon <- nrow(lon) ; nt <- nrow(time)
  tdec <- seq(1901+1/24, 2018+23/24, 1/12) # decimal time
  
  # get SPEI data for specified longitudes and latitudes, and all times
  spei12 <- ncvar_get(ncin,'spei',
                      start=c(startlon, startlat, 1),
                      count=c(countlon, countlat, nt) )
  
  # time series for a grid point near Sacramento for last 30 years
  yrs <- rep(1901:2018, times=1, each=12)
  mos <- rep(1:12, times=118)
  target <- which(yrs>=2007 & yrs<=2016 & mos==9)
  ilat <- which(lat == latitude)
  ilon <- which(lon == longitude)
  spei12_sac <- spei12[ilon,ilat,target]
  return(spei12_sac)
}

#e.g. of function
spei_water(38.75,-121.25)
##########################################################################################################
#calculate SPEI

for (i in 1:12){
lats <- lat_long[i,8]
longs <- lat_long[i,9]
spei_return <- spei_water(lats,longs)
lat_long$SPEI_2007[i] <- spei_return[1]
lat_long$SPEI_2008[i] <- spei_return[2]
lat_long$SPEI_2009[i] <- spei_return[3]
lat_long$SPEI_2010[i] <- spei_return[4]
lat_long$SPEI_2011[i] <- spei_return[5]
lat_long$SPEI_2012[i] <- spei_return[6]
lat_long$SPEI_2013[i] <- spei_return[7]
lat_long$SPEI_2014[i] <- spei_return[8]
lat_long$SPEI_2015[i] <- spei_return[9]
lat_long$SPEI_2016[i] <- spei_return[10]
}

#write_csv(lat_long,"Data/spei_pop.csv")


###############
#without function

# "start" and "count" determine which part of the netCDF files is read
# I chose these to zoom in on CA and OR (after some experimenting)
startlon <- 110 ; countlon <- 28
startlat <- 245 ; countlat <- 30
lon <- ncvar_get(ncin,"lon", start=startlon, count=countlon)
lat <- ncvar_get(ncin,"lat", start=startlat, count=countlat)
time <- ncvar_get(ncin,"time")

# more set-up
nlat <- nrow(lat) ; nlon <- nrow(lon) ; nt <- nrow(time)
tdec <- seq(1901+1/24, 2018+23/24, 1/12) # decimal time

# get SPEI data for specified longitudes and latitudes, and all times
spei12 <- ncvar_get(ncin,'spei',
                    start=c(startlon, startlat, 1),
                    count=c(countlon, countlat, nt) )

# time series for a grid point near Sacramento for last 30 years
yrs <- rep(1901:2018, times=1, each=12)
mos <- rep(1:12, times=118)
target <- which(yrs>=2007 & yrs<=2016 & mos==9)
ilat <- which(lat == 32.89928)
ilon <- which(lon == -116.5849)
spei12_sac <- spei12[ilon,ilat,target]
spei12_sac



ilat <- which(lat == 38.75)
ilon <- which(lon == -121.25)


