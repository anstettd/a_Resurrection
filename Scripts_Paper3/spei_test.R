##########################################################################################################
## Test importing of SPEI data
## Author Daniel Anstett
## Modified from Scott Robenson
##
## Last Modified May 19, 2021
####################################################################################################################################################################################################################
#Import libraries
library(ncdf4)
library(tidyverse)
##########################################################################################################
#Import Files
ncin <- nc_open("/Users/daniel_anstett/Dropbox/aaUBC/large_files/spei12.nc")  
print(ncin)

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
ilat <- which(lat == 38.75)
ilon <- which(lon == -121.25)
spei12_sac <- spei12[ilon,ilat,target]
spei12_sac

#Does not work for my first site
ilat <- which(lat == 32.89928)
ilon <- which(lon == -116.5849)
spei12_new <- spei12[ilon,ilat,target]
spei12_new #not correct