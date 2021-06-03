# read SPEI netCDF data
# S. Robeson, May 2021

library(ncdf4)
setwd("d:/Data/SPEI")
ncin <- nc_open("spei12_2018.nc")  # I renamed this because I have several versions downloaded
print(ncin)

# "start" and "count" determine which part of the netCDF files is read
# I chose these to zoom in on CA and OR (after some experimenting)
startlon <- 110
countlon <- 28
startlat <- 245
countlat <- 30
lon <- ncvar_get(ncin,"lon", start=startlon, count=countlon)
lat <- ncvar_get(ncin,"lat", start=startlat, count=countlat)
time <- ncvar_get(ncin,"time")

# make a grid from lat and lon vectors and visualize
lonlatg <- expand.grid(lon=lon, lat=lat)
par(mfrow=c(1,1),pty="s")
plot(lonlatg, pch=3, cex=0.5)
out <- read.table("Scripts_Paper3/us48.txt")
lines(out$V2, out$V1)
ilat <- which(lat == 38.75)
ilon <- which(lon == -121.25)
points(lon[ilon], lat[ilat], pch=16, cex=2) # ~Sacramento

# more set-up
nlat <- nrow(lat)
nlon <- nrow(lon)
nt <- nrow(time)
tdec <- seq(1901+1/24, 2018+23/24, 1/12) # decimal time

# get SPEI data for specified longitudes and latitudes, and all times
spei12 <- ncvar_get(ncin,'spei',
                 start=c(startlon, startlat, 1),
                 count=c(countlon, countlat, nt) )

# time series for a grid point near Sacramento for last 30 years
ilat <- which(lat == 38.75)
ilon <- which(lon == -121.25)
spei12_sac <- spei12[ilon,ilat,(nt-359):nt]
par(pty="m")
plot(tdec[(nt-359):nt],spei12_sac,
     type="l",
     xlab="",
     ylab="SPEI12",
     main="Sacramento")

# map of one month
library(RColorBrewer)
library(fields)
onetime <- spei12[,,nt] # last month
cutpts <- c(-3,-2,-1,0,1,2,3)
image.plot(lon, lat, onetime, 
      breaks=cutpts,
      col=(brewer.pal(6,"RdBu")),
      xlab="",ylab="",main="SPEI12 for December 2018")
lines(out$V2, out$V1)
points(lonlatg, pch=3, cex=0.3)
