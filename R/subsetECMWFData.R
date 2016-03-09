# subsetECMWFData.R 

# This script will be used to get useful subsets of ecmwf data. ecmwf provides
# data on the most crazy grids I have ever seen. 

library(ncdf4)
library(fields)
library(maps)

# read in the nc data 
fileName <- paste0("ecmwfData/2m-T-00UTC_ecmwf_may-oct.nc")
variable <- 't2m'

fileName <- paste0("ecmwfData/cloudFrac_00Z_ecmwf_may-oct.nc")
variable <- "tcc"

compressNC <- function(fileName=fileName,
                       variable=varibale){
  
  # Get nc data and dims 
  nc          <- nc_open(fileName) # data only contains June,July,August
  gridLon     <- ncvar_get(nc,"longitude")
  gridLat     <- ncvar_get(nc,"latitude")
  
  # NOTE: ecmwf latitude and longitude coords refer to middle or topleft of 
  # NOTE: grid box? 
  
  # Get the 2-meter temperature data
  X <- ncvar_get(nc,variable)    # [Longitude, Latitude, time]
  
  # This is a climate model grid, which is a bit tricky for me to wrap my 
  # head around because I am used to writing in standard coordinates. As a 
  # result I am going to re-write the part of the grid I care about, and plot
  # it with the workspace data to make sure it makes sense. 
  
  # arbitrary new name, needs to be the same dimensions as the original nc data
  N   <- X
  nLat <- dim(X)[2]
  
  # loop through every time, switching latitude for each step
  for (i in 1:dim(X)[3]){
    M <- X[,,i]
    N[,,i] <- M[, rev(1:nLat)] 
  }
  
  # Western hemisphere is negative coordinates on standard grid
  # NOTE: this will not correct the eastern hemisphere, but we are not using 
  # NOTE: that part of the data so that is OK
  newLon <- gridLon - 360
  newLat <- rev(gridLat) # because we switched the latitude in the matrix
  
  # subset the data by geographic coorinates
  lonMask <- newLon >= -180 # western hemisphere
  latMask <- newLat > 0     # northern hemisphere
  
  N_subset <- N[lonMask,latMask,]
  lat_subset <- newLat[latMask]
  lon_subset <- newLon[lonMask]
  
  #assign(variable, N_subset)
  assign("gridLon", lon_subset)
  assign("gridLat", lat_subset)
  
  
  ##############################################################################
  # Organize seconds data to something useful 
  ##############################################################################
  hours     <- ncvar_get(nc,"time")  # units: hours since 1900-01-01 00:00:0.0"
  # NOTE: If you plot hours you will see jumps, that is because this data is
  # NOTE: already subset May - Oct for each year
  
  seconds   <- hours * 60^2
  ecmwfTime <- as.POSIXct(seconds,  origin="1900-01-01 00:00:0.0",tz="UTC")
  
  L <- list()
  L[[variable]]  <- N_subset
  L[["gridLon"]] <- gridLon
  L[["gridLat"]] <- gridLat
  L[["ecmwfTime"]] <- ecmwfTime
  assign(variable, L)
  
  # Create a list to ruturn
  save(file=paste0('ecmwfData/',variable,".RData"),
       list=c(variable))
  
}

# compressNC(fileName=fileName,
#            variable=variable)


# Sanity check: Does ecmwf time series make sense? Do the temperature and cloud
#               fraction images make sense?
# NOTE: I have checked the cloud fraction images for several dates against the
# NOTE: MODIS image available from http://ge.ssec.wisc.edu/modis-today/
# NOTE: This comparison gives me confidense that times are aligned and that this
# NOTE: cloud fraction product is in agreement with observations. 
sanityCheck <- function(){

  # Load the ecmwf Temperature data
  load("ecmwfData/t2m.RData")
  gridLon <- t2m[["gridLon"]]
  gridLat <- t2m[["gridLat"]]
  ecmwfTime <- t2m[["ecmwfTime"]]
  t2m <- t2m[["t2m"]] # this will eraze the list 
  
  load("ecmwfData/tcc.RData")
  tcc <- tcc[["tcc"]] # this will erase the list 
  
  
  # Convert to degrees C 
  t2m <- t2m - 273.15
  
  # Now convert the temperature from C to f 
  t2m <- t2m * 9/5 + 32
  
  n <- dim(t2m)[3]
  for(i in 1:n){
    
    pdf(file=paste0("ecmwfData/dailyTemperature/",ecmwfTime[i],".pdf"),
        width=20, height=20)
    
    par(mfrow=c(2,1))
    
    image.plot(gridLon,gridLat,t2m[,,i])
    map("world", xlim=c(min(gridLon), max(gridLon)),
        ylim=c(min(gridLat), max(gridLat)), add=TRUE)
    map("state", add=TRUE)
    title(paste(ecmwfTime[i], "00Z degrees F"))
    
    image.plot(gridLon,gridLat,tcc[,,i])
    map("world", xlim=c(min(gridLon), max(gridLon)),
        ylim=c(min(gridLat), max(gridLat)), add=TRUE)
    map("state", add=TRUE)
    title(paste("cloud fraction"))
    
    
    
    
    
    dev.off()
    
  }

}


