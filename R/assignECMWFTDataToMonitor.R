# assignReanalysisTDataToMonitor.R 

# This script assigns 00Z ecmwf reanalysis data to monitors 
# 2m - ecmwf data was downloaded 
# http://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/

library(fields)
library(maps)
library(geosphere)
################################################################################  
# Assigns gridded temperature data to PM monitors passed here 
################################################################################  
getStationWeatherData <- function(workSpaceData=workSpaceData, 
                                  sanityCheck=FALSE){
  
  
  # Load the ecmwf Temperature data
  load("ecmwfData/t2m.RData")
  gridLon <- t2m[["gridLon"]]
  gridLat <- t2m[["gridLat"]]
  ecmwfTime <- t2m[["ecmwfTime"]]
  t2m <- t2m[["t2m"]] # this will erase the list 
  
  
  # Plot the data on a map, with monitor locations, to make sure everything
  # lines up and makes sense
  map('world')
  image.plot(gridLon, gridLat,t2m[,,1], add=TRUE)
  map('world', add=TRUE)
  
  # Load the monitor locations to plot with this data, do they overlap?
  lon <- workSpaceData[['lon']]
  lat <- workSpaceData[['lat']]
  points(lon,lat, pch=19, cex=0.5)
  

  image.plot(gridLon, gridLat,t2m[,,1])
  map("world", xlim=c(min(gridLon), max(gridLon)),
      ylim=c(min(gridLat), max(gridLat)), add=TRUE)
  map('state', add=TRUE)
  points(lon,lat, pch=19)
  title(paste("showing the ecmwf data grid for ", ecmwfTime[1]))
  
    
  # Load the cloud cover data (exact same grid, same lon lat and time)
  load("ecmwfData/tcc.RData")
  tcc <- tcc[["tcc"]] # this will erase the list 
    
  ##############################################################################
  # NOTE ON ECMWFTime:
  ##############################################################################
  # This 00Z time is 6:00 PM MDT 8PM EDT and 5 PM PDT of the PREVIOUS day. 
  # 00Z is the date of the day just starting, still previous date in US at this
  # hour. 
  # AKA: 00Z is 7 hours ahead of time here in Fort Collins MST.
  # http://scc-ares-races.org/generalinfo/utcchart.html
  # http://www.timeanddate.com/worldclock/timezone/zulu
  
  # So the date in North America is not the same as that of 00Z because they are
  # 7 hours ahead and 00Z has the date of the day just starting. 

  # Take a day away to make this time more useful to the PM and ozone data
  # which is in US time and date. That daily data will most meaningfully be 
  # matched with evenning temperature data of the 00Z data. 
  # NOTE: Evening temperature snapshot is not a perfect way to determine 
  # NOTE: ozone relevant ozone production but should be decent at determining
  # NOTE: what days are generally warmer and cooler than others. 
  secondsInDay <- 24 * 60^2
  ecmwfTimeModified <- ecmwfTime - secondsInDay
  
  # Load requested data packet
  Hybrid_mdf <- workSpaceData[["Hybrid_mdf"]]
  PM_df      <- workSpaceData[["PM_df"]]  
  lon        <- workSpaceData[["lon"]]
  lat        <- workSpaceData[["lat"]]
  
  # copycat will be replaced with T and cloud fraction values from ecmwf
  T_df       <- PM_df # temperature dataframe
  CC_df      <- PM_df # cloud cover dataframe
  nMonitors  <- dim(PM_df)[2] # columns of PM_df are PM Monitors at stations  

  # These dates are made from local times in the US. which are a day behind 00Z
  # I am placing them in UTC time zone so that it will be possible to match
  # the modified ecmwf time series. In reality there is no actual time of day
  # associated with daily PM and ozone data. 
  measuredDataDate <- rownames(T_df)
  PMTime <- as.POSIXct(measuredDataDate, tz="UTC") # SO THAT WE CAN MATCH!
  # NOTE: The time zone of the loaded ecmwf time is UTC. 
    
  # Loop through monitors and assign the appropriate grid of ecmwf time series 
  # data 
  for (i in 1:nMonitors){
    
    # Clear out the dataframe of PM values resulting from copying
    T_df[,i]  <- NA
    CC_df[,i] <- NA
    
    # Find the grid box the monitor falls inside of, create a mask 
    lonIndex <- which.min(abs(gridLon - lon[i]))
    latIndex <- which.min(abs(gridLat - lat[i]))
    
    # Compute the haversine distance of monitor to gridpoint center chosen
    meterPerkm <- 1000
    glon <- gridLon[lonIndex]
    glat <- gridLat[latIndex]
    greatCircleDistance <- distHaversine(c(glon,glat), c(lon[i], lat[i]))/meterPerkm
    #print(greatCircleDistance)
    
    # The maximum dy of this ecmwf data should be the hypotenouse of the 
    # A=dy/2, B=dy/2 of ecmwf data (dy=0.75 degrees).  C = (111.3195/2km^2 + 111.3195/2^2)^.5
    # so C=78.71477km
    maxDistanceAccepted <- 64 #km
    if(greatCircleDistance > maxDistanceAccepted){
      stop("You are not pulling the correct temperature data for this monitor.")
    }
    
    
    # Plot the grid center and monitor location, contributes to general sanity
    points(lon[i],lat[i], col="white", pch=19)
    points(gridLon[lonIndex], gridLat[latIndex], col="pink", pch=19)
    points(gridLon[lonIndex], gridLat[latIndex], col="pink", pch=3)
    
    # Exstract monitors temperature array 
    temp <- t2m[lonIndex, latIndex,] 
    
    # Convert to degrees C 
    temp <- temp - 273.15 # 0C == 273.15 K
    
    # Now convert the temperature from C to f 
    temp <- temp * 9/5 + 32.0
    
    # Exstract monitors cloud cover fraction array 
    CC <- tcc[lonIndex, latIndex,] 
    
    # Where in ecmwfTimeModified do PMTimes land? 
    matchecmwfTimeToPM <- match(PMTime, ecmwfTimeModified) 
    
    T_df[,i] <- temp[matchecmwfTimeToPM]
    CC_df[,i] <- CC[matchecmwfTimeToPM]
    
    # Commented date assigment in lines below is the most effective way to make
    # sure that you are assigning the correct dates temperature data using
    # the match() function. This has caused major headaches before...
    ##T_df[,i] <- as.character(ecmwfTimeModified[matchecmwfTimeToPM])
    ##CC_df[,i] <- as.character(ecmwfTimeModified[matchecmwfTimeToPM])
    
  } # End of for loop looping through PM/ozone monitors
    
  # Place new variables into the workspace 
  workSpaceData[["T_df"]] <- T_df
  workSpaceData[["CC_df"]] <- CC_df

  print("assigned temperature and cloud data based on ecmwf grid")

  return(workSpaceData)
  
}


# ################################################################################  
# # Now use this ecmwf temperature data to create a clear T mask that ensures
# # smoke-free days are warmer than smoke-impacted days. 
# ################################################################################  
# createSmokeFreeTMask <- function(workSpaceData, 
#                                  TSdFactor = 1,
#                                  applySkyMask=FALSE, 
#                                  maxCloudCoverPercent=10){
# 
#   # Get the required data 
#   T_df            <- workSpaceData[["T_df"]]
#   smokeImpactMask <- workSpaceData[["smokeImpactMask"]]
#   
#   # set dimensions for new temperature mask
#   nMonitor    <- dim(T_df)[2]
#   meanSmokedT <- rep(NA, nMonitor)
#   sdSmokedT   <- rep(NA, nMonitor)
#   smokeFreeMask <- smokeImpactMask # Copying for proper dimensions and labels
#   
#   # Loop through each monitor, figuring out the temperature threshold based
#   # on arguments given to this function
#   for (i in 1:nMonitor){
#     
#     # clear out smokeImpactMask data 
#     smokeFreeMask[,i] <- FALSE # Assume FALSE until proven otherwise 
#     
#     # Which rows are smoke-impacted based on work so far? 
#     smokedRows  <- smokeImpactMask[,i]
#     smokedDaysT <- as.numeric(T_df[smokedRows,i])
#     
#     # Get the statistics on the smoke-impacted temperatures
#     meanSmokedT[i] <- mean(smokedDaysT, na.rm=TRUE)  
#     sdSmokedT[i]   <- sd(smokedDaysT, na.rm=TRUE)  
#     
#     # Figure out where the temperature is greater than smoky day average
#     TThresh <- meanSmokedT[i] + sdSmokedT[i] * TSdFactor
#     TCuttoffMask <- T_df[,i] >= TThresh
#     
#     
#     if(is.nan(TThresh) & sum(smokedRows)==0){
#       # There are no smoke impacted days, so all rows are smoke free. 
#       # We know this because TThresh is not a number and there are zero smoked
#       # rows. 
#       smokeFreeRows <- rep(TRUE, length(smokedRows))
#     } else {
#       # There are smoke impacted days so we need to choose smoke-free carefully
#       # Also, we want to be sure that these warm days are not also smoke-impacted!
#       # NOTE: smokedRows == TRUE where smoke-impacted. Use ! to change those to
#       # NOTE: FALSE and smoke-free days to TRUE
#       smokeFreeRows <- !smokedRows & TCuttoffMask
#       
#       # TODO: Could add PM mask as well to ensure that clear days are not high
#       # TODO: PM days. Require PM measurement for smokeFreeDays
#       
#     }
#     # Store the smokeFreeRows (days) information in TMask
#     smokeFreeMask[,i] <- smokeFreeRows
#     
#   }
#   
#   # Include the smokeFreeMask in the workspace data 
#   workSpaceData[["smokeFreeMask"]] <- smokeFreeMask
#   workSpaceData[["meanSmokedT"]] <- meanSmokedT
#   workSpaceData[["sdSmokedT"]] <- sdSmokedT
#   
#   # For testing purposes give this information back to console 
#   print(paste("The sum of smokeFreeMask after T-Control is:", 
#               sum(smokeFreeMask,na.rm=TRUE)))
#   
#   
#   # Now apply the skycondition mask if desired
#   if(applySkyMask){
#     
#     # Get the Cloud Cover Dataframe
#     CC_df <- workSpaceData[["CC_df"]] * 100 # to make %
#     
#     # Where are the skies more clear than specified %?
#     cloudFreeMask <- CC_df <= maxCloudCoverPercent
#     
#     # Add this cloudMask to the workspace
#     workSpaceData[["cloudFreeMask"]] <- cloudFreeMask
#     
#     # Modify smokeFreeMask based on this new cloud condition
#     smokeFreeMaskNew <- smokeFreeMask & cloudFreeMask
#     
#     # Overwrite the original smokeFreeMask 
#     workSpaceData[["smokeFreeMask"]] <- smokeFreeMaskNew
#     
#     
#     print(paste("The sum of smokeFreeMask after sky-control is:", 
#                 sum(smokeFreeMaskNew,na.rm=TRUE)))
#     print("If the later is not small than the former, you have a problem.")
#   }
#   
#   # Return the appended workSpaceData
#   return(workSpaceData)
# 
# 
# }





