# assignReanalysisTDataToMonitor.R 


################################################################################  
#--------------------------------DEPRICATED-------------------------------------
# THIS FILE HAS BEEN REPLACED BY AssignECMWFTDataToMonitor.R. If you are looking
# for a working copy of some of the functionaliy written in this script please
# see AssignECMWFTDataToMonitor.R.
################################################################################  


library(ncdf4)
################################################################################  
# Assigns gridded temperature data to PM monitors passed here 
getPMWeatherData <- function(workSpaceData=workSpaceData, 
                             sanityCheck=FALSE){
  
  # read in the nc data 
  fileName <- paste0("ecmwfData/2m-T-00UTC_ecmwf_May-Sep.nc")
  
  # Get nc data and dims 
  nc          <- nc_open(fileName) # data only contains June,July,August
  gridLon     <- ncvar_get(nc,"longitude")
  gridLat     <- ncvar_get(nc,"latitude")

  
  ##############################################################################
  # Handle ecmwf data spatially.
  # NOTE: different lon that I am used to work with. Degrees easting only
  ##############################################################################
  
  # The following plot ensures that these degrees E and N cover the contiguous
  # united states
  minEastLon <- 230
  maxEastLon <- 305 # heading east remember
  minNorthLat <- 24
  maxNorthLat <- 51
  
  lonMask <- gridLon >= minEastLon & gridLon <= maxEastLon
  latMask <- gridLat >= minNorthLat  & gridLat <= maxNorthLat
  
  T2m <- ncvar_get(nc,"t2m")    # [Longitude, Latitude, time]
  T2m <- T2m[lonMask,latMask,]  # Subset by US lon + lat but keep all time

  gridLon <- gridLon[lonMask]
  gridLat <- gridLat[latMask]
  
  # DOUBLE CHECK THIS LOGIC TOO. RENAME GRID LON so that new expression is clear
  # convert to same lon system as monitors
  gridLon <- gridLon - 360
  
  # Sanity plot of ecmwf data and the way I convert the grid 
  map("state")
  points(gridLon, rep(40, length(gridLon)), pch=19)
  points(rep(-100, length(gridLat)), gridLat, pch=19)
  title("US + lon coords at 40N and lat at -100W")
  
  ##############################################################################
  # Organize seconds data to something useful 
  ##############################################################################
  hours     <- ncvar_get(nc,"time")  # units: hours since 1900-01-01 00:00:0.0"
  seconds   <- hours * 60^2
  ecmwfTime <- as.POSIXct(seconds,  origin="1900-01-01 00:00:0.0",tz="UTC")
  # NOTE: This 00Z time is 6:00 PM MDT 8PM EDT and 5 PM PDT
  # NOTE: of the PREVIOUS day. 
  # 00Z is the date of the day just starting, still previous date in US at this
  # hour. 
  # http://scc-ares-races.org/generalinfo/utcchart.html

  # Take a day away to make this time more useful to the PM and ozone data
  # which is in US time. That daily data will most meaningfully be matched with
  # evenning temperature data of the 00Z data. 
  secondsInDay <- 24 * 60^2
  ecmwfTimeModified <- ecmwfTime - secondsInDay
  
  # Load requested data packet
  Hybrid_mdf <- workSpaceData[["Hybrid_mdf"]]
  PM_df      <- workSpaceData[["PM_df"]]  
  lon        <- workSpaceData[["lon"]]
  lat        <- workSpaceData[["lat"]]
  T_df       <- PM_df # copycat will be replaced with T values from ecmwf
  nMonitors  <- length(lon)  

  # These dates are made from local times in the US. which are a day behind 00Z
  # I am placing them in UTC time zone so that it will be possible to match
  # the modified ecmwf time series. In reality there is no actual time of day
  # associated with this daily data. 
  measuredDataDate <- rownames(T_df)
  PMTime <- as.POSIXct(measuredDataDate, tz="UTC")
    
  for (i in 1:nMonitors){
    
    # TODO: Probably add a date forloop for ultimate clarity 
    
    # Clear out the dataframe of PM values resulting from coppying
    T_df[,i] <- NA
    
    # Find the grid box the monitor falls inside of, create a mask 
    lonIndex <- which.min(abs(gridLon - lon[i]))
    latIndex <- which.min(abs(gridLat - lat[i]))
    
    # Exstract monitors temperature array and convert to C 
    temp <- T2m[lonIndex, latIndex,] - 272.15
    
    # Now convert the temperature from C to f 
    temp <- temp * 9/5 + 32
    
    # Where in ecmwfTime do PMTimes land? (will be missing last days of Sept)
    matchTime <- match(PMTime, ecmwfTimeModified) 
    matchTime <- matchTime[!is.na(matchTime)] # Get rid of NA because you can't
                                              # place them. 
    
    # matchTime is not the same length as PMTime or T_df[,1] anymore because
    # there are last days of september missing
    # TODO: The proper fix is to get October data and use oct 1 00Z as the
    # TODO: last evenning in september data. There will be no NA in match when
    # TODO: this is done
    
    # What rows of the T_df (PMTime) match what we have from ecmwfTime
    
    rowMatch <- (match(ecmwfTimeModified[matchTime], PMTime))
    T_df[rowMatch,i] <- temp[matchTime]
  
  
    
  } # End of for loop looping through PM/ozone monitors
    
  # Replace dummy T_df with this assigned temperature data
  workSpaceData[["T_df"]] <- T_df

  print("assigned temperature data based on ecmwf grid")

  return(workSpaceData)
  
}
################################################################################  


################################################################################  
# Now use this ecmwf temperature data to create a clear T mask that ensures
# smoke-free days are warmer than smoke-impacted days. 
createClearSkyMask <- function(workSpaceData, 
                             PMThresh=100,
                             applySkyMask=FALSE,
                             clearSkyThresh=100){


  T_df      <- workSpaceData[["T_df"]]
  smokeMask <- workSpaceData[["smokeMask"]]
  
  # set dimensions for new mask
  nMonitor <- dim(T_df)[2]
  meanSmokedT <- rep(NA, nMonitor)
  sdSmokedT   <- rep(NA, nMonitor)
  greaterThanMeanSmokeTMask <- smokeMask # Copying for proper dimensions 
  
  # Loop through each monitor 
  for (i in 1:nMonitor){
    
    greaterThanMeanSmokeTMask[,i] <- NA # clear out smokeMask data 
    
    smokedRows <- smokeMask[,i]
    meanSmokedT[i] <- as.numeric(  mean(T_df[smokedRows,i], na.rm=TRUE)  )
    sdSmokedT[i]   <- as.numeric(  sd(T_df[smokedRows,i], na.rm=TRUE)  )
    
    
    # Figure out where the temperature is greater than smoky day average
    # for days with NO smoke
    # TODO: Experiment with T TCuttoff untill T distributions look alike
    TThresh <- (meanSmokedT[i] + sdSmokedT[i])
    TCuttoff <- T_df[,i] >= TThresh
    greaterThanMeanSmokeTMask[,i] <- TCuttoff  & smokeMask[,i] == FALSE
    # For clear days! 
  }
  
  
  print(paste("The original sum is", sum(greaterThanMeanSmokeTMask,na.rm=TRUE)))
  
  # Make sure sky % threshold is met! 
  if(applySkyMask==TRUE){
    
    # This dataframe already matches ozone and PM in demension
    skyCondition_df <- workSpaceData[["skyCondition_df"]]
    skyMask <- skyCondition_df >= clearSkyThresh # (clear sky) + (scattered sky) % 
    
    
#     print(paste("The dim of clearSkyMask:", dim(clearSkyMask)))
#     print(paste("The dim of greaterThanMeanSmokeTMask", dim(greaterThanMeanSmokeTMask)))
    
    workSpaceData[["Tmask"]] <- greaterThanMeanSmokeTMask
    
    # Make sure both temperature and sky condition are true
    greaterThanMeanSmokeTMask <- skyMask & greaterThanMeanSmokeTMask
    
  }
  
  print(paste("post sky cover sum is", sum(greaterThanMeanSmokeTMask, na.rm=TRUE)))
  
  
  # Append greaterThanMeanSmokeTMask to workSpaceData
  workSpaceData[["greaterThanMeanSmokeTMask"]] <- greaterThanMeanSmokeTMask
  return(workSpaceData)

  print("Made it through createClearSkyMask()")

}
################################################################################  

# This part of the function is concerning using EPA temperature data 
if(temperatureData=="EPA"){
  
  print("Using EPA temperature data")
  
  # TODO: This is where I may potentially be incorperating outside temperature
  # TODO: data. The idea is I will assign daily mean temperature to stations
  # TODO: if they are located close enough to the monitor.
  
  T_df <- get(load("timeSeriesData/mergedYears/Arithmetic.Mean/TEMP_allYears.RData"))
  rm("TEMP_allYears")
  
  TTime <- as.POSIXct(rownames(T_df))
  TempTimeOverlap <- match(time,TTime)
  
  # Subset Tdf by the time we are interested in
  T_df <- T_df[TempTimeOverlap, ]
  
  # Now subset by the stations
  TStations <- colnames(T_df) 
  
  TempStationOverlap <- match(hybridStations, TStations)
  TempStationOverlap <- TempStationOverlap[!is.na(TempStationOverlap)]
  
  T_df <- T_df[,TempStationOverlap] 
  TStations <- colnames(T_df)
  
  Ozone_df <- Ozone_df[,match(TStations, hybridStations)]
  PM_df    <- PM_df[,match(TStations,hybridStations)]
  
  hybridStations <- colnames(PM_df) # some 289
  print('subsetting PM and ozone monitors by temperature data')
  
} else{