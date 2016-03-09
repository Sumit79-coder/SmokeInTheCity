# getWeatherData.R

# This script will be used to explore the feasability of getting temperature 
# data from airport weather stations around the U.S. to allow for a temperature
# cutoff to be done in analysis.

# Load required packages
library(maps)
library(geosphere) # For calculating distances between lat lon pairs
library(weatherData)
library(stringr)
library(fields)


# UPDATE: 6/23/2015
# This script can now download wunderground airport daily summary temperature 
# data using getWundergroundDataMeta(). It saves the data as an array in 
# /wundergroundData. 

################################################################################
# Get wunderground data based on files stored on this machine 
################################################################################
getWundergroundDataMeta <- function(){

  # Load USA airport weather station metadata into the workspace 
  data(USAirportWeatherStations)
  mdf <- USAirportWeatherStations # weather station metadata 
  
  
  # KSEA is Sea-tac
  
  # Figure out which weather stations I have downloaded data for 
  files <- list.files('wundergroundDataNew/')
  airportCodeLocal <- str_sub(files, 1,4)
  metadataKeep     <- match(airportCodeLocal, mdf$airportCode)
  # NOTE: There should be no NA values in metadataKeep because 
  # NOTE: airportCodeLocal is a subset of mdf$airportCode
  
  mdf_subset       <- mdf[metadataKeep, ] 
  
  return(mdf_subset)

}


################################################################################
# Now that the metadata is subset by what weather stations we have data for we 
# can start to find weather data for Ozone monitors.
# NOTE: The only goal of this function is to assign wunderground T data
################################################################################
assignWundergroundTToMonitor <- function(workSpaceData=workSpaceData,
                                         maxDistance=40
                                         ){
  
  # Get relavent workSpaceData tp assign tempearture
  Ozone_df <- workSpaceData[["Ozone_df"]]     # to copy for T_df
  Hybrid_mdf <- workSpaceData[["Hybrid_mdf"]] # for location information
  nMonitors <- dim(Ozone_df)[2]
  
  # These lons and lats in the workspace belong to PM/ozone monitors
  O3Lon <- Hybrid_mdf$Longitude
  O3Lat <- Hybrid_mdf$Latitude
  monitorLocation <- cbind(O3Lon, O3Lat)
  
  # Place PM monitor time as MST to make clear that these data are for North 
  # america time dates
  O3Time <- as.POSIXct(rownames(Ozone_df), tz="MST")
  
  # get wunderground airport name and metadata information
  mdf <- getWundergroundDataMeta()
  weatherStations <- mdf$airportCode
  wLon<- mdf$Lon
  wLat<- mdf$Lat
  weatherLocation <- cbind(wLon, wLat)
  T_df <- Ozone_df # Values will be replaced, copying for dimensions and labels
  
  
  # Plot the monitor locations, make sure chosen airports are actually nearby
  map('state')
  points(O3Lon,O3Lat, pch=19)
  
  
  # Find the best weather station for each PM/O3 monitoring location 
  for (i in 1:nMonitors){ # nMonitors
    
    # Clear out the dataframe of PM values resulting from coppying
    T_df[,i] <- NA
    
    # Calculate monitor (great circle) distance from each airport 
    m_per_km <- 1000
    distance_km <- distHaversine(monitorLocation[i,],  weatherLocation) / m_per_km 
    
    # Which airports are close enough?
    distanceMask <- distance_km <= maxDistance
    nTStations <- sum(distanceMask)
    
    # Find the best fit based on proximity and data avialability 
    
    if(nTStations == 1){
      
      # There is only one station that is close enough, use this data
      nameOfClosestAirport <- weatherStations[distanceMask]
      loadFile <- paste0('wundergroundDataNew/',nameOfClosestAirport,".RData")
      w_df <- get(load(loadFile))
      rm(list=nameOfClosestAirport)
      
      haveData <- TRUE
      nameOfAirportUsed <- nameOfClosestAirport
      
    } else if(nTStations > 1){
      
      # We have a few options for airport data, lets use the station
      # with the most data
      nameOfCloseAirports <- weatherStations[distanceMask]
      
      # Where we will figure out how many days of data each monitor has
      dataDays <- rep(NA, nTStations)
      for (s in 1:nTStations){
        
        loadFile <- paste0('wundergroundDataNew/',nameOfCloseAirports[s],".RData")
        w_df <- get(load(loadFile))
        rm(list=nameOfCloseAirports[s])
        dataDays[s] <- dim(w_df)[1]
        
      }
      
      # Do they all have the same number of days measured? If so pick the 
      # airport that is closest to the monitor 
      nUnique <- length(unique(dataDays))
      if(nUnique == 1){
        
        # They all have the same amount of data so use the closer one
        mostMeasuredIndexAndClosest <- which.min(distance_km[distanceMask])
        nameOfBestAirport <- nameOfCloseAirports[mostMeasuredIndexAndClosest]
        
      } else {
        
        # Someone has more data then the other locations
        # Use the airport code who has the most days temperature measured
        mostMeasuredIndex <- which.max(dataDays)
        nameOfBestAirport <- nameOfCloseAirports[mostMeasuredIndex]
        
      }


      
      # Load and use this airport data
      loadFile <- paste0('wundergroundDataNew/',nameOfBestAirport,".RData")
      w_df <- get(load(loadFile))
      rm(list=nameOfBestAirport)
      
      haveData <- TRUE
      nameOfAirportUsed <- nameOfBestAirport
      
    } else if(nTStations == 0){
      
      # No airport data for this monitoring location
      haveData <- FALSE
      
    } # End of finding the best available airport data
    
    
    
    # Assign data if it exists
    if(haveData == TRUE){
      
      # Get the times associated with weather measurements 
      weatherTime <- as.POSIXct(w_df$date.local, tz="MST")
      
      # Where in the weather data do our measured PM and Ozone data fall?
      # These are the locations where we want weather data for this monitors
      # measurements
      placeWeatherDataHere <- match(O3Time, weatherTime)
      
      # NOTE: There could be NA values in this matched sequence because it is
      # NOTE: possible that there is no airport temperature data for the day 
      # NOTE: that we have monitoring data 
      
      # Always perfrom sanity check by testing your placement logic with date 
      # string matching
      shouldMatchO3Time <- as.POSIXct(w_df$date.local[placeWeatherDataHere], tz="MST")
      
      if(sum(is.na(placeWeatherDataHere)) == 0){
        # yay no missing airport data for any days in O3Time!
        # Assign this monitoring locations airport data
        test <- shouldMatchO3Time == O3Time
        
        # Be super sure there is a temperature value for every monitor value 
        if(sum(!test) == 0){
          T_df[, i] <- w_df$Mean.TemperatureF[placeWeatherDataHere]
          #T_df[, i] <- as.character(weatherTime[placeWeatherDataHere])
        }
        

      } else {
        
        # there are missing days in airport time record, handle with care
        
        # The places where placeWeatherDataHere are rows in O3Time where
        # we do not have weatherTime. So we want to not assign data to those
        # locations. 
        haveWeatherRows <- !is.na(placeWeatherDataHere) 
        
        # We then want to make sure to get rid of the NA index in 
        # placeWeatherDataHere
        getThisWeatherData <- placeWeatherDataHere[haveWeatherRows]
        
        # Test this making sure the times match using these masks
        test <- weatherTime[getThisWeatherData] == O3Time[haveWeatherRows]
        if(! (sum(test) < length(test)) ){
          # Place the data 
          T_df[haveWeatherRows, i] <- w_df$Mean.TemperatureF[getThisWeatherData]
          #T_df[haveWeatherRows, i] <- as.character(weatherTime[getThisWeatherData])
        }
        # Else, remains all NA
       
        
        
      }
  

      
      
      # Plot the data you have just added to the dataframe, is it close?
      points(O3Lon[i], O3Lat[i], col="black", pch=19)
      
      # metaMask
      mask <- nameOfAirportUsed == weatherStations
      points(wLon[mask], wLat[mask], col="pink", pch=19)
      
      
    } # End of assigning data
    
  } # End of looping through PM/Ozone monitors
  
  # Append the workSpace data and move along 
  workSpaceData[["T_df"]] <- T_df
  
  return(workSpaceData)
  
  print("Assigned temperature and cloud data using wunderground airports")

}
################################################################################




