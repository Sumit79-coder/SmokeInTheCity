

source("R/getWeatherData.R")

################################################################################
# function takes daily weather conditions and determines the ozone 
# relevent weather type for a given monitors time series
################################################################################
dailySkyType <- function(ASOS="KGMJ"){
  
  # Load the ASOS station's detailed weather data 
  detailedData <- get(load(paste0("wundergroundDetailed/", ASOS,".RData")))
  
  # First subset the temp data by photochemistry relevent hours
  # NOTE: times are supplied as local
  time <- as.POSIXlt(detailedData[,1], tz="UTC")
  photoChemistryMask <- time$hour >=12 & time$hour <= 17
  
  # Subset by relevant hours
  detailedDataSubset <- detailedData[photoChemistryMask,]
  time <- time[photoChemistryMask]
  
  # What are the unique days in this series? 
  timeString <- as.character(time)
  dateString <- str_sub(timeString, start = 1L, end = 10)
  uniqueDateString <- unique(dateString)
  dates <- as.POSIXlt(uniqueDateString, tz="UTC")
  
  # you now know how many rows will be in the summary dataframe, save this 
  # information and create the dataframe
  summary_df <- data.frame(date=dates, 
                           percentClear=rep(NA,length(dates)),
                           percentScattered=rep(NA,length(dates))
                           )
  
  # Loop through these days and assess the sky condition 
  for (i in 1:length(dates)){
   
    # Create the days mask 
    day  <- dates[i]
    jDay <- day$yday
    year <- day$year
    dayMask <- jDay == time$yday & year == time$year
    
    
    # What are the sky conditions?
    conditions <- detailedDataSubset[dayMask,]$Conditions
    
    #   [1] "Clear"                        "Scattered Clouds"             "Rain"                        
    #   [4] "Light Rain"                   "Light Drizzle"                "Drizzle"                     
    #   [7] "Mostly Cloudy"                "Unknown"                      "Overcast"                    
    #   [10] "Light Thunderstorms and Rain" "Heavy Thunderstorms and Rain" "Thunderstorms and Rain"      
    #   [13] "Mist"                         "Fog"                          "Haze"                        
    #   [16] "Thunderstorm"                 "Heavy Rain"                   "Heavy Drizzle"  
    
    
    # What % of the observations are "clear" | or Scattered Clouds? | haze? 
    percentClear     <- sum(conditions == "Clear") / length(conditions) * 100
    percentScattered <- sum(conditions == "Scattered Clouds") / length(conditions) * 100
    
    #percentScatted <- sum(conditions == "Scattered Clouds") / length(conditions) * 100
    
    # relevant daily value 
    summary_df[i,2] <- percentClear
    summary_df[i,3] <- percentScattered
    
    
  } # end of day loop 
  
  skySummary <- summary_df 
  save(skySummary, file=paste0("wundergroundSkySummary/",ASOS,".RData") )
  
  
}

# Run dailySkyType for every ASOS station 
makeDailySkyTypes <- function(){
  # Which ones of these do we have detailed data for? 
  files <- list.files("wundergroundDetailed/")
  downloaded <- str_replace(files, ".RData", "")
  
  for(ASOS in downloaded){
    try(dailySkyType(ASOS=ASOS), silent=TRUE)
  }
}


assignDetailedWeatherData <- function(dataSource="smokeMask_HMS+PM_Gravimentric.RData",
                                      maximumDistance=45){

  
	# Load the chosen data packet, these are the monitors we want to assign detailed weather 
	# data to 
	load(paste0("analysisData/",dataSource))

  # create mask
  ozone_df <- workSpaceData[["Ozone_df"]]
  workSpaceDates <- as.POSIXct(rownames(ozone_df), tz="UTC")
  
  # Copy this dataframe as a skycondition masking product
  skyCondition_df <- ozone_df
  
  monitorLon <- workSpaceData[["lon"]]
	monitorLat <- workSpaceData[["lat"]]
  
  # Pull out the ozone_df to copy for size of sky data 
  sky_df <- workSpaceData[["Ozone_df"]]
  nMonitor <- dim(sky_df)[2]
  
  # Define the dats for these data
	monitorTime <- as.POSIXct(rownames(sky_df), tz="UTC")
	
  
  # get metadata for ASOS monitors 
	mdf <- getWundergroundDataMeta()
	ASOSNames <- mdf$airportCode
	
	# Which ones of these do we have detailed data for? 
	files <- list.files("wundergroundSkySummary/")
	downloaded <- str_replace(files, ".RData", "")
	
	# where overlap is NA, these are monitors we dont have yet 
	overlap <- match(downloaded, ASOSNames)
	
	# Subset by the ASOS monitors where we have downloaded data 
	mdf_subset <- mdf[overlap,]
  ASOSLon  <- mdf_subset$Lon
  ASOSLat <- mdf_subset$Lat
	ASOSNames <- mdf_subset$airportCode
  
  # Make nice points
	monitorLocation <- cbind(monitorLon, monitorLat)
	ASOSLocation    <- cbind(ASOSLon, ASOSLat) 
  
  # Start looping through the monitors looking for sky data, and making sky
  # assements! 
  noSky <- 0
  for (i in 1:nMonitor){
    
  	# Calculate distance to all ASOS
  	distance <- distHaversine( monitorLocation[i,],  ASOSLocation) / 1000 # km great circle distance 
  	nearestOrder <- order(distance)
    
  	# Arrange relavent variables by distance to the monitor 
  	sortedDistance <- distance[nearestOrder] # for sanity, 
    distanceMask   <- sortedDistance <= maximumDistance
    
  	sortedASOSNames <- ASOSNames[nearestOrder][distanceMask]
    sortedASOSLocation <- ASOSLocation[nearestOrder,][distanceMask]
    
    
    # TODO: implement while() functionality to ensure the ASOS station with the
    # TODO: best data is used 
    
    # identify the closest ASOS station 
    closestASOS <- sortedASOSNames[1]
  	
  	# Open the detailed datafile of the nearby ASOS station
    if(is.na(closestASOS)){
      noSky <- noSky + 1
      skyCondition_df[,i] <- NA
      
    } else{
      # These is an ASOS station close enough
      
      skySummary <- get(load(paste0("wundergroundSkySummary/",closestASOS,".RData")))
      skyDates   <- skySummary$date
      
      # Subset by the workspace data dates
      subsetDates <- match(workSpaceDates, skyDates)
      skySummary_subset <- skySummary[subsetDates, ]
      skyCondition_df[,i] <- skySummary_subset$percentClear + skySummary_subset$percentScattered
      
    }
    
  } # End ozone monitor loop 
  print(noSky)
  # Once we have found and assigned sky data fort all ozone monitors save this
  # to the working packet 
  workSpaceData[["skyCondition_df"]] <- skyCondition_df
  
  saveDir    <- "analysisData/"
  dataPacket <- paste0("skyMask_",dataSource)
  saveFile   <- paste0(saveDir, dataPacket)
  
  save(workSpaceData,file=saveFile)
  

}

assignDetailedWeatherData(dataSource="smokeMask_HMS+PM_Gravimentric.RData",
                          maximumDistance=45)

