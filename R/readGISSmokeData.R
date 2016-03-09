# readGISSmokeData.R

################################################################################
# This script takes a specified PM file and creates a smoke mask based on HMS 
# and PM monitor observations. 
################################################################################

# load needed libraries
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)
library(maps)


################################################################################
# ARGUMENTS: REQUIRES HUMAN INTERACTION
################################################################################

# PMSpecies:       The type of PM used. Options are FRM and non_FRM
# Sample.Duration: What Sample duration do you want used? 1 hr or 24 hour? 
# Stat:            Do you want to use the maximum value for the sample duration 
#                  or the mean value. 
# PMThreshType: Options are "allSummers" | "individualSummer". The original 
#            published work uses an all summer mean + sd threshold for PM. 
#            "individualSummer" sets a new PM threshold for each summer of PM
#            data. 
# OzoneFile: The ozone data to be used to calculate MDA8 exceedances and the 
#            average differences between smoke-impacted and smoke-free O3 values
#            Only one option for MDA8 (maximum 8-hr average) in 
#            timeSeries/mergedYears
# minMonrh:  the earliest month in the year used for analysis
# maxMonth:  the latest month in the year used for analysis

PMSpecies <- "PM2.5_non_FRM_Mass" # PM2.5_non_FRM_Mass | PM2.5_FRM_FEM_Mass 
Sample.Duration <- "24"
stat <- "Arithmetic.Mean"


# Choose what type of PMThresh to use 
PMThreshType <- "allSummers" # "allSummers" | "individualSummer"

# option Max of MDA8 value only 
OzoneFile <- "Ozone_X1st.Max.Value_8hr_2005_2014" 
minMonth  <- 5 # May
maxMonth  <- 9 # Sept

HMSTest <- TRUE # Plots smoke and hms monitors, for testing only. This shows you
                # if the correct monitors are flagged with HMS polygons 
################################################################################
# Function creates a mask for station days that see smoke based on HMS GIS data 
# plus anomously high PM25 concentrations creates a work environment for making
# smoked vs. clear ozone distributions 
################################################################################
createHMSPM25SmokeMask <- function(PMSpecies="species",
                                   Sample.Duration="24",
                                   stat="stat",
                                   PMThreshType = "allSummers|individualSummer",
                                   PMMetaFile = "file.something",
                                   OzoneFile = "file.something",
                                   maxMonth=9, 
                                   minMonth=5,
                                   HMSTest=FALSE
                                   ){
  
  ##############################################################################
  # First write out what files you will be using for PM based on arguments
  ##############################################################################
  PMFile     <- paste0(PMSpecies,"_",stat,"_", Sample.Duration,"hr_2005_2014")
  PMMetaFile <- paste0(PMSpecies,"_",stat,"_", Sample.Duration,"hr_metadata_2005_2014")
  
  
  ##############################################################################
  # Handle HMS data 
  ##############################################################################
  
  # Location of the HMS GIS smoke files in project.
  # DataSource: ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/
  HMSDataDir <- "FireData/HMS_smoke/"
  smokeFiles <- list.files(HMSDataDir)
  
  # Only look at days that we have smoke data for. Determined by preference and
  # HMS dataset. 
  smokeDayString <- unique(str_sub(smokeFiles,5,12))
  smokeTime <- as.POSIXct(smokeDayString, format="%Y%m%d", tz="MST")
  # NOTE: no real time associated with this, making MST for consistency
  
  # We want to subset the time array by our specified max and min month,
  # this is most easily accomplished by using a POSIXlt object
  smokeTimeLT <- as.POSIXlt(smokeTime)
  month       <- smokeTimeLT$mon + 1 # because jan == 0
  monthMask   <- month >= minMonth & month <= maxMonth 
  
  # Apply the monthMask to further subset data 
  smokeTime <- smokeTime[monthMask]
  
  ##############################################################################
  # Load PM data [day , station]
  ##############################################################################  
  timeSeriesDataDir <- "timeSeries/mergedYears/"
  PMFileToLoad <- paste0(timeSeriesDataDir, PMFile,".RData")
  print(paste("The PM file being used is:",PMFile))
  PM_df <- get(load(PMFileToLoad))
  rm(list=PMFile) # clears up working memory 
  
  # Get PM monitor names (instrument specific)
  PMMonitors <- colnames(PM_df) # all columns of PM_df are unique 02/17/2016
  
  # Get PM station names (location specific) first 9 digits of ID
  PMStations <- str_sub(PMMonitors,1 ,9 ) # NOTE: These are NOT all unique
                                          # NOTE: many of these co-located
  
  ##############################################################################
  # Load ozone data [day, station]
  ##############################################################################
  OzoneFileToLoad <- paste0(timeSeriesDataDir, OzoneFile,".RData")
  print(paste("The Ozone file being used is:",OzoneFile))
  Ozone_df <- get(load(OzoneFileToLoad))
  rm(list=OzoneFile)                      # clears up working memory 
  
  # Get the names of the monitors and the stations they are at
  OzoneMonitors <- colnames(Ozone_df) # each unique
  OzoneStations <- str_sub(OzoneMonitors,1 ,9 ) # not unique, but fewer co-located
                                                # than PM monitors 
  ##############################################################################
  
  ##############################################################################
  # Which Ozone monitors have PM measurements too? Use site ID for co-location
  ##############################################################################
  nOzoneStations <- length(OzoneStations)
  hasMatch <- rep(FALSE, nOzoneStations) # Start with the assumption of no match
  for (i in 1:nOzoneStations){
    
    # Does this OzoneStation match any of the PMStations? 
    mask <- OzoneStations[i] == PMStations
    
    # If yes, then this OzoneStation has a match
    if(sum(mask, na.rm=TRUE) > 0){
      hasMatch[i] <- TRUE
    }
    
  } # End of checking for PMStations at each OzoneStations
  
  # Monitoring stations with both ozone and PM measurements 
  hybridStations <- sort(OzoneStations[hasMatch])
  hybridOzoneMonitors <- sort(OzoneMonitors[hasMatch])
  
  # Subset both dataframes to include only hybrid stations 
  Ozone_df <- Ozone_df[, match(hybridOzoneMonitors, OzoneMonitors)] # all matches unique
  OzoneMonitors <- colnames(Ozone_df) # these are all unique
  OzoneStations <- str_sub(OzoneMonitors,1 ,9 )
  
  # LOOP through ozone stations, find all PM matches, then find choose
  # the best PM monitor at this station.
  PMTime <- as.POSIXct(rownames(PM_df), tz="MST") # continious 2005-2014
  nDays <- length(PMTime)
  
  # Create an array where you will store the ID of the best PM monitor for each
  # hybrid ozone monitor. 
  nHybrids <- length(hybridStations)
  PMMonitorKeeperID <- rep(FALSE, nHybrids)
  
  # Create an array to store the station IDs that have a tricky availability
  # situation
  trickyLocations <- as.character()
  
  for(i in 1:nHybrids){
    
    # Set the hybrid station we are searching for best PM data for
    station      <- hybridStations[i]
    PMColumnMask <- station == PMStations
    PM_subset    <- PM_df[, PMColumnMask]
    
    # Is this still multi-dimension array?
    dimTest <- is.null(dim(PM_subset))
    
    # Choose the PM monitor with the most data. This makes the
    # statistics the least messy and not sketchy. 
    PMMonitorsAtSite <- sum(PMColumnMask)
    if(PMMonitorsAtSite > 1){
  
      percentAvailable <- rep(NA, PMMonitorsAtSite)
      averageTimeDiff  <- rep(NA, PMMonitorsAtSite)
      
      for (j in 1:PMMonitorsAtSite){
        
        # Get the values
        if(dimTest){
          # If there is only one monitor, use it
          v <- as.numeric(PM_subset)
        } else { 
          # Select a column of the subset (a monitor)
          v <- as.numeric(PM_subset[,j])
        }
        
        # How many are NaN or NA? Aka not available not real values
        missingMask <- is.na(v) # NOTE: is.na(c(NaN,NA,1)) == TRUE  TRUE FALSE
        percentAvailable[j] <- sum(!missingMask) / length(v) * 100 # save length as PMTime
        
        # Also consider average diff
        monitorsMeasuredDates <- PMTime[!missingMask]
        averageTimeDiff[j] <- as.numeric(mean(diff(monitorsMeasuredDates))) # days
        
      }
      
      # TODO: Maybe you find a way to choose?
      bestAvailability <- which.max(percentAvailable)
      mostContinuous   <- which.min(averageTimeDiff)
      
      # If these two metrics are not the same, a tradeoff may be needed
      if(!bestAvailability == mostContinuous){
        #print(paste('best availability and most continious do not match',station,i))
        trickyLocations <- append(trickyLocations, station)
      }
      
      # In the end we will go with the best availability 
      PMMonitorKeeperID[i] <- colnames(PM_subset)[bestAvailability]
      
    } else{ # This is the only monitor so you better use its data
      PMMonitorKeeperID[i] <- PMMonitors[station == PMStations]
    }
    
  } # end of looping through hybrids looking for best PM
  
  # Find where these PMMonitorKeeperIDs fall in the PM dataframe and subset
  PMMonitorKeeperIDSorted <- sort(PMMonitorKeeperID)
  PM_df   <- PM_df[, match(PMMonitorKeeperIDSorted,PMMonitors)]
  
  # NOTE: Since some of the Ozone measurements are co-located some of these
  # NOTE: PM monitors will be assigned twice, so the columns of PM_df will
  # NOTE: not be unique. 
  
  # Now we need to re-define station names again, because column names of these
  # are actually monitor (instrument) specific 
  PMMonitors <- colnames(PM_df)
  PMStations <- str_sub(PMMonitors,1 ,9 )
  
  
  # Are the columns of PM and Ozone df aligned?
  if(any(!PMStations == OzoneStations)){
    stop("The columns of PM and Ozone data are not aligned")
  }

  ##############################################################################
  # TODO: Consider only using PM monitors that make daily observtions, as of now
  # TODO: gap in PM data series likely to throw some smoke-impacted days into
  # TODO: smoke-free distributions. This will give overall conservative results. 
  ##############################################################################
  
  ##############################################################################
  # Subset PM data by the dates we have smoke data for 
  ##############################################################################
  PMTime <- as.POSIXct(rownames(PM_df), tz="MST") # continious 2005-2014
  matchSmokePMTime <- match(smokeTime, PMTime)
  naMask <- is.na(matchSmokePMTime) 
  if(sum(naMask) > 0){
    stop("Stop, for some reason PMTime is missing a possible smokeTime")
  }
  
  # This time, will end up being all days investigated for this data packet
  time <- PMTime[matchSmokePMTime]
  
  # Subset the PM_df to include only dates where we have HMS data 
  PM_df <- PM_df[matchSmokePMTime, ]
  nDays <- length(time)
  
  # Final PMTime sanity check, if any of the following is false, there is a 
  # problem with the time matching logic 
  testPMTimeMatch <- as.POSIXct(rownames(PM_df), tz="MST") == time
  if(any(testPMTimeMatch == FALSE)){
    stop("There is a problem with how you subset the PM_df")
  }

  ##############################################################################
  # Force Ozone_df to match this time sequence 
  ##############################################################################
  OzoneTime <- as.POSIXct(rownames(Ozone_df), tz="MST")
  Ozone_df  <- Ozone_df[match(time, OzoneTime), ]
  
  testPO3TimeMatch <- as.POSIXct(rownames(Ozone_df), tz="MST") == time
  if(any(testPO3TimeMatch == FALSE)){
    stop("There is a problem with how you subset the PM_df")
  }
  
  ##############################################################################
  # Sanity Check: Make sure the dataframes are the same size and have matching
  # Stations 
  ##############################################################################
  
  if(dim(PM_df)[1] == dim(Ozone_df)[1] & dim(PM_df)[2] == dim(Ozone_df)[2]){
    print("Dimensions of ozone data and pm data are the same. Looking good.")
    print(paste("Dimension of both are:", dim(Ozone_df)[1], "by", dim(Ozone_df)[2]))
  } else{
    stop("Ozone and PM data should be of the same dimension. Try again.")
  }
  
  if(any(!PMStations == OzoneStations)){
    stop("The two dataframes columns are not the same stations.")
  }
  
  ##############################################################################
  # Get Station metadata for ozone and PM 
  ##############################################################################
  metaDataDir <- "timeSeries/mergedYearsMetadata/"
  OzoneMetaFileBase <- "Ozone_X1st.Max.Value_8hr_metadata_2005_2014"
  Ozone_mdf <- get(load(paste0(metaDataDir, OzoneMetaFileBase,".RData")))
  rm(list=OzoneMetaFileBase)
  
  # Get ozone monitors metadata, lat lon, better name of location
  matchMeta <- match(OzoneMonitors, Ozone_mdf$ID)
  Hybrid_mdf <- Ozone_mdf[matchMeta,]
  lat       <- Hybrid_mdf$Latitude
  lon       <- Hybrid_mdf$Longitude
  city      <- Hybrid_mdf$CBSA.Name
  cityName  <- Hybrid_mdf$City.Name
  
  
  # Load the PM metadata
  PMMetaFileBase <- PMMetaFile
  PM_mdf <- get(load(paste0(metaDataDir, PMMetaFileBase,".RData")))
  rm(list=PMMetaFileBase)
  
  matchPMMeta <- match(PMMonitors, PM_mdf$ID)
  PM_mdf_subset <- PM_mdf[matchPMMeta,]
  
  # NOTE: I plot the locations of both and they are all overlapping! 
  
  # The station names of these two different metadata sources should be the same
  stationTest <- str_sub(PM_mdf_subset$ID,1,9) == str_sub(Hybrid_mdf$ID,1,9)
  countyTest  <- PM_mdf_subset$County.Name == Hybrid_mdf$County.Name
  ParameterTest  <- PM_mdf_subset$Parameter.Code == Hybrid_mdf$Parameter.Code
  
  if(any(!c(stationTest,countyTest))){
    stop("metadata not matching, double check code. ")
  } else if( any(ParameterTest)){
    stop("The parameters for these two different mdf should not be the same")
  }

  ##############################################################################
  # Create spatial points dataframe of hybrid monitor locations
  ##############################################################################
  coords    <- cbind(lon, lat)
  locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))
  
#   
#   PMCoords <- cbind(PM_mdf_subset$Longitude, PM_mdf_subset$Latitude)
#   PMLocations <- SpatialPoints(PMCoords, proj4string=CRS(as.character(NA)))
  

  ##############################################################################
  # Find each stations mean PM and sd in order to establish threshold
  # Also create a HighPMMask for each of these methods 
  ##############################################################################

  nStations <- length(hybridStations)
  HighPMMask <- array(data=FALSE, dim=c(nDays, nStations))
  
  if(PMThreshType == 'allSummers'){
    
    stationPMMean <- as.numeric(apply(PM_df, 2, mean, na.rm=TRUE))
    stationPMSd   <- as.numeric(apply(PM_df, 2, sd, na.rm=TRUE))
    sdFactor      <- 1 # This is the number of sd over mean required to say smoke 
    stationPMTheshold <- stationPMMean + stationPMSd * sdFactor
    
    # Loop through everyday, looking for where each monitor exceeds its thresh
    for (i in 1:length(time)){
      PMOverThresh <- PM_df[i, ] >= stationPMTheshold
      HighPMMask[i,] <- PMOverThresh
    }
    
    
  } else { #"individualSummer"
    
    # Do the same thing, only for each year's individual summers
  
    stationPMTheshold <- array(data=NA, dim=c(nDays, nStations))
    colnames(stationPMTheshold) <- colnames(PM_df)
    rownames(stationPMTheshold) <- as.character(time)
    years <- 2005:2014
    for (i in 1:length(years)){
      
      # Set up the time mask for this year 
      timeLT <- as.POSIXlt(time)
      monitorYear <- timeLT$year + 1900
      summerMask <- years[i] == monitorYear
      
      # Now get statistics on each monitor for each summer 
      stationPMMean <- as.numeric(apply(PM_df[summerMask,], 2, mean, na.rm=TRUE))
      stationPMSd   <- as.numeric(apply(PM_df[summerMask,], 2, sd, na.rm=TRUE))
      sdFactor      <- 1 # This is the number of sd over mean require to say smoke 
      
      stationSummerPMTheshold <- stationPMMean + stationPMSd * sdFactor
      
      # This needs to have the same length as time, repeat a value for each
      # summer. 
      for(dayIndex in which(summerMask)){
        stationPMTheshold[dayIndex,] <- stationSummerPMTheshold
      }
      
    } # End of looping through years coming up with PM Thresholds
    
    # Loop through everyday, looking for where each monitor exceeds its thresh
    for (p in 1:length(time)){
      PMOverThresh <- PM_df[p, ] >= stationPMTheshold[p,]
      HighPMMask[p,] <- PMOverThresh
    }
    
    
  } # End of if statement choosing PMThreshType
  
  # Missing values need to be changed to FALSE for HighPMMask
  HighPMMaskNAMask <- is.na(HighPMMask)  # aka where we are missing PM Data 
  HighPMMask[HighPMMaskNAMask] <- FALSE # defualt conservative option is to say 
                                         # this does not have high PM
  
  rownames(HighPMMask) <- as.character(time)
  colnames(HighPMMask) <- PMMonitors
  ##############################################################################
  # Create array to store monitor specific smoke mask by day (same dimensions
  # as ozone and PM data)
  ############################################################################## 
  HMSMask <- array(data=FALSE, dim=c(nDays, nStations))
  rownames(HMSMask) <- as.character(time)
  colnames(HMSMask) <- PMMonitors # It was the monitor that set the PM thresh
  
  ##############################################################################
  # loop through days opening hms smoke data as you go
  ##############################################################################
  for (i in 1:length(time)){
    
    t <- as.POSIXlt(time[i])
    year <- t$year + 1900
    mon <- t$mon+1;if(str_length(mon) < 2){mon <- paste0("0",mon)}
    day <- t$mday; if(str_length(day) < 2){day <- paste0("0",day)}
    
    # Load the GIS smoke data
    layername <- paste0("hms_",year, mon, day)
    print(paste("Working on layername:", layername))
    
    # Use try() to see if we can load this days GIS file
    try_error <- try(
      GIS <- readOGR(dsn="FireData/HMS_smoke/", layer=layername, verbose=FALSE)
      , silent = FALSE
    )
    
    # If the GIS data exists carry on
    if(!class(try_error) == "try-error"){
      
      # What stations overlap with smoke? 
      index <- over(locations, GIS)[,1] # We want where it is NOT NA
      HMSDayMask  <- !(is.na(index)) # where true, that location is covered by hms smoke 
      
      if(HMSTest){
        
        pdf(file=paste0("dump/",layername,".pdf"))
        map("state")
        plot(GIS, add=TRUE, col="red")
        plot(locations[HMSDayMask],add=TRUE, col="blue", pch=19, cex=0.7)
        plot(locations[!HMSDayMask], add=TRUE, col="black", pch=19, cex=0.5)
        title(layername)
        dev.off()
        
      }
      
      
      # Mark stations under/in smoke GIS and PM > normal by 1 sd as smoked
      # rowMask <- HMSmask #& PMMask
      HMSMask[i, HMSDayMask] <- TRUE # All started as false, so change the ones
                                     # that meet the condition
      

    } # End of if statement checking for GIS data completeness
    
  } # End of looping through PM ozone, HMS data days 
  
  ##############################################################################
  # Create a smokeImpactMask that will be combination of PMMask and HMSMask 
  ##############################################################################
  smokeImpactMask <- HMSMask & HighPMMask
  
  workSpaceData <- list()
  workSpaceData[["Ozone_df"]]        <- Ozone_df
  workSpaceData[["PM_df"]]           <- PM_df
  workSpaceData[["PM_mdf"]]          <- PM_mdf_subset
  workSpaceData[["Hybrid_mdf"]]      <- Hybrid_mdf
  workSpaceData[["HMSMask"]]         <- HMSMask
  workSpaceData[["HighPMMask"]]      <- HighPMMask
  workSpaceData[["smokeImpactMask"]] <- smokeImpactMask
  workSpaceData[["lat"]]             <- lat
  workSpaceData[["lon"]]             <- lon
  workSpaceData[["city"]]            <- city
  workSpaceData[["cityName"]]        <- cityName
  workSpaceData[["time"]]            <- time

  
  # Return the workspace parameters
  print("finished creating a new smoke mask and data packet")
  
  # TODO: Decide how and if you are going to save the following list
  dataPacketSaveName <- paste0("analysisData/",PMSpecies,"_",Sample.Duration,"hr_",
                               stat,"_",PMThreshType,".RData")
  
  if(file.exists(dataPacketSaveName)){
    warning('You are overwriting existing workSpaceData packet')
  }
  
  save(workSpaceData, file=dataPacketSaveName)  
  return(workSpaceData)
  
}

# Execute the function, create the masks and workspace
workSpaceData <- createHMSPM25SmokeMask(PMSpecies=PMSpecies,
                                        Sample.Duration=Sample.Duration,
                                        stat=stat,
                                        PMThreshType=PMThreshType,
                                        PMMetaFile=PMMetaFile,
                                        OzoneFile=OzoneFile,
                                        maxMonth=maxMonth, 
                                        minMonth=minMonth,
                                        HMSTest=HMSTest)



################################################################################
# Function creates a mask for station days that see smoke based on HMS GIS data 
# plus anomously high PM25 & CO concentrations creates a work environment for 
# making smoked vs. clear ozone distributions 
#
# ------------------------------IN DEVELOPMENT!---------------------------------
################################################################################
createHMSPM25COSmokeMask <- function(temperatureData = "ecmwf", 
                                   PMFile = "PM2.5_non_FRM_Mass_allYears.RData",
                                   COFile= "CO_allYears.RData",
                                   maxMonth=9, 
                                   minMonth=5
){
  
  ##############################################################################
  # Handle HMS data 
  ##############################################################################
  
  # Location the HMS GIS smoke files in project 
  smokeFiles <- list.files("FireData/HMS_smoke/")
  
  # Only look at days that we have smoke data for. Determined by preference and
  # HMS dataset 
  smokeDayString <- unique(str_sub(smokeFiles,5,12))
  smokeTime <- as.POSIXct(smokeDayString, format="%Y%m%d")
  
  smokeTimeLT <- as.POSIXlt(smokeTime)
  month       <- smokeTimeLT$mon + 1
  monthMask   <- month >= minMonth & month <= maxMonth 
  
  # Apply the monthMask to further subset data 
  smokeTime <- smokeTime[monthMask]
  
  ##############################################################################
  # Handle AQS Data source
  ##############################################################################
  AQSDataDir <- "timeSeriesData/mergedYears/X1st.Max.Value/"

  ##############################################################################
  # Load PM data [day by station]
  ##############################################################################  
  PMFile <- paste0(AQSDataDir, PMFile)
  print(paste0("The PM file being used is:",PMFile))
  PM_df <- get(load(PMFile))
  
  # First get rid of all ozone stations that do not also measure PM2.5
  PMStations <- colnames(PM_df)
  
  ##############################################################################
  # Load CO data [day by station]
  ##############################################################################  
  COFile <- paste0(AQSDataDir, COFile)
  print(paste0("The CO file being used is:",COFile))
  CO_df <- get(load(COFile))
  
  # First get rid of all ozone stations that do not also measure PM2.5
  COStations <- colnames(CO_df)
  
  
  ##############################################################################
  # Load ozone data [day by station]
  ##############################################################################
  Ozone_df <- get(load("timeSeriesData/mergedYears/X1st.Max.Value/Ozone_allYears.RData"))
  rm("Ozone_allYears")
  OzoneStations <- colnames(Ozone_df)
  
  # TODO: Look for monitors with linear trend!!!!
  
  # match() can take three arguments, will work best if most restrictive 
  # conditions is first e.g.
  # a <- 1:10
  # b <- 1:6
  # c <- 1:3
  # match(c,b,a)
  # 1 2 3 
  
  matchPMtoCO <- match(COStations,PMStations)
  matchPMtoCO <- matchPMtoCO[!is.na(matchPMtoCO)]
  COandPMStations <- PMStations[matchPMtoCO]
  
  # Now see where these overlap with ozone 
  matchPMCOtoOzone <- match(OzoneStations,COandPMStations)
  matchPMCOtoOzone <- matchPMCOtoOzone[!is.na(matchPMCOtoOzone)]
 
  # Stations where all three species are measured 
  hybridStations   <- COandPMStations[matchPMCOtoOzone]
  
  # Make sure this is true
  ozoneMissing <- NA %in% match(hybridStations, OzoneStations)
  PMMissing <- NA %in% match(hybridStations, PMStations)
  COMissing <- NA %in% match(hybridStations, COStations)
  truthArray <- c(ozoneMissing,PMMissing,COMissing)
  
  # If one of these statements is true it means you have not found the locations
  # where all three measurements are being made. 
  if(any(truthArray)){
    stop("code halted. You do not have consistent locations across species.")
  }
  
  
  # Subset all three dataframes to include only hybrid stations 
  Ozone_df <- Ozone_df[, match(hybridStations,OzoneStations)]
  PM_df    <- PM_df[, match(hybridStations,PMStations)]
  CO_df    <- CO_df[,match(hybridStations,COStations)]
  
  # Subset PM and CO data by the dates we have smoke data for 
  PMTime <- as.POSIXct(rownames(PM_df))
  matchSmokePMTime <- match(smokeTime, PMTime)
  naMask <- is.na(matchSmokePMTime)
  matchSmokePMTime <- matchSmokePMTime[!naMask]
  time <- PMTime[matchSmokePMTime]
  
  PM_df <- PM_df[matchSmokePMTime, ]
  nDays    <- length(time)
  
  # Force ozone to match this time sequence 
  OzoneTime <- as.POSIXct(rownames(Ozone_df))
  Ozone_df <- Ozone_df[match(time, OzoneTime), ]
  
  # Force CO to match this time sequence
  COTime <- as.POSIXct(rownames(CO_df))
  CO_df <- CO_df[match(time, COTime), ]
  
 
  # As a dummmy place holder. This will get overwritten in getWeatherData
  # NOTE: Not needed anymore as this workspace is no longer saved 
  T_df <-   PM_df
    
  
  if(dim(PM_df)[1] == dim(Ozone_df)[1] & dim(PM_df)[2] == dim(Ozone_df)[2]){
    print("Dimensions of ozone data and pm data are the same. Looking good.")
    print(paste("Dimension of both are:", dim(Ozone_df)))
  } else{
    print("Ozone and PM data should be of the same dimension. Try again.")
  }
  
  ##############################################################################
  # hybrid station metadata
  ##############################################################################
  Ozone_mdf <- get(load("EPAData/Metadata/Ozone_all_metadata.RData"))
  rm("Ozone_all_metadata")
  
  # Get ozone stations metadata, lat lon, better name of location
  matchMeta <- match(hybridStations, Ozone_mdf$stationID)
  lat       <- Ozone_mdf$Latitude[matchMeta]
  lon       <- Ozone_mdf$Longitude[matchMeta]
  city      <- Ozone_mdf$CBSA.Name[matchMeta]
  cityName  <- Ozone_mdf$City.Name[matchMeta]
  Hybrid_mdf<- Ozone_mdf[matchMeta,]  
  
  ##############################################################################
  # Create spatial points dataframe of hybrid monitor locations
  ##############################################################################
  coords    <- cbind(lon, lat)
  locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))
  
  ##############################################################################
  # Create array to store monitor specific smoke mask by day (same dimensions
  # as ozone data)
  ############################################################################## 
  nStations <- length(hybridStations)
  smokeMask <- array(data=FALSE, dim=c(nDays, nStations))
  rownames(smokeMask) <- as.character(time)
  colnames(smokeMask) <- hybridStations
  
  ##############################################################################
  # Find each stations mean PM and sd in order to establish threshold
  ##############################################################################
  # TODO: This needs a potential update to be a 2 week or a month or season
  # TODO: before fire threshold.
  stationPMMean <- as.numeric(apply(PM_df, 2, mean, na.rm=TRUE))
  stationPMSd   <- as.numeric(apply(PM_df, 2, sd, na.rm=TRUE))
  sdFactor      <- 1 # This is the number of sd over mean require to say smoke 
  stationPMTheshold <- stationPMMean + stationPMSd * sdFactor
  
  
  ##############################################################################
  # Remove each CO station linear trend then,
  # Find each stations mean PM and sd in order to establish threshold
  ##############################################################################
  stationCOMean <- as.numeric(apply(CO_df, 2, mean, na.rm=TRUE))
  stationCOSd   <- as.numeric(apply(CO_df, 2, sd, na.rm=TRUE))
  sdFactor      <- 1 # This is the number of sd over mean require to say smoke 
  stationCOTheshold <- stationCOMean + stationCOSd * sdFactor
    
  ##############################################################################
  # loop through days opening hms smoke data as you go
  ##############################################################################
  for (i in 1:length(time)){
    
    t <- as.POSIXlt(time[i])
    year <- t$year + 1900
    mon <- t$mon+1;if(str_length(mon) < 2){mon <- paste0("0",mon)}
    day <- t$mday; if(str_length(day) < 2){day <- paste0("0",day)}
    
    # Load the GIS smoke data
    layername <- paste0("hms_",year, mon, day)
    print(paste("Layername:", layername))
    
    try_error <- try(
      GIS <- readOGR(dsn="FireData/HMS_smoke/", layer=layername, verbose=FALSE)
      , silent = FALSE
    )
    
    # If the GIS data exists carry on
    if(!class(try_error) == "try-error"){
      
      # What stations overlap with smoke? 
      # TODO: Save a sperate dataframe to show what level of severity of smoke
      index <- over(locations, GIS)[,1] # We want where it is NOT NA
      HMSmask  <- !(is.na(index)) # where true, that location is covered by hms smoke 
      
      # We also need to find which stations experienced higher than normal
      # PM on this day ("higher than normal" TBT)
      PMMask <- as.numeric(PM_df[i, ]) > stationPMTheshold
      
      # Where NA station has no data to make a mean so set NA to FALSE
      PMMask[is.na(PMMask)] <- FALSE
      
      # We also need to find which stations experienced higher than normal
      # CO on this day ("higher than normal" TBT)
      COMask <- as.numeric(CO_df[i, ]) > stationCOTheshold
      
      # Where NA station has no data to make a mean so set NA to FALSE
      PMMask[is.na(PMMask)] <- FALSE
      
      
      # Mark stations under/in smoke GIS and PM > normal by 1 sd as smoked
      rowMask <- HMSmask & PMMask & COMask
      smokeMask[i, rowMask] <- TRUE # All started as false, so change the ones
      # that meet the condition
      
      
      
      
    } # End of if statement checking for GIS data completeness
    
  } # End of looping through PM ozone, HMS data days 
  
  densityMask <- smokeMask
  # Return the workspace parameters
  # NOTE: These are no longer saved automattically when this is executed
  # NOTE: The workspace is saved mannually in the annalysis pipeline
  L <- list()
  L[["Ozone_df"]] <- Ozone_df
  L[["PM_df"]] <- PM_df
  L[["Hybrid_mdf"]] <- Hybrid_mdf
  L[["smokeMask"]] <- smokeMask
  L[["lat"]] <- lat
  L[["lon"]] <- lon
  L[["city"]] <- city
  L[["cityName"]] <- cityName
  L[["densityMask"]] <- densityMask
  L[["T_df"]] <- T_df
  
  print("finished creating a new smoke mask and data packet")
  
  return(L)
  
  
  
}