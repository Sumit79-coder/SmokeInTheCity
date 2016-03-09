# createSpeciesArray.R 

library(stringr)


# This script will organize a single species yearly .RData dataframes into a single
# dataframe/array that has stations as the columns and daily/hourly/timeunit 
# values as rows. 


################################################################################
# specisNames <- c("Ozone",  "SO2",  "CO",  "NO2", "PM2.5_FRM_Mass", "PM10_Mass", "PM2.5_non_FRM_Mass")
# NOTE: PM2.5_FRM_Mass starts in 1997
################################################################################

################################################################################
# Year Query annual messy csv files to merge by station ID
################################################################################
startYear <- 2004 # first year of years to be merged
endYear   <- 2004 # last year of years to be merged
stat            <- "X1st.Max.Value" # | Arithmetic.Mean | "X1st.Max.Value"
Sample.Duration <- "8" # Ozone: "8-HR RUN AVG BEGIN HOUR" PM:"24 HOUR" | "1 HOUR" | "24-HR BLK AVG"
                        # so str_detect() will be used, looking for desired hour
species <- "Ozone" # "PM2.5_non_FRM_Mass" | "PM2.5_FRM_FEM_Mass | Ozone
################################################################################

################################################################################
# Stat argumemnt
# There is a bit of new stuff going on here:
# 1) Ozone: we want MDA8 quantities. Meaning the sample.duration needs to be
#    8-hour (Sample.Duration == "8-HR RUN AVG BEGIN HOUR"), the quantity we want
#    is X1st.Max.Value aka the maximum of these 8-hour avareges. For this analysis
#    there is no other quantity of ozone we care about. 
# 2) PM2.5_non_FRM_Mass: Here our analysis requires we use both maximum daily
#    hour value AND the mean of 24-hr samples. The unique options for 
#    Sample.Duration seen in the data are "24 HOUR" & "1 HOUR" & "24-HR BLK AVG"
#    I do beleive a single stationID including POC can make multiple types of 
#    these sample durations. So, PM matricies will only contrain 1 of each
#    and indicate the type in the saved file name. 
#    e.g PM2.5_non_FRM_Mass_24hr_X1st.Max.Value
#    e.g PM2.5_non_FRM_Mass_24hr_Arithmetic.Mean
#    e.g PM2.5_non_FRM_Mass_1hr_X1st.Max.Value

# SAMPLE DURATION DOCUMENTATION
# Source: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html#_daily_summary_files

#The length of time that air passes through the monitoring device before it is 
# analyzed (measured). So, it represents an averaging period in the atmosphere 
# (for example, a 24-hour sample duration draws ambient air over a collection 
# filter for 24 straight hours). For continuous monitors, it can represent an 
# averaging time of many samples (for example, a 1-hour value may be the average 
# of four one-minute samples collected during each quarter of the hour).

# The way I interpret this is that "24 HOUR" could be made up of the average
# of a continious monitors 1 hour measurements and "24-HR BLK AVG" is a filter
# left on an inlet for 24 hours with no higher resolution temporal information
# available. 

# NOTE: a test has shown that PM monitors do not possess both "24 HOUR"
#         and "24-HR BLK AVG" for a given instrument
################################################################################

################################################################################
# Function for handling inserted zeros needed in unique station name creation
# will give us the format specified here:
# http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html#_monitors
insertZeros <- function(stateCode, countyCode, siteNum, POCNum){
  
  stateLength <- str_length(stateCode)
  stateCode[stateLength == 1] <- paste0("0",stateCode[stateLength == 1])
  
  countyLength <- str_length(countyCode)
  countyCode[countyLength == 1] <- paste0("00",countyCode[countyLength == 1])
  countyCode[countyLength == 2] <- paste0("0",countyCode[countyLength == 2])
  
  siteLength <- str_length(siteNum)
  siteNum[siteLength == 1] <- paste0("000",siteNum[siteLength == 1])
  siteNum[siteLength == 2] <- paste0("00",siteNum[siteLength == 2])
  siteNum[siteLength == 3] <- paste0("0",siteNum[siteLength == 3])
  
  # TODO: Add POC when making time series but not when making location meata
  POCLength <- str_length(POCNum)
  POCNum[POCLength == 1] <- paste0("0",POCNum[POCLength == 1])
  
  # Return the unique INSTRUMENT ID. The information before the "-" is the
  # station relevant information 
  stationID <- paste0(stateCode,countyCode, siteNum,"-",POCNum)
  
}
################################################################################

################################################################################
# Build Metadata for a given species, stat, and sample duration argument 
buildAnnualMetadata <- function(species="PM2.5_non_FRM_Mass", 
                                stat="Arithmetic.Mean",
                                Sample.Duration="24",
                                dataDir="EPAData",
                                startYear=2005, 
                                endYear=2015
){
  
  # Create year array from input
  years <- seq(startYear,endYear)
  
  # Get all years local file names for chosen species 
  files <- paste0(species,"_",years)
  
  # Read each years data and get all station names 
  for (file in files){ # equivalent to looping through given species years
    
    fileName <- paste0(dataDir,"/",file,".RData")
    df <- get(load(fileName))
    rm(list=file) # remove unwanted df from workspace
    
    # Subset the df by what sample duration we want
    durationMask <- str_detect(df$Sample.Duration, Sample.Duration)
    df    <- df[durationMask,]
    
    # This is the naming scheme as scene on URL
    # http://www.epa.gov/airquality/airdata/ad_viz_plotval.html
    # SITEID = XXYYYZZZZAA
    # XX   = state code 1-50, where 1=alabama 50=wyoming and so on
    # YYY  = county code
    # ZZZZ = site number
    # AA   = POC (instrument of the type and location) 
    # Follow naming guidelines set here
    # http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html
    SITEID <-  insertZeros(df$State.Code, df$County.Code, df$Site.Num, df$POC)
    
    # List all of the stationIDs and figure out how many unique instruments we
    # have for this species/sample.duraction/stat
    stationIDAll       <- SITEID
    stationIDUnique    <- unique(SITEID)
    nUnique            <- length(stationIDUnique) 
    
    # Check to see if there is any data! (TRUE when there are unique IDs)
    if(!nUnique==0){
      
      # Create metadata for unique station locations only 
      # highjack the column dimensions of the 
      #dataframe where all the metadata is coming from 
      metadata <- df[1:nUnique,] 
      for (i in 1:nUnique){
        
        # find the first instance of this unique stationID 
        # where is the first place in the EPA data that this unique ID is found?
        # Use that row to get the associated meata
        firstMatch <- which(stationIDUnique[i] == stationIDAll)[1] 
        metadata[i,] <- df[firstMatch,]
      
        
      } # End metadata dataframe creation loop
      
      # Include unique ID aka stationID column for the metadata
      metadata$ID <- stationIDUnique
      
      # Make sure that the original stationID and stationID match
      matchingNames <- metadata$ID == insertZeros(metadata$State.Code, 
                                                  metadata$County.Code, 
                                                  metadata$Site.Num, 
                                                  metadata$POC)
      if(sum(matchingNames) < length(metadata$ID)){
        stop("The names of your metadata row names dont match your data!")
      }
      
      
      # We don't want columns specific to a dates measurement included in meta
      # We only want to keep information that applies to any measurement
      # take by the unique instrument
      unwantedColumnNames <- c("Date.Local","Event.Type","Observation.Count",
                               "Observation.Percent","Arithmetic.Mean","X1st.Max.Value",      
                               "X1st.Max.Hour", "AQI", "Date.of.Last.Change")
      
      # Create a column mask that assumes we want the column until shown 
      # otherwise
      columnMask <- rep(TRUE, length(names(metadata)))
      
      # Now loop through the offending column names and mark FALSE if not wanted
      for (j in 1:length(unwantedColumnNames)){
        unwantedIsTrue <- str_detect(unwantedColumnNames[j], names(metadata))
        # So set that location to FALSE
        columnMask[unwantedIsTrue] <- FALSE
      }
      
      # Subset the metadata data.frame to exclude unwanted columns
      metadata <- metadata[columnMask]
      
      # Place ID column first to make reading easier in future
      newOrder <- c(length(names(metadata)), (2:length(metadata)-1) )
      metadata_ordered <- metadata[newOrder]
      
      # Give specific savename based on all unique arguments
      metadataSaveName <- paste0(file,"_",stat,"_",Sample.Duration,"hr_","metadata")
      assign(metadataSaveName, metadata_ordered)
      
      # Save this species and years metadata
      metadataFileSaveName <- paste0(dataDir,"/","Metadata/",metadataSaveName,".RData")
      save(list=metadataSaveName, file=metadataFileSaveName)
      rm(list=metadataSaveName)
      
    } # end of checking for data if statement 
    rm(df)
    
  } # End of looping though species years
}
################################################################################

################################################################################
# Using the metadata and data for a given year arrange data by station (column)
# and time (rows). 
################################################################################
createTimeSeriesData <- function(startYear=2005, 
                                 endYear=2014,
                                 speci="PM2.5_non_FRM_Mass",
                                 Sample.Duration="24", # "1" | "8" | "24"
                                 dataDir="EPAData",
                                 metadataDir="EPAData/Metadata",
                                 stat = "Arithmetic.Mean"
){
  
  # Create annual time series for each year in range of years given 
  years <- startYear:endYear
  for (year in years){
    
    # Shared file name characters
    fileBaseName <- paste0(speci,"_",year)
    
    # Load species df
    df <- get(load(paste0(dataDir,"/",fileBaseName,".RData")))
    rm(list=fileBaseName)
    
    # Subset df by the sample.duration argument. We don't want a mix of hourly
    # and 24 hour measurements. This will ensure a match with the metadata being 
    # loaded below and created above. 
    durationMask <- str_detect(df$Sample.Duration, Sample.Duration)
    df    <- df[durationMask,]
    
    # Load metadata
    metaBaseName <- paste0(fileBaseName,"_",stat,"_",Sample.Duration,"hr_metadata")
    metaFile <- paste0(metadataDir,"/",metaBaseName,".RData")
    
    meta <- get(load(metaFile))
    rm(list=metaBaseName)
    
    # Figure out unique time axis
    uniqueLocalDateString <- unique(df$Date.Local) # Not used
    localDates <- as.POSIXct(uniqueLocalDateString) # Not used 
    dates <- sort(as.POSIXct(unique(df$Date.Local), tz="MST"))
    
    # NOTE: All dates are LOCAL to station LOCATION! I am putting MST since
    # NOTEL this will help remind me that the dates are assocaited with North 
    # NOTE: American time zones. If I use GMT this could confuse the date when
    # NOTE: when I start thinking about time. When time is stored as a string
    # NOTE: in rownames() there is no memory of time zone. 
    
    # Get the station IDs from the data the same way it was created for metadata
    stationIDAll <- insertZeros(df$State.Code, df$County.Code, df$Site.Num, df$POC)
    
    # Make sure the unique here is the same as if we combine sample.duration
    uniqueDFStations <- length(unique(stationIDAll))
    uniqueDFStations_wDuration <- length(unique(paste0(stationIDAll, df$Sample.Duration)))
    if(!(uniqueDFStations == uniqueDFStations_wDuration)){
      stop("Some of your columns may include multiple monitoring instruments!")
    }
    
    # Now get the unique of these from the metadata 
    stationIDs <- meta$ID
    
    if( !(length(stationIDs) == uniqueDFStations_wDuration)){
      stop("Number of stations in metadata and in years data should be the same")
    }
    
    # We now have the dimensions of the needed array. rows X columns 
    # where rows are dates and columns are stations. a will store all information
    # and be labelled as needed
    nRow <- length(dates)
    nCol <- length(stationIDs)
    a <- array(data=NA,
               dim = c(nRow,nCol)
    )
    
    # Label the columns as the unique identifiers for each station 
    colnames(a) <- stationIDs
    row.names(a) <- as.character(dates) # No time zone memory 
    
    # Assign data to this array, one station at a time 
    for (stationID in stationIDs) {
      
      # Create mask for given station 
      stationMask <-  stationID == stationIDAll
      
      # Subset the data by the mask of the desired station only 
      df_subset <- df[stationMask,]
      
      # Check to see how many unique Sample.Durations df_subset has
      if(length(unique(df_subset$Sample.Duration)) > 1){
        stop(paste("You may be using two measurement durations at this monitor!", stationID))
      }
      
      # Get the values 
      stationValues <- df_subset[[stat]] 
      stationDates  <- as.POSIXct(df_subset$Date.Local,tz="MST") # Note each stations dates are needed to we can assign the data correctly 
      
      # TODO: Maybe a better faster way to do this than the "match" function
      stationDataOrder <- match(stationDates,dates)
      
      # place the data in "a"
      columnMask <- colnames(a) == stationID
      
      #     # Sanity Check: This will check to make sure the data is aligning correctly 
      #     a[stationDataOrder,columnMask] <- as.character(stationDates)
      
      a[stationDataOrder,columnMask] <- stationValues
      
      
    } # end of assigning data for individual station loop 
    
    # Save in a directory and well labeled file based on species and
    # and chosen value e.g. value.mean, 90th percentile etc. 
    saveDataDir <- paste0("timeSeries/individualYears/")
    
    # If this saveDataDir does not exist make sure to create it
    if (!file.exists(saveDataDir)) {
      dir.create(saveDataDir)
    }
    
    # Create unique file Identifier and use in a file savename 
    fileIdentifier <- paste0(fileBaseName,"_",stat,"_",Sample.Duration,"hr_t") # the _t is for time series
    fileSaveName   <- paste0(saveDataDir,fileIdentifier,".RData") 
    
    # Assign the nice name and save in appropriete directory 
    assign(fileIdentifier,a)
    save(list=fileIdentifier, file=fileSaveName)
    
  } # end of year loop
  
}

# ################################################################################
# Execute functions
# ################################################################################

# First build the metadata for each year 
buildAnnualMetadata(species=species, 
                    stat=stat,
                    Sample.Duration=Sample.Duration,
                    dataDir="EPAData",
                    startYear=startYear, 
                    endYear=endYear)

# Then create the year time series arrays 
createTimeSeriesData(startYear=startYear, 
                     endYear=endYear,
                     speci=species,
                     Sample.Duration=Sample.Duration, # "1" | "8" | "24"
                     dataDir="EPAData",
                     metadataDir="EPAData/Metadata",
                     stat = stat)

# ################################################################################







