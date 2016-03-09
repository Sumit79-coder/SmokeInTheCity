# mergeAnnualTimeSeries.R

# Load required libraries 
library(stringr)

####################################################################################
# This script will merge the annual time series created for each years data 
# using 'createEPASpeciesArray.R'. The function will take a year as an argument
# and match data for that years stations with all other requested years data. 
# The result will be a different dataset for each year starting years stations
# requested. 
####################################################################################

####################################################################################
# ARGUMENTS: HUMAN INPUT REQUIRED 
####################################################################################

makeData <- FALSE # TRUE == functions are executed, FALSE == they are not
startYear=2005
endYear=2014
species="PM2.5_FRM_FEM_Mass" # PM2.5_non_FRM_Mass | PM2.5_FRM_FEM_Mass | Ozone | CO
Sample.Duration="24" # "1" | "8" | "24" # hours
dataDir="timeSeries/individualYears" 
metadataDir="EPAData/Metadata"
stat = "Arithmetic.Mean" # | "X1st.Max.Value" | "Arithmetic.Mean"
####################################################################################

####################################################################################
# This function finds all the unique monitoring IDs for all years specified for 
# given parameter
####################################################################################
allSpeciesStations <- function(species="PM2.5_non_FRM_Mass",
                               startYear=2005,
                               endYear=2014,
                               Sample.Duration="24",
                               stat = "Arithmetic.Mean",
                               dataDir="timeSeries/individualYears",
                               returnStations=TRUE){
  
  # Array that will be appended with each years station names, unique 
  # command taken at the end. 
  allStationIDs <- rep(NA,0) 
  
  for (year in startYear:endYear){
    
    # Shared file name characters
    fileBaseName <- paste0(species,"_",year,"_",stat,"_",Sample.Duration,"hr_t")
    fileName <- paste0(dataDir,"/",fileBaseName,".RData")
    # Load data df
    
    a <- get(load(fileName))
    rm(list=fileBaseName)
    
    # Get this years station names (locations)
    stations <- colnames(a)
    allStationIDs <- append(allStationIDs,stations)
    
  }
  
  allStationIDs <- unique(allStationIDs)
  
  # Save the merged data for the selected matchYear
  newFileSaveName <- paste0(species,"_",stat,"_",Sample.Duration,"hr","_allStationIDs")
  assign(newFileSaveName,allStationIDs)
  saveDir <- paste0("timeSeries/")
  
  diskName <- paste0(saveDir,newFileSaveName,".RData")
  
  # Only write the file if it does not exist 
  #if(!file.exists(diskName)){
  #save(list=newFileSaveName, file=diskName)
  #}
  
  # If the user wants the list in the workspace return
  if(returnStations){
    return(allStationIDs)
  }
  
  
}


####################################################################################
# Create matrix of merged data for a given year for all stations for a species
####################################################################################
mergeAnnualTimeSeries <- function(species="PM2.5_non_FRM_Mass",
                                  startYear=2005,
                                  endYear=2014,
                                  Sample.Duration="24",
                                  stat="Arithmetic.Mean",
                                  dataDir="timeSeries/individualYears"
){
  
  # create years array to loop over   
  years <- startYear:endYear
  
  # Load unique array
  allStations <- allSpeciesStations(species=species,
                                      startYear=startYear,
                                      endYear=endYear,
                                      Sample.Duration=Sample.Duration,
                                      stat = stat,
                                      dataDir=dataDir,
                                      returnStations=TRUE)
  
  # create dummy matrix to preserve working code structure 
  nUniqueStations <- length(allStations)
  
  # Make a master time array
  startDate       <- as.POSIXct(paste(startYear,"01-01",sep="-"), tz="MST")
  endDate         <- as.POSIXct(paste(endYear,"12-31",sep="-"), tz="MST")
  masterTime      <- seq(startDate, endDate, by="day")
  nDays           <- length(masterTime)
  
  # Make a master matrix to store all data
  df <- matrix(data=NaN, 
               nrow=nDays, 
               ncol=nUniqueStations)
  
  # label the columns with the names of the unique stations
  colnames(df) <- allStations
  rownames(df) <- as.character(masterTime)
  
  # Loop through years appending the correct data by station and date
  for (year in years){
    
    # Shared file name characters
    fileBaseName <- paste0(species,"_",year,"_",stat,"_",Sample.Duration,"hr_t")    
    
    # Load data df
    a_t <- get(load(paste0(dataDir,"/",fileBaseName,".RData")))
    rm(list=fileBaseName)
    
    # Get this years station names (locations)
    yearlyStations <- colnames(a_t)
    stationTime    <- as.POSIXct(rownames(a_t), tz="MST")
    
    
    print(paste("working on year:", year))
    
    # loop through stations
    stationCount <- 0
    for (station in yearlyStations){
      stationCount <- stationCount + 1
      
      # Get this individual station/monitor's data
      stationData <- as.numeric(a_t[, station == yearlyStations])
      
      # Sanity check
      if(!length(stationData) == length(stationTime)){
        stop("There is a problem with assigning the data")
      }
      
			# Which column on df do we need to place data?
      columnMask <- station == colnames(df)
      
      # Make sure this gives what you want 
      test <- colnames(df)[columnMask] == station
      if(!test){
        stop("YOU ARE NOT PLACING DATA IN CORRECT COLUMN")
      }
      
      timeMask <- match(stationTime, masterTime)
      #df[timeMask, columnMask] <- as.character(stationTime)
      df[timeMask, columnMask] <- stationData
      
    } # end of station loop 
    
  } # end of matching data to matchYear for loop
  
  # Save the new master dataframe 
  newFileSaveName <- paste0(species,"_",stat,"_",Sample.Duration,"hr_",startYear,"_",endYear)
  assign(newFileSaveName,df)
  saveDir <- paste0("timeSeries/mergedYears/")
  save(list=newFileSaveName, file=paste0(saveDir,newFileSaveName,".RData"))
  
}



####################################################################################
# We need the annual metadata to be merged into a single file also for each species
####################################################################################
mergeAllSpeciesMetadata <- function(species="PM2.5_non_FRM_Mass",
                                    startYear=2005,
                                    endYear=2014,
                                    Sample.Duration="24",
                                    stat="Arithmetic.Mean",
                                    dataDir="EPAData/Metadata"
){
  
  years <- startYear:endYear
  
  # Figure out how many columns metadata for this set of arguments has
  metaBaseName <- paste0(species,"_",years[1],"_", stat,"_",Sample.Duration,"hr_metadata")
  metaFile <- paste0(dataDir,"/",metaBaseName,".RData")
  meta <- get(load(metaFile))
  rm(list=metaBaseName)
  colNames <- colnames(meta)
  
  # Create massive matrix/dataframe to save all metadata in 
  m <- matrix(NA,nrow=0,ncol=length(colNames))
  colnames(m) <- colNames
  df <- as.data.frame(m)
  
  # change name to make clear that we are merging different meta dataframes
  merged <- df 
  
  # Loop through years appending the correct data 
  for (year in years){
    
    # Shared file name characters
    metaBaseName <- paste0(species,"_",year,"_", stat,"_",Sample.Duration,"hr_metadata")
    
    # Load data df
    a <- get(load(paste0(dataDir,"/",metaBaseName,".RData")))
    rm(list=metaBaseName) # NOTE: class(a) == "data.frame"
    
    # Simply append all metadata for each year then cut to unique information 
    # at the end 
    merged <- rbind(merged, a)
    
    
  } # end of loop merging each years metadata
  
  # ensure the rows are unique
  allMetaStations    <- merged$ID
  uniqueMetaStations <- unique(allMetaStations)
  matchingIndicies   <- match(uniqueMetaStations,allMetaStations)
  
  # Subset the dataframe and keep only the unique 
  mergedUnique <- merged[matchingIndicies, ]
  
  
  # Save the merged data for the selected matchYear
  newFileSaveName <- paste0(species,"_", stat,"_",Sample.Duration,"hr_metadata_",startYear,"_",endYear)
  assign(newFileSaveName,mergedUnique)
  saveDir <- paste0("timeSeries/mergedYearsMetadata/")
  save(list=newFileSaveName, file=paste0(saveDir,newFileSaveName,".RData"))
  
  
}


################################################################################
# Create requested data using the functions degined above 
################################################################################
if(makeData){
  
  # First merge the yearly timeseries
  mergeAnnualTimeSeries(species=species,
                        startYear=startYear,
                        endYear=endYear,
                        Sample.Duration=Sample.Duration,
                        stat=stat,
                        dataDir=dataDir)
  
  # Then create a merged metadata for the same merged time series
  mergeAllSpeciesMetadata(species=species,
                          startYear=startYear,
                          endYear=endYear,
                          Sample.Duration=Sample.Duration,
                          stat=stat,
                          dataDir=metadataDir)

  
}




