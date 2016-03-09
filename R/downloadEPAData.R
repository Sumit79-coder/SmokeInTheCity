# downloadEPAData.R 

# This script is used to download EPA AQS data. 

# DATA URL:
# BASIC METADATA URL: http://www.epa.gov/airquality/airdata/ad_basic.html
# More important metadata: http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/FileFormats.html

# IF you want to download all data uncomment lines 38 and 39. Otherwise choose
# which ones to download one at a time. 

# Library for manipulating strings 
library(stringr)

################################################################################
# ARGUMENTS: HUMAN INPUT REQUIRED 
################################################################################
getDaily  <- TRUE
getHourly <- FALSE
overRideOldFile <- TRUE # If file exists will be overwritten when TRUE
species <- "44201"
speciNames <- "Ozone"
years       <- 2004:2004

# Name of directory outside of working directory where the files will be saved 
localDir <- "EPAData"
################################################################################

################################################################################
# Exhaustive list of daily data codes and names
################################################################################
# years       <- 2005:2015
# species     <- c(44201,   42401,   42101,  42602,      88101,                 88502,           81102,     "VOCS","HAPS",  TEMP	)
# specisNames <- c("Ozone",  "SO2",  "CO",   "NO2", "PM2.5_FRM_FEM_Mass", "PM2.5_non_FRM_Mass", "PM10_Mass","VOCS","HAPS", "Temperature")
################################################################################


# Build the file names and get the data making sure to delete unneeded files and
# clearing workspace along the way
downloadDailyData <- function(years=years,
                              species=species,
                              speciNames=specisNames
                              ){
  
  # Loop through the requested years and species and get the daily data
  for (year in years) {
    speciesIndex <- 0 
    # Loop through species of interest
    for (specie in species) {
      # Count what species index we are on
      speciesIndex <- speciesIndex + 1
      
      print(paste("Species being downloaded is: ", specie))
      
      # Build the species data url for download  
      baseURL <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_"
      url <- paste0(baseURL,specie,"_",year,".zip")
      
      # make download names and explicit file extension name 
      fileExtensionName <- paste0(specie,"-",year)
      file <- paste0(localDir,"/",basename(url))
      
      # If the file does not exist get the file
      #if (!file.exists(file)) {
      # Currently set to always download, handy for updating current year data
      if (overRideOldFile){
        download.file(url, file)
        unzip(file,exdir=localDir)
      } else if(!file.exists(file)){
        download.file(url, file)
        unzip(file,exdir=localDir)
      }
      
      # remove the zip file and replace file .zip with .csv
      unlink(file,recursive = TRUE)
      file <- str_replace_all(file, ".zip", ".csv")
      
      # Convert the downloaded .csv to an RData.frame 
      df <- read.csv(file,header = TRUE, stringsAsFactors=FALSE)
      
      ##########################################################
      # Format the df for analysis using R (appropriete types)
      ##########################################################
      columnNames <- names(df)
  
      # TODO: Consider giving site ID before the file is saved as .RData
      niceFileName <- paste0(speciNames[speciesIndex],"_",year)
      niceFileSaveName <- paste0(localDir,"/",niceFileName,".RData")
      df$stationID <- paste0(df$State.Code,"-",df$County.Code, "-", df$Site.Num, "-", df$POC)
      assign(niceFileName, df)
      save(list=niceFileName, file=niceFileSaveName)
      
      # remove the .csv from the data directory (may want to rerun without this for sharin with others)
      unlink(file,recursive = TRUE)
      rm(df) # clears workspace of extra dataframe that has been renamed 
      rm(list=niceFileName) # clears workspace of saved file
      
      # TODO: Make sure the unique name is removed after it is saved!
      
    }
  }
}

# Execute the download. 
if(getDaily){
  downloadDailyData(years=years,
                    species=species,
                    speciNames=speciNames)
}


################################################################################
# Function for downloading hourly data 
################################################################################
downloadHourlyData <- function(years=years,
                               species=species,
                               specisNames="PM2.5_FRM_Mass_hourly"
){
  
  # Loop through the requested years and species and get the daily data
  for (year in years) {
    speciesIndex <- 0 
    # Loop through species of interest
    for (specie in species) {
      # Count what species index we are on
      speciesIndex <- speciesIndex + 1
      
      # Build the species data url for download  
      url <- paste0("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/hourly_",specie,"_",year,".zip")
      
      # make download names and explicit file extension name 
      fileExtensionName <- paste0(specie,"-",year)
      file <- paste0(localDir,"/",basename(url))
      
      # If the file does not exist get the file
      #if (!file.exists(file)) {
      if (TRUE){
        download.file(url, file)
        unzip(file,exdir=localDir)
      }
      
      # remove the zip file and replace file .zip with .csv
      unlink(file,recursive = TRUE)
      file <- str_replace_all(file, ".zip", ".csv")
      
      # Convert the downloaded .csv to an RData.frame 
      df <- read.csv(file,header = TRUE, stringsAsFactors=FALSE)
      
      ##########################################################
      # Format the df for analysis using R (appropriete types)
      ##########################################################
      columnNames <- names(df)
      df$Date.Local <- as.character(df$Date.Local)
      # TODO: Figure out how to convert to R date format without breaking 
      #df$Date.Local <- as.POSIXct(df$Date.Local,format="%Y-%m-%d") # breaks R console
      #df$Longitude <- df$Longitude
      #df$Latitude <- df$Latitude
      #df$Observation.Percent
      
      # TODO: Consider giving site ID before the file is saved as .RData
      niceFileName <- paste0(speciNames[speciesIndex],"_",year)
      niceFileSaveName <- paste0(localDir,"/",niceFileName,".RData")
      assign(niceFileName, df)
      df$stationID <- paste0(df$State.Code,"-",df$County.Code, "-", df$Site.Num, "-", df$POC)
      save(list=niceFileName, file=niceFileSaveName)
      
      # remove the .csv from the data directory (may want to rerun without this for sharin with others)
      unlink(file,recursive = TRUE)
      rm(df) # clears workspace of extra dataframe that has been renamed 
      rm(list=niceFileName) # clears workspace of saved file
      
      # TODO: Make sure the unique name is removed after it is saved!
      
    }
  }
}

# FRM PM25 88101
# non_FRM PM25 88502

if(getHourly){
  downloadHourlyData(years=years,
                     species=88101,
                     speciNames="PM2.5_FRM_Mass_hourly")
}






