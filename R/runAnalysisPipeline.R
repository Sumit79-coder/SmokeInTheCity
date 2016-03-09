# runAnalysisPipeline.R

# This script will be used to run the data pipeline starting with the creation 
# of HMS/PM smoke masks (optional) and moving all the way to figures from 
# ozoneDistributios.R

# Clear the workspace before you start
rm(list = ls())


################################################################################
# ANALYSIS ARGUEMENTS: USER INPUT REQUIRED
################################################################################
daysThreshold   <- 10      # the minimum smoky days to create distribution
temperatureData <- "ecmwf" # "ecmwf" | "wunderground" 
makeDist        <- TRUE    # make the individual distribution plots
makePlots       <- TRUE    # make the analysis plots 
PMThresh        <- 25      # Do not allow maximum hourly pm to exceed this amount
applySkyMask    <- FALSE   # do you want to specify a sky condition? 
compareToNoSky  <- FALSE   # do you want to compare results to when there was no sky mask?
clearSkyThresh  <- 100     # % of a smoke-free days afternoon hours required to be clear 
maxCloudCoverPercent <- 10 # OR for ecmwf, this is the cloud cover % threshold to be considered
                           # clear sky at 00Z ~ late afternoon 
TSdFactor       <- 1       # Factor on standard deviation above the mean that smoke-free
                           # days must have 
                           # TClearMin = mean(smokeImpacted) + sd(someImpacted) * TSdFactor
standAloneTitles <- FALSE   # Places detailed titles on figures so they are self explanatory

################################################################################
# Select a data packet: USER INPUT REQUIRED
################################################################################
PMSpecies <- "PM2.5_non_FRM_Mass" # PM2.5_non_FRM_Mass | PM2.5_FRM_FEM_Mass 
Sample.Duration <- "24"           # "1" | "24"
stat <- "Arithmetic.Mean"         # Xst.Max.Value | Arithmetic.Mean
PMThreshType <- "allSummers"      # 'allSummers' | 'individualSummer'

# write the name of the file with these parameters 
workSpaceDataFile <- paste0(PMSpecies,"_", Sample.Duration,"hr_",stat,"_",PMThreshType)

# Load data packet to work with: Loads workSpaceData list()
load(paste0("analysisData/",workSpaceDataFile,".RData"))


################################################################################
# Also create a directory for analysis figures created in this pipeline
################################################################################
figureDir <- paste0("figures/",workSpaceDataFile,"_",temperatureData,"_",
                   "nDays=",daysThreshold, "_TSdFactor=",TSdFactor
                   )

# Append sky_stuff in name if used 
if(applySkyMask){
  figureDir <- paste0(figureDir, "_", "skyMask=",applySkyMask,"_",
                      "maxCC=", maxCloudCoverPercent)
}
figureDir <- paste0(figureDir,"/")

# If the file does not exist, create it
if(!file.exists(figureDir)){
  dir.create(figureDir)
}
################################################################################




################################################################################    
# Inlcude desired temperature data in the working environment
################################################################################ 
if(temperatureData == "wunderground"){
  
  # Use airport temperature data downloaded from wunderground
  # e.g. https://www.wunderground.com/history/airport/PARL/2005/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2005&format=1
  source("R/getWeatherData.R")
  maxDistance <- 40 # km, checked with haversine
  
  # First append the airport data (T_df) to workSpaceData 
  workSpaceData <- assignWundergroundTToMonitor(workSpaceData, 
                                                maxDistance)
  
  print("wunderground airport temperature data assigned")
  
} else if(temperatureData == "ecmwf") {
  
  # Use gridded ECMWF temperature data to account for the temperature difference
  # between smoke-impacted and smoke-free days
  
  # Source the needed script
  source("R/assignECMWFTDataToMonitor.R")
  
  # Assign temperature data to PM monitors (decide whether to append to list or not)
  workSpaceData <- getStationWeatherData(workSpaceData=workSpaceData, 
                                         sanityCheck=FALSE)
  
  print("gridded ECMWF temperature data assigned")
  

  
}

################################################################################    
# Ensure warmer temperature for clear sky days now that you have temperature
# data. 
################################################################################ 
# NOTE: Skymask not in use. 
source("R/createSmokeFreeMask.R")
workSpaceData <- createSmokeFreeTMask(workSpaceData, 
                                      TSdFactor = TSdFactor,
                                      applySkyMask=applySkyMask, 
                                      maxCloudCoverPercent=maxCloudCoverPercent)

print("The workspace has all the data needed to compare smoke-free vs.")
print("smoke-impacted ozone values")



################################################################################    
# Do the analysis using the created data environment
################################################################################  
print("About to create the distributions")
source("R/ozoneDistributionsAnalysis.R")


