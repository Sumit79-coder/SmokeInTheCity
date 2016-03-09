# creataSmokeFreeMask.R

# NOTE: This function was originally located in and developed in 
# NOTE: assignECMWFTDataToMontor.R , those interested in history of development
# NOTE: should look at that file. 


################################################################################  
# Now use temperature data to create a clear T mask that ensures
# smoke-free days are warmer than smoke-impacted days by TSdFactor. 
################################################################################  
createSmokeFreeTMask <- function(workSpaceData, 
                                 TSdFactor = 1,
                                 applySkyMask=FALSE, 
                                 maxCloudCoverPercent=10){
  
  # Get the required data 
  T_df            <- workSpaceData[["T_df"]]
  smokeImpactMask <- workSpaceData[["smokeImpactMask"]]
  
  # set dimensions for new temperature mask
  nMonitor    <- dim(T_df)[2]
  meanSmokedT <- rep(NA, nMonitor)
  sdSmokedT   <- rep(NA, nMonitor)
  smokeFreeMask <- smokeImpactMask # Copying for proper dimensions and labels
  
  # Loop through each monitor, figuring out the temperature threshold based
  # on arguments given to this function
  for (i in 1:nMonitor){
    
    # clear out smokeImpactMask data 
    smokeFreeMask[,i] <- FALSE # Assume FALSE until proven otherwise 
    
    # Which rows are smoke-impacted based on work so far? 
    smokedRows  <- smokeImpactMask[,i]
    smokedDaysT <- as.numeric(T_df[smokedRows,i])
    
    # Get the statistics on the smoke-impacted temperatures
    meanSmokedT[i] <- mean(smokedDaysT, na.rm=TRUE)  
    sdSmokedT[i]   <- sd(smokedDaysT, na.rm=TRUE)  
    
    # Figure out where the temperature is greater than smoky day average
    TThresh <- meanSmokedT[i] + sdSmokedT[i] * TSdFactor
    TCuttoffMask <- T_df[,i] >= TThresh
    
    
    if(is.nan(TThresh) & sum(smokedRows)==0){
      # There are no smoke impacted days, so all rows are smoke free. 
      # We know this because TThresh is not a number and there are zero smoked
      # rows. 
      smokeFreeRows <- rep(TRUE, length(smokedRows))
    } else {
      # There are smoke impacted days so we need to choose smoke-free carefully
      # Also, we want to be sure that these warm days are not also smoke-impacted!
      # NOTE: smokedRows == TRUE where smoke-impacted. Use ! to change those to
      # NOTE: FALSE and smoke-free days to TRUE
      smokeFreeRows <- !smokedRows & TCuttoffMask
      
      # TODO: Could add PM mask as well to ensure that clear days are not high
      # TODO: PM days. Require PM measurement for smokeFreeDays.
      
    }
    # Store the smokeFreeRows (days) information in TMask
    smokeFreeMask[,i] <- smokeFreeRows
    
  }
  
  # Include the smokeFreeMask in the workspace data 
  workSpaceData[["smokeFreeMask"]] <- smokeFreeMask
  workSpaceData[["meanSmokedT"]] <- meanSmokedT
  workSpaceData[["sdSmokedT"]] <- sdSmokedT
  
  # For testing purposes give this information back to console 
  print(paste("The sum of smokeFreeMask after T-Control is:", 
              sum(smokeFreeMask,na.rm=TRUE)))
  
  
  # Now apply the skycondition mask if desired
  if(applySkyMask){
    
    # Get the Cloud Cover Dataframe
    CC_df <- workSpaceData[["CC_df"]] * 100 # to make %
    
    # Where are the skies more clear than specified %?
    cloudFreeMask <- CC_df <= maxCloudCoverPercent
    
    # Add this cloudMask to the workspace
    workSpaceData[["cloudFreeMask"]] <- cloudFreeMask
    
    # Modify smokeFreeMask based on this new cloud condition
    smokeFreeMaskNew <- smokeFreeMask & cloudFreeMask
    
    # Overwrite the original smokeFreeMask 
    workSpaceData[["smokeFreeMask"]] <- smokeFreeMaskNew
    
    
    print(paste("The sum of smokeFreeMask after sky-control is:", 
                sum(smokeFreeMaskNew,na.rm=TRUE)))
    print("If the later is not small than the former, you have a problem.")
  }
  
  # Return the appended workSpaceData
  return(workSpaceData)
  
  
}