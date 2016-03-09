# compareData.R


################################################################################
# This function is used to compare the data of individual years and merged years
# for a given monitor and set of dates
# This is the look at all the data for a given species test. If you dont get a 
# message back grom the function, all the data appears to be matching. 

# Sample arguments
species <- "PM2.5_non_FRM_Mass"
stat    <- "Arithmetic.Mean"
sample.duration <- "24"
year <- 2009
################################################################################
compareYearlyToMergedData <- function(species=species,
                                      stat=stat,
                                      sample.duration=sample.duration,
                                      year=year){
  
  # load the individual year 
  singleYearBaseName <- paste0(species,"_",year,"_",stat,"_",sample.duration,"hr_t")
  singleYearFile <- paste0("timeSeries/individualYears/",singleYearBaseName,".RData")
  a_year <- get(load(singleYearFile))
  rm(list=singleYearBaseName)
  
  # Load the merged data
  allYearsBaseName <- paste0(species,"_",stat,"_",sample.duration,"hr_2005_2014")
  allYearsFile <- paste0("timeSeries/mergedYears/",allYearsBaseName,".RData")
  a_all <- get(load(allYearsFile))
  rm(list=allYearsBaseName)
  
  yearlyStations <- colnames(a_year)
  nStations <- length(yearlyStations)
  misMatch_ <- rep(NA, nStations)
  
  # evaulate the values from each station for this year
  for (i in 1:nStations){
    
    ID <- yearlyStations[i]
    
    # get the data from the yearly dataframe 
    stationTime_year   <- as.POSIXct(rownames(a_year), tz="MST")
    stationValues_year <- as.numeric(a_year[, colnames(a_year)==ID])
    
    
    pdf(file=paste0("dump/",ID,"_", year,".pdf"), width=9, height=5)
    plot(stationTime_year, stationValues_year, main=paste(ID, "year:",year), las=1, pch=19, col="red", 
         ylim=c(0,max(stationValues_year,na.rm=TRUE)+0.01))
    lines(stationTime_year, stationValues_year)
    abline(h=0.075, lty=2)
    
    # get data from merged years dataframe
    startDate <- stationTime_year[1]
    endDate <- stationTime_year[length(stationTime_year)]
    
    time_all <- as.POSIXct(rownames(a_all), tz="MST")
    time_all_mask <- time_all >= startDate & time_all <= endDate
    time_all_subset <- time_all[time_all_mask]
    all_col_mask <- ID == colnames(a_all)
    values_all <- as.numeric(a_all[time_all_mask, all_col_mask])
    
    points(time_all_subset, values_all, pch=19, col="blue")
    lines(time_all_subset, values_all, col="blue", lty=2)
    dev.off()
    
    
    # Are the values in these arrays equal? 
    # NOTE: NA will show up where the values are NA bceause NA==NA is NA
    equalTest <-  values_all == stationValues_year
    # Are they NA in the same places? 
    equalTestNA <- is.na(equalTest)
    
    NAAllProblem <- any(!equalTestNA == is.na(values_all)) 
    NAYearProblem <- any(!equalTestNA == is.na(stationValues_year))
    
    # if they are NA in the same places, see if any values of equal test == FALSE
    # where they are not NA
    if(NAAllProblem == FALSE & NAYearProblem==FALSE){
      misMatch <- any(!equalTest[!equalTestNA])
    } else {
      # if you come in here then there is a problem is NAs not being in the same
      # locations 
      misMatch <- TRUE
    }
    
    # store the result of misMatch
    misMatch_[i] <- misMatch
    
  }
  if(any(misMatch_)){
    print("one or more of the monitor data time series did not align")
  }
  
  return(mistMatch)
}
################################################################################

################################################################################
# This function is used to compare organized dataframes to those plotted
# here: http://www3.epa.gov/airdata/ad_viz_plotval.html
# This is the look at one at a time test. 
# NOTE: Low ozone values can sometimes be omitted from AQS online plotting 

# Sample arguments
ID="530630046-01"
startYear=2005
endYear=2013
species="O3" # "O3" | "PM2.5"
makePlot <- TRUE

################################################################################
plotMergedTimeSeriesData <- function(ID=ID,
                                     startYear=startYear,
                                     endYear=endYear,
                                     species=species){
  
  
  # Create time bookends
  startTime <- as.POSIXct(paste0(startYear,"-01-01"), tz="MST")
  endTime   <- as.POSIXct(paste0(endYear,"-12-31"), tz="MST")
  
  
  if(species=="PM2.5"){
    
    # See if this ID is in non_FRM, a kind of best guess 
    baseName <- "PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_2005_2014"
    df <- get(load(paste0("timeSeries/mergedYears/",baseName,".RData")))
    rm(list=baseName)
    dfNames <- colnames(df)
    
    # Load the metadata too (for plot labels check)
    metaBaseName <- "PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_metadata_2005_2014"
    mdf <- get(load(paste0("timeSeries/mergedYearsMetadata/",metaBaseName,".RData")))
    rm(list=metaBaseName)
    
    columnMask <- ID == dfNames
    if(sum(columnMask) == 0){
     
      # That monitor is not in non_FRM, load the FRM_FEM PM data instead
      baseName <- "PM2.5_FRM_FEM_Mass_Arithmetic.Mean_24hr_2005_2014"
      df <- get(load(paste0("timeSeries/mergedYears/",baseName,".RData")))
      rm(list=baseName)
      dfNames <- colnames(df)
      
      # Load the metadata too (for plot labels check)
      metaBaseName <- "PM2.5_FRM_FEM_Mass_Arithmetic.Mean_24hr_metadata_2005_2014"
      mdf <- get(load(paste0("timeSeries/mergedYearsMetadata/",metaBaseName,".RData")))
      rm(list=metaBaseName)
      
      # Is it in this one? 
      columnMask <- ID == dfNames 
      if(sum(columnMask) == 0){
        stop("No PM data found for this SITE ID + POC")
      }
      
    } # else you do not need to open the other file 
    
    # Subset by years specified in argument 
    t <- as.POSIXct(rownames(df), tz="MST")
    tMask <- t >= startTime & t <= endTime
  
    df_subset <- df[tMask, columnMask]
    
    # Get and plot the data 
    t_subset <- t[tMask]
    values   <- as.numeric(df_subset)
    
    # start plotting
    plot(t_subset, values, 
         main=paste(ID, startYear,"-",endYear, "PM2.5"), 
         las=1, pch=1, col="black", 
         ylim=c(0,max(values,na.rm=TRUE)+3))
    lines(t_subset, values)
    abline(h=35, lty=2)
    # TODO: Get the county and STATE information on the plot, + the lat and lon
    # TODO: to make sure all of it aligns with correct values 

    
  } # end of PM2.5 
  
  # If the user selects O3 this is where we go
  if(species=="O3"){
    
    baseName <- "Ozone_X1st.Max.Value_8hr_2005_2014"
    df <- get(load(paste0("timeSeries/mergedYears/",baseName,".RData")))
    rm(list=baseName)
    dfNames <- colnames(df)
    
    # Load the metadata too (for plot labels check)
    metaBaseName <- "Ozone_X1st.Max.Value_8hr_metadata_2005_2014"
    mdf <- get(load(paste0("timeSeries/mergedYearsMetadata/",metaBaseName,".RData")))
    rm(list=metaBaseName)
    
    # Subset by years specified in argument and monitor 
    columnMask <- ID == dfNames # ID should always be in this dataframe 
    t <- as.POSIXct(rownames(df), tz="MST")
    tMask <- t >= startTime & t <= endTime
    
    df_subset <- df[tMask, columnMask]
    
    # Get and plot the data 
    t_subset <- t[tMask]
    values   <- as.numeric(df_subset)
    
    # start plotting
    plot(t_subset, values, 
         main=paste(ID, startYear,"-",endYear,"O3"), 
         las=1, pch=1, col="black", 
         ylim=c(0, max(values, na.rm=TRUE) + 0.01))
    lines(t_subset, values)
    abline(h=0.075, lty=2)
    # TODO: Get the county and STATE information on the plot, + the lat and lon
    # TODO: to make sure all of it aligns with correct values 
    
  } # end of O3
  
  
  # Plot the metadata
  mdf_subset <- mdf[columnMask, ] # NOTE: columnMask is really stationMask, here thats rows
  CBSA <- mdf_subset$CBSA.Name
  County <- mdf_subset$County.Name
  State <- mdf_subset$State.Name
  Parameter <- mdf_subset$Parameter.Name
  
  title(line=0.5, paste0("CBSA:",CBSA, ", County:",County, ", State:",State))
  title(line=-1, paste("Parameter:", Parameter))
  
  
  
}
################################################################################

if(makePlot){ # execute the plotting audit function
  plotMergedTimeSeriesData(ID=ID,
                           startYear=startYear,
                           endYear=endYear,
                           species=species)
}







