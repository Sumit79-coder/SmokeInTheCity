# ozoneDistributions.R

# The goal of this script is to explore ozone distributions subset by 
# smoke-impact. The idea is to see if ozone distributions are different for 
# smokeImpacted vs. smoke-free days. Arguments specified in 
# R/runAnalysisPipeline.R determine what is defined as smoke-impacted vs. 
# smoke-free.

# Required libraries that have not been loaded yet
library(sp)
library(stringr)
library(shape)

print("About to source ozoneDistributions.R")

################################################################################
# Function for removing linear trend from ozone data 
################################################################################
detrendData <- function(t,y){

  # Make sure there is enough data to detrend
  nPoints <- length(unique(y))
  if(nPoints > 2){ # because NaN and NA are the two unique were there could be no data
  
    # function for detrending a time series with missing data and uneven spacing
    NAMask      <- is.na(y)
    ySubset <- y[!NAMask]
    tSubset  <- t[!NAMask]
    fit <- lm(ySubset ~ tSubset)
    
    y_detrend <- (ySubset - as.numeric(fit$fitted.values)) + mean(fit$fitted.values)
    
    # Now we want to return arrays that are the same dimensions as those passsed
    t[!NAMask] <- tSubset
    y[!NAMask] <- y_detrend
    
    # Create a list and return the de-trended data 
    l <- list()
    l[["t"]] <- t
    l[["y"]] <- y
    
  } else{ # not enough data, pass back the nothing as it was given1
    
    l <- list()
    l[["t"]] <- t
    l[["y"]] <- y
  }
  
  return(l)
  
}



################################################################################    
# Removes illegal charactors from citys names so they can be used as save file
# names. Nice but city names are no longer used for file save names.  
################################################################################    
removeIlligal <- function(s){
  
  s <- str_replace(s, "/", " ")
  s <- str_replace(s, ",", " ")
  return(s)
  
}


################################################################################    
# Calculates whether two arrays means are statistically different t-test
################################################################################    
differennceOfMeans <- function(smokeOzone, clearOzone){
  
  # takes arrays of ozone data in the presense and not in presense of smoke
  # and checks to see if the distribution means are from the same population. 
  
  # Using the built in t-test instead
  tTest <- t.test(smokeOzone, clearOzone, alternative="two.sided")
  pValue <- tTest$p.value
  
  # RECAL: "A small p-value (≤ 0.05) indicates strong evidence against the null 
  #         hypothesis, so it is rejected."
  #         Ho: Difference of means is 0.
  #         H1: Difference of the means is not equal to 0.
  signif <- pValue <= 0.05
  
  # Lets also return the tValue and the 95% confidense interval. This will make
  # the health assesment study easier to relate to analysisPipelineArguments
  confInt <- as.numeric(tTest$conf.int)
  tValue  <- as.numeric(tTest$statistic)
  
  l <- list()
  l[["signif"]] <- signif
  l[["pValue"]] <- pValue
  l[["confInt_lower"]] <- confInt[1]
  l[["confInt_upper"]] <- confInt[2]
  l[["tValue"]] <- tValue
  
  
  
  return(l)
  
}


################################################################################    
# Removes NA values from arrays and DOES NOT replace them with anything 
################################################################################    
remove_NA <- function(a){
  # Very basic function for removing NA values for an array and returns
  # the array with no NA! If there are NA values in an array this will return
  # an NA free array that is shorter than the one supplied. 
  a <- a[!is.na(a)]
  return(a)
}


################################################################################    
# Create and compare distributions 
################################################################################    
makeDistributionsBySmoke <- function(workSpaceData,
                                     figureDir=figureDir,
                                     daysThreshold=30,
                                     TSdFactor=1
                                     ){
  
  print(paste("analysisFigures will be saved in", 
              figureDir))
  
  stationPlotsDir <- paste0(figureDir,"stationPlots/")
  
  # If the directory does not exist, create it
  if(!file.exists(stationPlotsDir)){
    dir.create(stationPlotsDir)
  }
  
  
  # Load the workspace data required for analysis
  Ozone_df        <-  workSpaceData[["Ozone_df"]]
  PM_df           <-  workSpaceData[["PM_df"]]
  Hybrid_mdf      <-  workSpaceData[["Hybrid_mdf"]]
  PM_mdf          <-  workSpaceData[["PM_mdf"]]
  smokeImpactMask <-  workSpaceData[["smokeImpactMask"]]
  lat             <-  workSpaceData[["lat"]]
  lon             <-  workSpaceData[["lon"]]
  city            <-  workSpaceData[["city"]]
  cityName        <-  workSpaceData[["cityName"]]
  T_df            <-  workSpaceData[["T_df"]]
  smokeFreeMask   <-  workSpaceData[["smokeFreeMask"]] # accounts for clouds and T as argued
  
  # Make distinction between monitor ID and station ID for each species, then
  # do a sanity check that PM and ozone agree
  OzoneMonitors <- Hybrid_mdf$ID
  PMMonitors    <- PM_mdf$ID
  
  # Do these match the column names of the associated data?
  if(any(!colnames(Ozone_df) == OzoneMonitors)){stop("O3 df and mdf do not match")}
  if(any(!colnames(PM_df) == PMMonitors)){stop("PM df and mdf do not match")}
  
  OzoneStations <- str_sub(OzoneMonitors,1,9)
  PMStations    <- str_sub(PMMonitors,1,9)
  nStations     <- length(OzoneStations)
  
  # NOTE: These two station lists MUST be identical in order for this to work
  # NOTE: so check. If we made it this far, we already know the metadata matches
  sameStationsMask <- OzoneStations == PMStations
  if(any(sameStationsMask==FALSE)){
    stop("PM and ozone data is not aligned, cannot perform analysis.")
  } else {
    stations <- OzoneStations
  }
  
  # Get the time
  time <- workSpaceData[["time"]]
  ozoneTime <- as.POSIXct(rownames(Ozone_df), tz="MST")
  sameTimeMask <- time == ozoneTime
  if(any(sameTimeMask==FALSE)){
    stop("Something is wrong, ozone time and stored time do not match")
  }
  
  
  # detrend the data so the trends do not bias the distributions 
  OzoneNoTrend_df <- Ozone_df
  for (i in 1:nStations){
    OzoneNoTrend_df[,i] <- detrendData(time, Ozone_df[,i])[["y"]]
  }
  # Store the detrended data 
  workSpaceData[["OzoneNoTrend_df"]] <- OzoneNoTrend_df
  
  # Find out which stations have enough smoke-impacted days to perform analysis
  smokeDays <- as.numeric(apply(smokeImpactMask, 2,sum))
  stationsWithEnoughData <- sum(smokeDays >= daysThreshold)
  print(paste("Number of stations smoke-impaced days:", stationsWithEnoughData))
  

  # Create arrays to store information about plotted stations
  # TODO: Make these names better 
  ozoneMonitorPlotted <- character(0)
  smokeImpactedOzoneMean <- numeric(0)
  smokeFreeOzoneMean  <- numeric(0)
  nSmoky     <- numeric(0) # smoky days with data rather than sum of smokeImpact mask
  differenceOfMeans <- numeric(0)
  signif     <- logical(0)
  pValue <- numeric(0)
  confInt_lower <- numeric(0)
  confInt_upper <- numeric(0)
  tValue <- numeric(0)
  
  # Loop through the stations looking for ones with enough smoked days to 
  # create a separate distribution 
  for (i in 1:nStations){
    
    # Get a nice name for city
    city[i] <- removeIlligal(city[i])
    
    # Get this stations data and masks 
    stationOzone <- OzoneNoTrend_df[,i] * 1000 # convert ppmv to ppbv
    stationT     <- T_df[,i]
    stationSmokeMask  <- smokeImpactMask[,i]
    stationSmokeFreeMask <- smokeFreeMask[,i] 
    
    # Apply smokeMask to segregate station data days between days with smoke
    # and days without smoke. 
    smokeImpactedOzone <- as.numeric(stationOzone[stationSmokeMask])
    
    # Get smoke-free ozone values 
    smokeFreeOzone <- as.numeric(stationOzone[stationSmokeFreeMask])
      
    # get rid of NA values for statistics 
    smokeImpactedOzone <- remove_NA(smokeImpactedOzone)
    smokeFreeOzone <- remove_NA(smokeFreeOzone)
    
    ############################################################################    
    # If you have enough data, plot the smoked vs. clear distributions
    ############################################################################    
    if( length(smokeImpactedOzone) >= daysThreshold & 
        length(smokeFreeOzone) >= daysThreshold) {
      
      # Append needed values 
      ozoneMonitorPlotted <- append(ozoneMonitorPlotted, OzoneMonitors[i])
      smokeImpactedOzoneMean <- append(smokeImpactedOzoneMean, mean(smokeImpactedOzone))
      smokeFreeOzoneMean <- append(smokeFreeOzoneMean, mean(smokeFreeOzone))
      
      # Calculate densities 
      smokeImpactedDensity <- density(smokeImpactedOzone)
      smokeFreeDensity <- density(smokeFreeOzone)
      
      # Calculate the difference of the means 
      meanDiff <- mean(smokeImpactedOzone) - mean(smokeFreeOzone)
      differenceOfMeans <- append(differenceOfMeans,meanDiff)  
        
      # Calculate the significance of the difference in the sample means and 
      # other relevant statistics
      statSummary <-  differennceOfMeans(smokeImpactedOzone, smokeFreeOzone)
      
      # Append these stats to the summary arrays 
      result <- statSummary[["signif"]] # for immediate use
      signif <-  append(signif, result )
      pValue <-  append(pValue, statSummary[["pValue"]])
      confInt_lower <- append(confInt_lower, statSummary[["confInt_lower"]])
      confInt_upper <- append(confInt_upper, statSummary[["confInt_upper"]])
      tValue <- append(tValue, statSummary[["tValue"]])
      
      # How many smoke-impacted days?
      nSmoky <- append(nSmoky,  length(smokeImpactedOzone))
      
      ##########################################################################    
      # Plot all the time series and map
      ########################################################################## 
      if(TRUE){
        
      pdf(file=paste0(figureDir,"stationPlots/",stations[i] ,".pdf"),
          height=46, width=10)
      par(mfrow=c(8,1), las=1)
      
      
      ##########################################################################    
      # Plot this stations location on a map
      ########################################################################## 
      map("state")
      points(lon[i], lat[i], pch=19,cex=1)
      points(lon[i], lat[i], pch=3,cex=3)
      
      
      ##########################################################################    
      # Plot the Ozone data (No trend)
      ##########################################################################    
      ozoneColor <- rep("black", length(stationOzone))
      ozoneColor[stationSmokeMask] <- "red"
      ozoneColor[stationSmokeFreeMask] <- "blue"
      
      plot(time, stationOzone, 
           col=ozoneColor,
           main=paste(OzoneMonitors[i],city[i], lon[i],":", lat[i]), 
           ylab="O3", 
           pch=19,
           bty="n")
      
      title(line=0.3, "Linear trend removed")
      legend("topright", 
             legend=c("smoke-impacted", "smoke-free", "niether"),
             col=c("red", "blue", "black"),
             pch=19,
             bty="n"
      )
      
      ##########################################################################    
      # Plot the Ozone data raw
      ##########################################################################    
      ozoneColor <- rep("black", length(stationOzone))
      ozoneColor[stationSmokeMask] <- "red"
      ozoneColor[stationSmokeFreeMask] <- "blue"
      
      plot(time, Ozone_df[,i], 
           col=ozoneColor,
           main=paste(OzoneMonitors[i],city[i], lon[i],":", lat[i]), 
           ylab="O3", 
           pch=1,
           bty="n")

      title(line=0.3, "Raw ozone data")
      legend("topright", 
             legend=c("smoke-impacted", "smoke-free", "niether"),
             col=c("red", "blue", "black"),
             pch=19,
             bty="n"
      )
      abline(h=0.075, lty=2, lwd=2)
      ##########################################################################    
      # Plot the PM time series 
      ##########################################################################    
      PMColors <- rep("black", length(stationOzone))
      PMColors[stationSmokeMask] <- "red"
      PMColors[stationSmokeFreeMask] <- "blue"
      
      plot(time, PM_df[,i], 
           col=PMColors,
           main=paste("PM2.5", PMMonitors[i], PM_mdf$CBSA.Name[i]), 
           ylab="ug/m3",
           pch=19,
           bty="n")
      
      legend("topright", 
             legend=c("smoke-impacted", "smoke-free", "niether"),
             col=c("red", "blue", "black"),
             pch=19,
             bty="n"
      )
      
      ##########################################################################    
      # Plot station Temperature time series
      ##########################################################################    
      plot(time, stationT, main="00Z Temperature (late afternoon)", 
           ylab="F",
           col=PMColors,
           pch=19,
           bty="n")
      
      # Show the minimum temperature line for the smoke-free days
      meanSmokedT <- as.numeric(mean(stationT[stationSmokeMask], na.rm=TRUE))
      sdSmokedT <- as.numeric(sd(stationT[stationSmokeMask], na.rm=TRUE))
      TThresh <- meanSmokedT + sdSmokedT * TSdFactor
      
      # Show the mean temp of smoke impacted and the thresh
      # NOTE: Make the lty different because if TSdFactor=0 then they are same
      abline(h=TThresh, col="blue", lwd=2, lty=4)
      abline(h=meanSmokedT, col="red", lwd=2, lty=2)
      
      ##########################################################################    
      # Plot the Masks 
      ########################################################################## 
      plot(time, stationSmokeMask,
           col=PMColors,
           main="Masks",
           pch=1,
           bty="n")
      points(time, stationSmokeFreeMask, pch=3, lwd=2, col=PMColors)
      legend("center",
             legend=c("SmokeMask","SmokeFreeMask"),
             pch=c(1,3),
             col=c("red","blue"),
             bty="n")
      
      ##########################################################################    
      # Plot the density estimates
      ########################################################################## 
      
      # Set the ylimit for the plot 
      maxes <- c(max(smokeImpactedDensity$y), max(smokeFreeDensity$y))
      ylim=c(0, max(maxes))
      
      # Set the xlimit for the plot 
      xmaxes <- c(max(smokeImpactedDensity$x), max(smokeFreeDensity$x))
      xmins  <- c(min(smokeImpactedDensity$x), min(smokeFreeDensity$x))
      xlim=c(min(xmins), max(xmaxes))
      

      plot(smokeFreeDensity, col="blue", 
           lwd=4, main="", 
           sub="", 
           bty="n",
           xlab="Ozone [ppbv]",
           ylim=ylim,
           xlim=xlim)
      lines(smokeImpactedDensity, col="red", lwd=4)
      title(paste("Differene is significant?:",result, "value:",meanDiff))
      
      ozoneDist <- paste("Clear ozone Dist n =", smokeFreeDensity$n)
      highOzoneDist <- paste("Smoked ozone Dist n =", smokeImpactedDensity$n)
      legend("topleft",
             legend=c(ozoneDist, highOzoneDist),
             col=c("blue", "red"),
             lty=1,
             lwd=6,
             bty="n"
      )
      
      # Now plot the temperature density curves 
      # UPDATE: removing NA values for smokeImpactedTDensity for this 
      # UPDATE: histagrame like plot 
      TSmokeFreeForDensity <- remove_NA(stationT[stationSmokeFreeMask])
      TSmokeImpactedForDensity <- remove_NA(stationT[stationSmokeMask])
      
      smokeFreeTDensity <- density(TSmokeFreeForDensity)
      smokeImpactedTDensity <- density(TSmokeImpactedForDensity)
      
      # Set the ylimit for the plot 
      maxes <- c(max(smokeFreeTDensity$y), max(smokeImpactedTDensity$y))
      ylim=c(0, max(maxes))
      
      # Set the xlimit for the plot 
      xmaxes <- c(max(smokeFreeTDensity$x), max(smokeImpactedTDensity$x))
      xmins  <- c(min(smokeFreeTDensity$x), min(smokeImpactedTDensity$x))
      xlim=c(min(xmins), max(xmaxes))
      
      plot(smokeFreeTDensity, col="blue", 
           lwd=4, main="", 
           sub="", 
           bty="n",
           xlab="Temperature [f]",
           ylim=ylim,
           xlim=xlim)
      lines(smokeImpactedTDensity, col="red", lwd=4)
      
      # Make a legend for temperature density plot 
      ozoneDist <- paste("Clear T Dist n =", smokeFreeTDensity$n)
      highOzoneDist <- paste("Smoked T Dist n =", smokeImpactedTDensity$n)
      legend("topleft",
             legend=c(ozoneDist, highOzoneDist),
             col=c("blue", "red"),
             lty=1,
             lwd=6,
             bty="n"
      )
      
      dev.off() # end of station plot 
      }
      
      
    } # end of checking for enough data
    
    
  } # end of looping through stations
  
  
  # Pass through only the stations that met the criteria specified by the user
  # where do there plotted monitor IDs land index-wise in OzoneMonitors?
  plottedStationMask <- match(ozoneMonitorPlotted, OzoneMonitors)
  lonSubset <- lon[plottedStationMask]
  latSubset <- lat[plottedStationMask]
  cityName  <- cityName[plottedStationMask]
  
  # return these parameters for analysis plotting
  l <- list()
  l[['smokeImpactedOzoneMean']] <- smokeImpactedOzoneMean
  l[['smokeFreeOzoneMean']] <- smokeFreeOzoneMean
  l[["differenceOfMeans"]] <- differenceOfMeans
  l[['lon']] <- lonSubset
  l[['lat']] <- latSubset
  l[['cityName']] <- cityName
  l[['stationPlotted']] <- ozoneMonitorPlotted
  l[['figureDir']] <- figureDir 
  l[['daysThreshold']] <- daysThreshold
  l[['nSmoky']] <- nSmoky
  l[['signif']] <- signif
  l[['pValue']] <- pValue
  l[["confInt_lower"]] <- confInt_lower
  l[["confInt_upper"]] <- confInt_upper
  l[["tValue"]] <- tValue
  
  return(l)
}

