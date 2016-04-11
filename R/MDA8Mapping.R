# MD80Mapping.R 


# This script will be used to calculate the number of MDA8 exceedances that 
# are smoke-impaced based on our definition.

library(shape)
library(openair)
library(maps)
library(sp)

limit <- 0.070 # ppmv

# All options for smokeMaskFiles will be located in /analysisData. 
SmokeMaskFile <- "PM2.5_non_FRM_Mass_24hr_Arithmetic.Mean_allSummers"      

# Load the data to be passed to plotting function
dataDir <- "analysisData/"
load(paste0(dataDir, SmokeMaskFile,".RData"))

# Function for finding number of exceedence days and the % that are smoked
percentSmokedExceedence <- function(workSpaceData=workSpaceData, limit=0.065){
  
  # Get needed data out of workspace 
  Hybrid_mdf <- workSpaceData[["Hybrid_mdf"]]
  Ozone_df   <- workSpaceData[["Ozone_df"]]
  smokeMask  <- workSpaceData[["smokeImpactMask"]]
  stations <- Hybrid_mdf$ID
  lon <- Hybrid_mdf$Longitude
  lat <- Hybrid_mdf$Latitude
  mdf <- Hybrid_mdf
  
  # NOTE: tz MST is to recognize that this date is assoicated with UTC-7, or a 
  # NOTE: date day behind 00Z
  ozoneTime <- as.POSIXct(rownames(Ozone_df), tz="MST")
  smokeTime <- as.POSIXct(rownames(smokeMask), tz="MST")
  
  # create Exceedense mask for each monitor with ozone data
  exceedanceMask <- Ozone_df > limit # in paper we use exceed this limit
  nExceed <- apply(exceedanceMask, 2, sum, na.rm=TRUE)     
  
  # Where do you exceed standard and have smoke present?
  smokedExceedanceMask <- exceedanceMask & smokeMask
  
  # Show total number of exceedances by station
  nSmoked <- apply(smokedExceedanceMask, 2, sum, na.rm=TRUE)
  
  percentSmoked <- as.numeric(nSmoked / nExceed)  * 100
  percentSmoked <- round(percentSmoked) 
  
  
  l <- list()
  l[['percentSmoked']] <- as.numeric(percentSmoked)
  l[['mdf']] <- mdf
  l[['nExceed']] <- as.numeric(nExceed)
  l[['time']] <- ozoneTime   # NOTE: This time series is important for figure 
                             # NOTE: caption and explaining why the climatoligies
                             # NOTE: are the way they are. 
  return(l)
  
}

# Will plot what is returned by percentSmokedExceedeence, executed within
plotPercentExceedence   <- function(limit, workSpaceData, pdf=TRUE){
  
  
  # Execute function() and extract data 
  data          <- percentSmokedExceedence(workSpaceData=workSpaceData, 
                                           limit=limit)
  mdf           <- data[['mdf']]
  percentSmoked <- data[['percentSmoked']]
  nExceed       <- data[['nExceed']]
  time          <- data[['time']]
  
  # Where nan divided by zero, no exceedence, nExceed = 0
  nanMask <- (is.nan(percentSmoked) )
  percentSmoked[nanMask] <- -1e6 # so they don't get colored
  
  # Get locations for plotting 
  lon <- mdf$Longitude
  lat <- mdf$Latitude
  
  
  ################################################################################
  # Create map showing exceedance % based on data type and threshold
  ################################################################################
  if(pdf){
    pdf(file=paste0("figures/Exceed_",limit*1000,"_",SmokeMaskFile,".pdf"),
        width=11, height=7)
    par(mar=c(5,4,0,4))
    inset <- c(0,-0.14)  
    legendCex <- 1.3
  
  } else { # make a png image instead
    png(filename=paste0("figures/Exceed_",limit*1000,"_",SmokeMaskFile,".png"),
        width=1000, height=1000*7/11
        )
    par(mar=c(4,4,4,0))
    inset <- c(0,-0.1) 
    legendCex <- 1.6
  }

  
  # Plot these numbers 
  map("state")
  
  # Place the no exceedence points on map
  points(lon[nanMask],lat[nanMask], pch=3)
  
  # Then place the places with exceedances on the map, open circle, color later
  points(lon[!nanMask],lat[!nanMask])
  
  nPoints <- length(percentSmoked)
  
  # NOTE: red4 and brown are really similar, will change to make ppt a bit more
  # NOTE: clear. brown has been replaced by lightsalmon3
  
  # Colors for % smoked bins 
  middleCols <- c("deepskyblue","forestgreen","darkorange","lightsalmon3","red")
  
  # 0% and 100% get their own color, done at the end

  colorLegendCols <- c("mediumorchid1", middleCols, "red4")
  
  # This creates 5 bins 
  breaks <- c(0,10,20,30,50,100)
  
  # Show these 5 middle bin labels and the special end labels 
  colorLegendText <- c("0%", 
                       "1-10%", "11-20%","21-30%", "31-50%", "51-99%",
                       "100%")
  
  # Assign the middle colors based on bins
  binColor <- rep(NA, nPoints)
  
  # Assign the colors 
  col <- rep(NA, nPoints)
  nBreakVakues <- length(breaks)
  for (i in 1:(nBreakVakues-1) ){
    
    # Show the limits being used
    print(paste("lower=",breaks[i], "upper=",breaks[i+1]))
    mask <- percentSmoked > breaks[i] & percentSmoked <= breaks[i+1]
    
    print(paste("min in bin:", min(percentSmoked[mask])))
    print(paste("max in bin:", max(percentSmoked[mask])))
    
    # Show how many monitors meet this criteria
    paste(print(paste("total monitors in bin:", sum(mask, na.rm=TRUE))))
    
    # Assign the middleCols, because that is the bins we are looping through
    col[mask] <- middleCols[i]
    
  }
  
  # Special color for where exceedances are all smokey days 
  col[percentSmoked==100] <- colorLegendCols[length(colorLegendCols)]
  
  # Special color for when no exceedance is smoky
  col[percentSmoked == 0] <- colorLegendCols[1]
  
  ##############################################################################
  # Place the color legend associated with % of exceedance smoke-impacted
  ##############################################################################
  legend("bottomleft",
         legend=colorLegendText,
         col=colorLegendCols,
         pch=19,
         bty="n",
         cex=legendCex,
         title="% smoke-impacted",
         inset=inset,
         xpd=TRUE
  )
  
  ##############################################################################
  # Make the size of the dot proportional to how many exceedance days are on 
  # record for a given location. Make bins
  ##############################################################################
  assignedCex <- rep(NA, length(nExceed))
  
  # Creates 5 bins in middles, these are exceedance days!  
  cexBins <- c(1,10,20,30,50,100)
  nBins <- length(cexBins)
  
  # Including the over 100 category
  cexValues <- seq(1,3, length.out=nBins)
  
  for (i in 1:(nBins-1)){
    
    print(paste("lowerLimit=",cexBins[i]))
    print(paste("upperLimit=",cexBins[i+1]))
    
    cexMask <- nExceed >= cexBins[i] & nExceed < cexBins[i+1]
    print(paste("min in bin", min(nExceed[cexMask])))
    print(paste("max in bin", max(nExceed[cexMask])))
    
    assignedCex[cexMask] <- cexValues[i] 
  }
  
  # Assign the cex of the zero and over 100 days category
  assignedCex[nExceed == 0] <- 0 # these will be shown with a different symbol 
  assignedCex[nExceed >= 100] <- cexValues[i + 1]
  
  # Place points on the map. Place the larger points first, then add smaller 
  # as you go so they are  all visible 
  nCexValues <- length(cexValues)
  for (j in rev(1:nCexValues)){
    
    # Plot this cexValue only, these points only! 
    print(paste("plotting cex values = ", cexValues[j]))
    cexMask <- cexValues[j] == assignedCex
    
    # Plot with the approprite col and cex
    points(lon[cexMask],lat[cexMask], 
           col=col[cexMask], 
           pch=19, 
           cex=assignedCex[cexMask])
    
  }
  
  # The symbol tells us about exceedances
  #              none, some amount 
  assignedPch <- c(3,rep(19, nCexValues))
  
  # The size of the dot tells us about the # of exceedances, the no exceedance
  # locations have a different symbol and should have cex==1
  legendCex <- c(1,cexValues)
    
  ##############################################################################
  # Size legend 
  ##############################################################################
  sizeLegend <- c("0 Days", "1-10","11-20","21-30","31-50","51-100", " >= 100")
  legend("bottomright",
         legend=sizeLegend,
         pt.cex=legendCex,
         col="black",
         pch=assignedPch,
         bty="n",
         cex=1,
         title=paste("days MDA8 >", limit*1000,"ppbv"),
         inset=c(0,-0.08),
         xpd=TRUE)
  
  # Emily thinks this information should just be in the figure caption 
  
#   # Main title
#   title(paste("Smoke presence in days MDA8 >", limit*1000, "ppbv"),cex.main=2)
  
#   # Sub title explaining days used
#   if(pdf){
#     title(line=-28,paste(length(time), "monitored summer days used"))
#   } else{
#     title(line=-35,paste(length(time), "monitored summer days used"))
#   }
  
  
  dev.off()
  
}

# Make a plot showing the % days influenced by smoke for a given limit
plotPercentExceedence(limit=limit, workSpaceData=workSpaceData, pdf=TRUE)

################################################################################
# Make a plot showing how impact of smoke changes if limit lowed 
################################################################################

changeInLimit <- function(oldLimit=0.075, 
                          newLimit=0.070, 
                          workSpaceData=workSpaceData,
                          SmokeMaskFile=SmokeMaskFile){
  
  # get output from two limits of interetsed
  oldLimit_df  <- percentSmokedExceedence(workSpaceData=workSpaceData, limit=oldLimit)
  newLimit_df  <- percentSmokedExceedence(workSpaceData=workSpaceData, limit=newLimit)
  
  # Get station metedata
  mdf <- newLimit_df[['mdf']]
  lon <- mdf$Longitude
  lat <- mdf$Latitude
  
  percentSmokedOld <- oldLimit_df[['percentSmoked']]
  # nan for this means zero exceedences. this effectively means 0% as far as 
  # comparing to a new standard 
  percentSmokedOld[is.nan(percentSmokedOld)] <- 0
  
  # Now load the new standard data 
  percentSmokedNew <- newLimit_df[['percentSmoked']]
  # nan for this means zero exceedences. this effectively means 0% as far as 
  # comparing to old standard 
  #percentSmokedNew[is.nan(percentSmokedNew)] <- 0
  
  # nan means no exceedances at the new level
  changeInPercent <- percentSmokedNew - percentSmokedOld
  
  changeInNExceed <- newLimit_df[['nExceed']] - oldLimit_df[['nExceed']]
  
  # Where are there never any violations? (same mask twice)
  nonViolaterMask <- newLimit_df[['nExceed']] == 0 | is.nan(changeInPercent)
  
  ##############################################################################
  # Create map showing exceedance % based on data type and threshold
  ##############################################################################
  pdf(file=paste0("figures/changeLimit",oldLimit,"_to_",newLimit,"_",SmokeMaskFile,".pdf"),
      width=11, height=7)
  
  par(mar=c(6,4,4,4))
  
  
  dotCex <- 1
  
  # Plot these numbers 
  map("state")
  
  # Place the no exceedence points on map
  #points(lon[nonViolaterMask],lat[nonViolaterMask], pch=3)
  #points(lon[!nonViolaterMask],lat[!nonViolaterMask])
  
  nPoints <- length(lon)
  
  # for change in percent of days smoked ()
  breaks <- c(-1e6, -25, -15 , -5 , 0, 5, 15, 25, 1e9) # 8 bins
  colorLegendText <- c("<= -26", "-25:-16", "-15:-6","  -5:0",
                       "    0:5","    6:15","  16:25","  >= 26") # length 8 
  
  # Bin the percentages 
  colorOptions <- rev(rainbow(length(colorLegendText)))
  
  nCol <- length(colorOptions)
  nGaps <- length(breaks) - 1
  
  # Assign the colors 
  assignedColor <- rep(NA, nPoints)
  for (i in 1:nGaps){
    
    print(paste(breaks[i], "to" , breaks[i+1]))
    mask <- changeInPercent > breaks[i] & changeInPercent <= breaks[i+1]
    
    # Show what these values are
    print(paste("min changeInPercent",min(changeInPercent[mask],na.rm=TRUE)))
    print(paste("max changeInPercent",max(changeInPercent[mask],na.rm=TRUE)))
    
    # Assign the appropriete color 
    assignedColor[mask] <- colorOptions[i]
  }
  
  # NOTE: col[is.na(changeInPercent)] == NaN, no exceedance locations
  
  # Color legend
  legend("bottomleft",
         legend=colorLegendText,
         col=colorOptions,
         pch=19,
         bty="n",
         cex=dotCex,
         pt.cex=2,
         title=paste("(% smoked @", oldLimit*1000,") -(% smoked @ ",newLimit*1000,")"),
         xpd=TRUE,
         inset=c(0,-0.1)
  )
  
  # Make the size of the dot proportional to how many exceedance days are on 
  # record for a given location. Make bins
  assignedCex <- rep(NA, nPoints)
  
  cexBins <- c(0, 1, 10, 20, 30, 50, 75, 100, 200, 1e6) # 9 bins
  nBins   <- length(cexBins) - 1 
  cexValues <- seq(1,3.5, length.out=(nBins))
  
  cexLegend <- c("0 @ 75 or 70", "0:10","11:20","21:30","31:50","51:75","76:100",
                 "101:200", ">= 201")
  
  for (i in 1:nBins){
    
    print(paste(cexBins[i], "to", cexBins[i+1]))
    cexMask <- changeInNExceed >= cexBins[i] & changeInNExceed < cexBins[i+1]
    
    print(paste("min changeInNExceed:", min(changeInNExceed[cexMask])))
    print(paste("max changeInNExceed:", max(changeInNExceed[cexMask])))
    
    assignedCex[cexMask] <- cexValues[i] 
    
  }

  # Place points on the map 
  # TODO: Place the larger points first, then add smaller as you go so they are 
  # TODO: all visible 
  for (j in rev(1:nBins)){
    
    # Devide which locations to plot, starting with largest assigned cex
    cexMask <- cexValues[j] == assignedCex
    
    if(cexValues[j] == 1){ # No exceedances here. 
      points(lon[cexMask],lat[cexMask], col="black", pch=3, cex=assignedCex[cexMask])
    } else { # give a size to number of extra exceedances
      points(lon[cexMask],lat[cexMask], col=assignedColor[cexMask], pch=19, cex=assignedCex[cexMask])
    }
    
  }

  pchLabel <- c(3, rep(19, length(cexValues)-1))
  # Size legend 
  legend("bottomright",
         legend=cexLegend,
         pt.cex=cexValues,
         col="black",
         pch=pchLabel,
         bty="n",
         cex=1,
         title="Additional Exceedance Days",
         xpd=TRUE,
         inset=c(0,-0.1))
  
#   # Main title
#   title(paste("Change in the number of MDA8 exceedence days"),cex.main=2)
#   title(line=0, paste("and % of exceedence days smoke-influenced"),cex.main=2)
  

  dev.off()
  
  
  
}

changeInLimit(oldLimit=0.075, newLimit=0.070, workSpaceData=workSpaceData,
              SmokeMaskFile=SmokeMaskFile)





