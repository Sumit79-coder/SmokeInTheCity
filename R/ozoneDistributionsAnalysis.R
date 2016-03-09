# ozoneDistributionsAnalysis.R 

# This script was created with the intension of cleaning up ozoneDistributions.R 
# That script had a wierd mixture of doing grunt work and making analysis plots. 
# This script will be a reserved space for creating analysis figures and 
# performing analysis on the output of ozoneDistrions.R

source('R/ozoneDistributions.R')

print("ozoneDistributions.R has been sourced.")


################################################################################    
# Get output of ozone distributions and start analysis
################################################################################ 
if(makeDist){
  
  distributionDataSummary <- makeDistributionsBySmoke(workSpaceData=workSpaceData, 
                                                      figureDir=figureDir,
                                                      daysThreshold=daysThreshold,
                                                      TSdFactor=TSdFactor
                                                      )
  
  print("Finsished creating distribution summaries!")
  
  # retrieve returned output
  smokeOzoneMeans <- distributionDataSummary[["smokeImpactedOzoneMean"]]
  clearOzoneMeans <- distributionDataSummary[["smokeFreeOzoneMean"]]
  differenceOfMeans<-distributionDataSummary[["differenceOfMeans"]]
  lon             <- distributionDataSummary[["lon"]]
  lat             <- distributionDataSummary[["lat"]]
  signif          <- distributionDataSummary[["signif"]]
  cityName        <- distributionDataSummary[['cityName']]
  stationPlotted  <- distributionDataSummary[['stationPlotted']] 
  figureDir       <- distributionDataSummary[['figureDir']] 
  daysThreshold   <- distributionDataSummary[['daysThreshold']]
  nSmoky          <- distributionDataSummary[['nSmoky']]
  nStations       <- length(stationPlotted)
  
  confInt_lower <- distributionDataSummary[["confInt_lower"]]
  confInt_upper <- distributionDataSummary[["confInt_upper"]]
  pValue        <- distributionDataSummary[["pValue"]]
  tValue        <- distributionDataSummary[["tValue"]]
  
}

################################################################################ 
# Save this analysis data packet as a csv for sharing and plottig in other
# languages
################################################################################    
summary_df <- data.frame(stationPlotted=stationPlotted,
                         cityName=cityName,
                         lon=lon,
                         lat=lat,
                         daysThreshold=daysThreshold,
                         nSmoky=nSmoky,
                         smokeOzoneMeans=smokeOzoneMeans,
                         clearOzoneMeans=clearOzoneMeans,
                         differenceOfMeans=differenceOfMeans,
                         signif=signif,
                         confInt_lower=confInt_lower,
                         confInt_upper=confInt_upper,
                         pValue=pValue,
                         tValue=tValue
                         )

csvFileName <- paste0(figureDir,"summary_data.csv")
write.csv(summary_df, file=csvFileName)

makeAnalysisPlots <- function(makePlots){
  
  ################################################################################    
  # Create map of change 
  ################################################################################ 
  if(makePlots){
    print("making the first version of the change figure")
    
    change <- smokeOzoneMeans - clearOzoneMeans # the same as difference of Mean
    coords <- cbind(lon, lat)
    pointData <- data.frame(change=change, 
                            city=cityName, 
                            lon=lon, 
                            lat=lat)
    
    locations <- SpatialPointsDataFrame(coords, pointData,
                                        proj4string=CRS(as.character(NA)))
    
    
    Colors <- rep("black", nStations)
    smokedHigher <- smokeOzoneMeans > clearOzoneMeans
    
    
    Colors[smokedHigher & signif] <- "red"
    Colors[!smokedHigher & signif] <- "blue"
    
    plotSymbol <- rep(1,nStations) # open circle
    plotSymbol[smokedHigher & signif] <- 24  # up triangle
    plotSymbol[!smokedHigher & signif] <- 25 # down triangle 
    
    # Start the map image object   
    pdf(file=paste0(figureDir,"SignChange_map.pdf"), 
        width=10, height=6)
    
    par(mar=c(4,4,0,0))
    
    map("state")
    points(lon,lat, col=Colors, pch=plotSymbol)
    
    
    legendText <- c("Increase", "Decrease", "No difference")
    legend("bottomleft",
           legend=legendText,
           pch=c(24,25,1),
           col=c("red","blue","black"),
           box.lwd=0,
           cex=1.5,
           bty="n"
    )
    
    if(standAloneTitles){
      title("Response of mean ozone mixing ratio in the presense of smoke (May-Sep)")
      title(line=0.5,"Change stated with 95% Confidense")
    }
    
    dev.off()
  }
  
  
  ################################################################################    
  # Create a map showing magnitude of change in distributions 
  ################################################################################
  if(makePlots){
    
    pdf(file=paste0(figureDir,"changeMagnitude_map.pdf"),
        width=8, height=5)
  
    # Set the graphic parameters 
    par(mar=c(4,5,4,4))
    
    # label dz
    dc <- 5 
    
    # Plot the state map
    map("state")
    
    # How many monitors are we working with?
    nPlotted <- length(change)
    
    # Default closed, implies significant difference 
    pchs <- rep(19, nPlotted) 
    
    # Change the pch in locations where change is not significant 
    pchs[!signif] <- 15 # closed square
    
    # Colors near zero will be hard to see, so give all shapes an outline 
    pchs_outline <- rep(1, nPlotted)
    pchs_outline[!signif] <- 0
    
    # We want the colorramp to be symetric about zero, so it must go above and
    # below the same amount, the magnitude is set by the maximum value of the
    # absolute change. The dc * 2 nudge is to make sure that the scale goes
    # beyond where the +5 -5 nice label buffer may make edges
    maxMag <- max(abs(change)) + dc*2
    placedOnScale <- seq(-maxMag,maxMag, by=0.01)
    
    # Create the symetric color array
    Colors <- colorRampPalette(c("purple","blue","darkturquoise","white","orange" ,"red","firebrick"))(length(placedOnScale))
    
    # Give each change value a color based on where it falls on scale of possible
    # values stored in placedOnScale
    assignedColor <- rep(NA, nPlotted)
  
    # Assign colors based on location on the linear created scale
    for (i in 1:nPlotted){
      
      # Difference from all values on scale
      d <- placedOnScale - change[i]
      
      # What color is that associated with?
      minIndex <- which.min(abs(d)) 
      
      minD <- d[minIndex]
      if(minD > 0.01){
        stop("Bad color assignment")
      }
      
      
      # Assign that color to this change value
      assignedColor[i] <- Colors[minIndex]
    }
    
    # Plot the station locations with their assigned colors for magnitude
    points(locations, col=assignedColor, pch=pchs, cex=1.5)
    
    # Give each location an outline so they are easy to see
    points(locations, col="black", pch=pchs_outline, cex=1.5)
    
    # Only show the needed portion of the colorbar colors given nice round edges
    # That make a range that is divisible by 5 (dc argument)
    upperLimit <- ceiling(max(change))
    remainder <- upperLimit/dc - upperLimit %/% dc
    while(!remainder == 0){
      upperLimit <- upperLimit + 1
      remainder <- upperLimit/dc - upperLimit %/% dc
    }
    
    lowerLimit <- floor(min(change))
    remainder <- lowerLimit/dc - lowerLimit %/% dc
    while(!remainder == 0){
      lowerLimit <- lowerLimit - 1
      remainder <- lowerLimit/dc - lowerLimit %/% dc
    }
    
    # Create the labels
    zval <- seq(lowerLimit, upperLimit, by=dc)
    
    # These are the colors you want to keep for nice scale that has all values
    colorMask <- placedOnScale >= lowerLimit & placedOnScale <= upperLimit
    
    # Now subset the Colors by the desired limits
    Colors_subset <- Colors[colorMask]
    
    print("created the scale subset for sanity. Edgeds should match lowerLimit:upperLimit")
    scale_subset <- placedOnScale[colorMask]
    
    if(!length(scale_subset) == length(Colors_subset)){
      stop(" scale_subset and Colors_subset should be the same length")
    }
    
    print(paste("lowerLimit:", lowerLimit))
    print(paste("upperLimit:", upperLimit))
    
    print(paste("upper limted plotted:", max(scale_subset)))
    print(paste("lower limted plotted:", min(scale_subset)))
    
    print(paste("max change plotted:", max(change)))
    print(paste("min change plotted:", min(change)))
    
    # Make a colorbar legend showing the magnitude of the change 
    colorlegend(col = Colors_subset,
                zlim = c(lowerLimit, upperLimit),
                zval=zval,
                main="ppbv")
    

    # Place the nice title
    if(standAloneTitles){
      title("Response of mean ozone mixing ratio in the presense of smoke (May-Sep)")
    }
    
    legend(x=1.0,y=0.3,
           legend=c("significant", "not-significant"),
           pch=c(19,15),
           col="black",
           horiz=FALSE,
           bty="n", 
           cex=1
           #title="95% confidense"
           )
    
    dev.off()

  }
  
#   # Sanity plot. The zero value should be white, colors symetric about. 
#   plot(scale_subset, pch=19, col=Colors_subset)
#   abline(h=0)
#   abline(h=max(change))
#   abline(h=min(change))
#   colorlegend(col = Colors_subset,
#               zlim = c(lowerLimit, upperLimit),
#               zval=zval,
#               main="ppbv")
  
  ################################################################################    
  # Create a hist of the change in magnitude, urban vs. rural based on NOX Emis
  ################################################################################
  if(makePlots){
    
    # TODO: USE ALL LOCATIONS NOT JUST THE SIGNIFICANT 
    
    # The first thing to be done is to find out which stations are urban
    # NOTE: These NOX boundaries were created using R/createNOxRegions.R
    load("NEI/Top10PercentNOX.RData")
    locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))
    
    # Only consider locations where differences are significant
    # UPDATE: For the correction Emily and I decided this did not make  
    # UPDATE: sciencetific sense. Using only significant locaitons is going
    # UPDATE: to bias the comparison to bimodal distirbutions (significantly
    # UPDATE: positive and significantly negative.)
    ##locations <- locations[signif, ]
    index <- over(locations, Top10PercentNOX) # We want where it is NOT NA
    urbanMask  <- !(is.na(index))
    
    # Change in ozone in Highest Nox environs vs. lower Nox environs 
#     urbanChange <- smokeOzoneMeans[signif][urbanMask] - clearOzoneMeans[signif][urbanMask]
#     ruralChange <- smokeOzoneMeans[signif][!urbanMask] - clearOzoneMeans[signif][!urbanMask]  
    urbanChange <- smokeOzoneMeans[urbanMask] - clearOzoneMeans[urbanMask]
    ruralChange <- smokeOzoneMeans[!urbanMask] - clearOzoneMeans[!urbanMask]  
    
    
    # Are the distributions of the changes different? 
    urbanVsRuralStats <- differennceOfMeans(urbanChange, ruralChange)
    signifcantDifference <- urbanVsRuralStats[["signif"]]
    
    # Update the command line user
    print("-------------------------------------------------------")
    print(paste("The difference between the means is significant:", signifcantDifference))
    print("-------------------------------------------------------")
    
    # Estimate the densities for nice plotting
    urbanDensity <- density(urbanChange)
    ruralDensity <- density(ruralChange)

    # Find the max between the two density curves to set plot limit   
    maxes <- max( c(max(urbanDensity$y), max(ruralDensity$y))  )
    ylim  <- c(0,max(maxes) + .01) # .1 so that labels fit 
    
    
    # Begin making the figure
    pdf(file=paste0(figureDir,"highNOxLowNOxResponse.pdf"), 
        width=8, height=12)
    par(lwd=3, las=1, mfrow=c(2,1), mar=c(3,3,3,3))
    
    # Show how these stations are segregated
    map("state", lwd=1,mar=c(0,8,0,0) )
    transparentRed <- adjustcolor("orangered3", alpha.f=0.3)
    
    # Add the high NOx boxes
    plot(Top10PercentNOX, add=TRUE, col=transparentRed, border = "transparent")
    
    # Add the urban monitors 
    plot(locations[urbanMask], add=TRUE, pch=19, col="orangered3")
    
    # Add the rural monitors     
    plot(locations[!urbanMask], add=TRUE, pch=19, col="blue")
    #title(line=1.1,"Ozone monitor locations and top 10% NOx emitting regions")
    #title(line=0.2, "based on NEI 2008 July emissions")
    
    
    legend("bottomleft",
           legend=c(as.expression(bquote("high" ~ NO[.('x')] ~ "monitors")),
                    as.expression(bquote("low " ~ NO[.('x')] ~ "monitors"))
                    ),
           col=c("orangered3","blue"),
           pch=19,
           bty="n"
    )
    
    
    plot(urbanDensity,
         ylim=ylim,
         bty="n",
         xlab=as.expression(bquote("Change in" ~ O[.('3')] ~ "mixing ratio")),
         ylab="Density",
         #xlab="Change in ozone mixing ratio [ppbv]",
         #main="Response of mean ozone mixing ratio to smoke",
         main="",
         col="orangered3")
    
    lines(ruralDensity, col="blue")
    
    # How many of each are there?
    nR <- ruralDensity$n
    nU <- urbanDensity$n
    
    print(paste("number of rural monitors:", nR))
    print(paste("number of urban monitors:", nU))
    
    # TODO: subscript these X's
    urbanLab <- paste0("Urban (high NOx) n = ",nU)
    ruralLab <- paste("rural    (lower NOx) n = ",nR)
    
    # TODO: Get the number of urban vs. rural locations scripted!
    
    legend("topright",
           legend=c(urbanLab,
                    ruralLab)
           ,
           col = c("orangered3", "blue"),
           lty=1,
           bty="n"
    )
    
    # Label the mean value of each dist 
    urbanMeanChange <- mean(urbanChange)
    ruralMeanChange <- mean(ruralChange)
    
    
    text(x=urbanMeanChange, y=max(urbanDensity$y)+0.005,
         labels=paste0(round(urbanMeanChange,2)),
         col="orangered3"
    )
    
    text(x=ruralMeanChange-1.5, y=max(ruralDensity$y)+0.005,
         labels=paste0(round(ruralMeanChange,2)),
         col="blue"
    )
    
    #title(paste("Difference of mean significant?=:",signifcantDifference))
    
    dev.off()
    
    
  }
  
  ################################################################################    
  # Create a hist of the change in magnitude, urban vs. rural based census data 
  ################################################################################
  if(makePlots & FALSE){ # We are not using this figure for the paper
    
    # Load census GIS data 
    load("NEI/urbanPolygons.RData")
    coords <- cbind(lon,lat)
    locations <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))
    index <- over(locations, urbanPolygons)[,1] # We want where it is NOT NA
    mask  <- !(is.na(index))
    
    # Change in ozone in Highest Nox environs vs. lower Nox environs 
    urbanChange <- smokeOzoneMeans[mask] - clearOzoneMeans[mask]
    ruralChange <- smokeOzoneMeans[!mask] - clearOzoneMeans[!mask]
    
    urbanDensity <- density(urbanChange)
    ruralDensity <- density(ruralChange)
    
    maxes <- max( c(max(urbanDensity$y), max(ruralDensity$y))  )
    ylim  <- c(0,max(maxes))
    
    pdf(file=paste0("figures/urbanGISOzoneResponse_",dataSource,"_n=",daysThreshold,".pdf"),
        width=8, height=12)
    par(lwd=3, las=1, mfrow=c(2,1))
    
    # Show how these stations are segregated
    map("state", lwd=1,mar=c(0,0,5,0) )
    transparentRed <- adjustcolor("blue", alpha.f=0.9)
    plot(urbanPolygons, add=TRUE, col=transparentRed, border = "transparent")
    plot(locations[mask], add=TRUE, col="brown", cex=.5)
    plot(locations[!mask], add=TRUE, col="forestgreen", cex=0.5)
    title(line=1.1,"Ozone monitor locations and urban polygons")
    title(line=0.2, "based on 2013 census data")
    legend("bottomleft",
           legend=c("high NOx monitors", "Low NOx monitors"),
           col=c("brown","forestgreen"),
           pch=3,
           bty="n"
    )
    
    
    plot(urbanDensity,
         ylim=ylim,
         bty="n",
         xlab="Change in ozone mixing ratio [ppbv]",
         main="Response of mean ozone mixing ratio to smoke",
         col="brown")
    
    lines(ruralDensity, col="forestgreen")
    
    legend("topright",
           legend=c("Urban (high NOx)", "rural    (lower NOx)"),
           col = c("brown", "forestgreen"),
           lty=1,
           bty="n"
    )
    dev.off()
    
  }
  

  
  ################################################################################    
  # Show if the effect is a function of days
  ################################################################################
#   file <- "figures/effectvsDays.pdf"
#   if(!file.exists(file)){
#     pdf(file="figures/effectvsDays.pdf")
#     plot(nSmoky,change, xlab="smoky days",ylab="change from clear to smoky")
#     title("number of smoky days vs. effect ")
#     dev.off()
#   }
}

# Exexcute all the above plotting 
makeAnalysisPlots(makePlots)

print("Finished all plotting. Data pipeline complete.")

