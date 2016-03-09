# createNOxRegions.R

# This script will be used to read NEI NOx emissions data to create a GIS
# poygons around the U.S. to charactorize whether a locatoin is urban or rurual
# based on emissions. This will allow us to see how ozone concentrations respond
# to smoke in different NOx regimes. 


library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)
library(maps)
library(ncdf4)

# Open the NEI netcdf file 
ncFile <- paste0("NEI/NEI08_2010_1x1_Jul_wkday_regrid.nc")
nc     <- nc_open(ncFile)
lat    <- ncvar_get(nc, "lat") 
lon    <- ncvar_get(nc, "lon") 

# NOTE: The nc data does not provide whether or not the data is stored on a 
# NOTE: "generic 1x1 grid" 
# NOTE: http://acmg.seas.harvard.edu/geos/doc/archive/man.v9-02/appendix_2.html#Generic_1x1
# NOTE: For the current version of this code I have assumed a generic grid aka
# NOTE: grid point centers 

# For NO and NO2 we want to sum over the time dimension because we are simply
# trying to differentiate based on region percentile rank
NO  <- apply(ncvar_get(nc, "NO"), 1:2, sum) 
NO2 <- apply(ncvar_get(nc, "NO2"), 1:2, sum) 
NOX <- NO + NO2

# Subset the grid to be U.S. only 
lonMask <- lon > -125 & lon < -60 # Boundary moved west to include the westcaost
latMask <- lat > 15 & lat < 51

lat <- lat[latMask]
lon <- lon[lonMask]

NOX <- NOX[lonMask, latMask]

# This bit of code that removes the zero emission grid boxes is more conservative
# but is not what we described in the text. 
# # Get rid of zero values for computing percentiles
# NonZeroNOX <- NOX[NOX > 0]
# thresh <- quantile(NonZeroNOX,probs=0.90)

thresh <- quantile(NOX,probs=0.90)

# Update: 2016-01-13; looks good so far. Trying to explain shading in NOX boxes
urbanMask <- NOX >= thresh

polyCount <- 0 # Keep track of how many urban polygons we have
polyList  <- list() # list to store polygon coordinate segments

# NOTE: This should ONLY HAVE 1x1 degree boxes output 
for (i in 1:length(lat)){
  
  # For each lat consider every lon here
  for (j in 1:length(lon)){
   
    if(urbanMask[j,i]) { # if TRUE we need to make a polygon for this spot 
      
      polyCount <- polyCount + 1 
      

      # Netcdf files use midpoint of coordinates. See email correspondense with
      # ckeller@seas.harvard.edu
      
      # East-West extent    
      W <- lon[j] - 0.5 # left edge
      E <- W + 1        # right edge
      
      # North-South extent 
      N <- lat[i] + 0.5 # top edge
      S <- N-1          # bottom edge 
      
      # Create coords of box starting in NW corner, going clockwise and 
      # ending in the NEW corner
      plons <- c( W, E, E, W, W)
      plats <- c( N, N, S, S, N)
      
      coords <- cbind(plons, plats)
      
      Sr1 = Polygon(coords)
      
      # Store this object in a list 
      Srs1 = Polygons(list(Sr1), paste0("s",polyCount))
      
      polyList[[polyCount]] <- Srs1 
      
    }    
  }
}

# Length of polylist should be the same as number of TRUE in urbanMask
Top10PercentNOX = SpatialPolygons(polyList, 1:polyCount)

# Save a figure showing urban polygons and top 10% of NOx emitters based on NEI
pdf(file = "NEI/NOXRegines.pdf",
    width=10,
    height=6)
map("state")
plot(Top10PercentNOX, add=TRUE, col="red")

# Save the data 
save(Top10PercentNOX, file = "NEI/Top10PercentNOX.RData")


# Read in urban GIS data and save as RData for faster loading and analysis 
# Data url: https://www.census.gov/geo/maps-data/data/cbf/cbf_ua.html

layername <- "cb_2013_us_ua10_500k" # NOT USED anymore! Use /cb_2014_us_state_500k
localDir  <- "NEI"
urbanPolygons <- readOGR(dsn=localDir, layer=layername, verbose=FALSE)
proj4string(urbanPolygons) <- proj4string(Top10PercentNOX)

plot(urbanPolygons, add=TRUE, col="blue")

# Add a legend to the plotted GIS data
legend("bottomleft",
       legend=c("Top 10% NOx emitting gridboxes","2013 U.S. census urban area"),
       fill = c("red","blue")
       )
title("Spatial Segregation of Urban vs. rural polygons")
title(line=0, "EPA NEI July NOx emissions and 2013 census data")

dev.off()

# Save the 500k 2013 census data 
save(urbanPolygons, file="NEI/urbanPolygons.RData")







