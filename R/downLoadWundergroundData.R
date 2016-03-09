# downLoadWundergroundData.R

library(weatherData)
library(RCurl)

################################################################################
# THIS IS WHERE I WANT TO MAKE SURE THE DATA IS GOOD. NEED TO FIGURE OUT TIME 
# ZONE OF DATE AND TIME GIVEN WITH THESE DATA

# NOTE: There are two types of weather stations IDs. There is the ID for airports
# NOTE: and for all 3-letter stations
# http://www.wunderground.com/about/faq/US_cities.asp?MR=1
# http://weather.rap.ucar.edu/surface/stations.txt
# getStationCode("Athens", region="GA") # in the US State of Georgia
# In case you are short on internet while traveling download all the available 
# met data
# For years 2005:2014 # to match hms PM subset data 
################################################################################
downLoadWundergroundData <- function(startIndex=1, endIndex=1600){
  
  # startIndex allows me to pick up where I left off when trying to get all
  # the airport data
  data(USAirportWeatherStations)
  df <- USAirportWeatherStations
  
  years <- 2005:2014 # These are the years I have hms data for 
  
  useIndex <- startIndex:endIndex
  
  for (stationCode in df$airportCode[useIndex]){ # all 4 letter airport codes are unique
    
    startIndex <- startIndex + 1
    
    # Loop through years requesting all data in each
    print(paste("starting to loop through years for",stationCode))
    for (year in years){
      
      print(year)
      
      startDate <- paste(year,"01-01",sep="-")
      endDate   <- paste(year,"12-31",sep="-")
      
      # build the data url: Requires airport code and year of interest
      urlBase <- "https://www.wunderground.com/history/airport/"
      urlEnd <- "/1/1/CustomHistory.html?dayend=31&monthend=12&yearend="
      urlText <- paste0(urlBase, stationCode, "/", year, urlEnd,year,"&format=1")
      
      # NOTE: The date for these data is given in local times
      
      #print(urlText)
      
      # Read the data from the internet
      x <- getURL(urlText)
      yearsData <- read.csv(text = x)
      
#       # subset the data, get only min, mean, max temperature
#       wantedColumns <- c("Max.TemperatureF",
#                          "Mean.TemperatureF",
#                          "Min.TemperatureF")
#       
#       columnMask <- names(yearsData) %in% wantedColumns
#       
#       # We also always want the first column, which is the date
#       columnMask[1] <- TRUE
#       
#       yearsData <- yearsData[,columnMask]
      
      # Can't use RBind function unless the column names match, sometimes 
      # standard time or daylight time reported
      names(yearsData)[1] <- "date.local"
      
      
      #print(paste("dim of yearsData", dim(yearsData)))
      
      # Place this years data in the all years dataframe
      if(year == years[1]){
        # If this is the first year, make the dataframe to be merged with
        allYearsData <- yearsData
      } else {
        # We have already gotten a years data, append the dataframe with new
        # years data 
        
        # How to merge data.frame
        #total <- rbind(data frameA, data frameB)
        allYearsData <- rbind(allYearsData, yearsData)
        
      }
      
    } # end of looping through years 
    
    assign(stationCode, allYearsData)
    
    save(list=stationCode, file=paste0("wundergroundDataNew/",stationCode,".RData"))
    print(paste("finished",stationCode))
    
    print(paste("startIndex:",startIndex))
    
    
  } # End of looping through airport codes 
  
}