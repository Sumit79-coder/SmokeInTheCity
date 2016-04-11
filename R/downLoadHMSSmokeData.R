# This script will be used to read GIS smoke data from 
# ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/

# load libraries
library(sp)
library(rgdal)
library(rgeos)
library(stringr)
library(R.utils)


################################################################################
# Function for downloading HMS GIS data from NOAA
################################################################################
downLoadHMSSmoke <- function(startYear, endYear, startMonth, endMonth,
                             dataDir){
  
  # 2005 - 2010: hms_smoke20050805.dbf.gz  also shx shp
  # 2011 - 2014: hms_smoke20050805.zip.gz data/oper/hmsfinalprocessing/intermediate
  
  dataDir   <- "FireData/HMS_smoke/"
  startYear  <- 2005
  endYear    <- 2014
  
  startTime <- as.POSIXlt(  paste(startYear, "08", "05" ,sep="-") ) # this is just when the data starts
  endTime   <- as.POSIXlt(  paste(endYear, endMonth, "30" ,sep="-") )
  
  time <- seq(startTime, endTime, by="days")
  time <- as.POSIXlt(time)
  timeMask <- (time$mon+1) >= startMonth & (time$mon+1) <= endMonth
  time <- time[timeMask]
  
  # Download the GIS fire data for all the days in "time" array 
  for (i in 1:length(time)){
    
    t   <- time[i]
    year <- t$year + 1900
    mon <- t$mon+1;if(str_length(mon) < 2){mon <- paste0("0",mon)}
    day <- t$mday; if(str_length(day) < 2){day <- paste0("0",day)}
    
    # Build the URL: 
    # ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/2014/hms_smoke20140101.zip.gz
    
    urlBase <- 'ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/'
    fileURL <- paste0(urlBase, year,"/", "hms_smoke",year,mon,day)
    
    
    if(year >= 2011){
      
      # Its a zip file with a bunch of stupid layers
      url <- paste0(fileURL, ".zip.gz")
      destfile <- paste0(dataDir,"temp.zip.gz")
      
      # Make sure file exists
      try_error <- try(
        download.file(url=url, destfile=destfile)
      )
      
      if( !(class(try_error) == "try-error") ){
        
        gunzip(destfile)
        
        # Now unzip regular .zip 
        zipDestFile <- paste0(dataDir,"temp.zip")
        unzip(zipfile=zipDestFile, 
              exdir=dataDir)
        
        # Now move the files and rename 
        extension <- c(".dbf", ".shp", ".shx")
        for(ext in extension){
          
          zippedDir <- paste0(dataDir, "data/oper/hmsfinalprocessing/intermediate/")
          file      <- paste0("smokepolygons.",year,mon,day,ext) 
          moveFile  <- paste0(zippedDir, file)
          moveFileTo <- paste0(dataDir,"hms_" , year, mon, day, ext)
          
          # Construct the final url of the three GIS files that must be downloaded
          file.rename(from=moveFile, to=moveFileTo)
          
        } # end of GIS file loop 
        
        # Get rid of unneeded files and directories
        file.remove(zipDestFile)
      } else{
        print("The data does not exist")
      }
      
    } else { # Not a zip file with a bunch of stupid layers 
      
      # Make sure file exists
      
      url <- paste0(fileURL, ".dbf.gz")
      destfile<- paste0(dataDir,"hms_",year,mon,day,ext)
      try_error <- try(  
        download.file(url=url,destfile=destfile)
      )
      
      if(class(try_error) == "try-error"){
        # Do nothing because the data does not exist 
        print("no data for day")
      }  else {
        
        # Loop through file extensions that need to be downloaded 
        extension <- c(".dbf.gz", ".shp.gz", ".shx.gz")
        for(ext in extension){
          
          # Construct the final url of the three GIS files that must be downloaded
          url <- paste0(fileURL, ext)
          destfile<- paste0(dataDir,"hms_",year,mon,day,ext)
          
          download.file(url=url, destfile=destfile)
          
          # unzip the file
          gunzip(filename=destfile)
          
        } # end of GIS file extension loop 
      }
      
    } # end of early year data retrieval 
    
    
  } # End of looping through days you want to download GIS data for 
}


################################################################################
# TODO: Merge the better functionality introduced by James Crooks. Commented
# TODO: below
################################################################################

# downLoadHMSSmoke <- function(startYear=2005, endYear=2014,
#                              dataDir){
#   
#   # 2005 - 5/31/2010: hms_smoke20050805.dbf.gz  also shx shp
#   # 6/1/2010 - 2014: hms_smoke20050805.zip.gz data/oper/hmsfinalprocessing/intermediate
#   
#   dataDir   <- 
#   
#   if(startYear>endYear) stop("year error")
#   if(startYear<2005) startYear <- 2005
#   
#   if(startYear==2005){
#     startMonth <- "08"
#     startTime <- as.POSIXlt(  paste(startYear, startMonth, "05" ,sep="-"), tz = "GMT" ) # this is just when the data starts
#   }else{
#     startMonth <- "01"
#     startTime <- as.POSIXlt(  paste(startYear, startMonth, "01" ,sep="-"), tz = "GMT" )
#   }
#   endMonth <- "12"
#   endTime   <- as.POSIXlt(  paste(endYear, endMonth, "31" ,sep="-") )
#   
#   time <- seq(startTime, endTime, by="days")
#   time <- as.POSIXlt(time)
#   timeMask <- (time$mon+1) >= startMonth & (time$mon+1) <= endMonth
#   
#   time <- time[timeMask]
#   
#   # Download the GIS fire data for all the days in "time" array 
#   for (i in 1:length(time)){
#     
#     t   <- time[i]
#     year <- t$year + 1900
#     mon <- t$mon+1;if(str_length(mon) < 2){mon <- paste0("0",mon)}
#     day <- t$mday; if(str_length(day) < 2){day <- paste0("0",day)}
#     
#     # Build the URL: 
#     # ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/2014/hms_smoke20140101.zip.gz
#     
#     urlBase <- 'ftp://satepsanone.nesdis.noaa.gov/volcano/FIRE/HMS_smoke/'
#     fileURL <- paste0(urlBase, year,"/", "hms_smoke",year,mon,day)
#     
#     
#     if( ( time[i]>=as.POSIXlt("2010-06-01",tz="GMT") && time[i]<=as.POSIXlt("2012-07-29",tz="GMT") ) || 
#         ( time[i]>=as.POSIXlt("2013-01-01",tz="GMT") ) ){
#       #if(year >= 2011){
#       
#       # Its a zip file with a bunch of stupid layers
#       url <- paste0(fileURL, ".zip.gz")
#       destfile <- paste0(dataDir,"temp.zip.gz")
#       print(time[i])
#       
#       # Make sure file exists
#       try_error <- try(
#         download.file(url=url, destfile=destfile)
#       )
#       
#       if( !(class(try_error) == "try-error") ){
#         
#         gunzip(destfile)
#         
#         # Now unzip regular .zip 
#         #print(list.files(dataDir))
#         zipDestFile <- paste0(dataDir,"temp.zip")#".gz"
#         
#         unzip(zipfile=zipDestFile, 
#               exdir="./Brey_Data/FireData/HMS_Smoke")
#         
#         # Now move the files and rename 
#         extension <- c(".dbf", ".shp", ".shx")
#         for(ext in extension){
#           
#           zippedDir <- paste0(dataDir, "data\\oper\\hmsfinalprocessing\\intermediate/")
#           file      <- paste0("smokepolygons.",year,mon,day,ext) 
#           moveFile  <- paste0(zippedDir, file)
#           moveFileTo <- paste0(dataDir,"hms_" , year, mon, day, ext)
#           
#           # Construct the final url of the three GIS files that must be downloaded
#           file.rename(from=moveFile, to=moveFileTo)
#           
#         } # end of GIS file loop 
#         
#         # Get rid of unneeded files and directories
#         file.remove(zipDestFile)
#       } else{
#         print("The data does not exist")
#       }
#       
#     } else { # Not a zip file with a bunch of stupid layers 
#       
#       # Make sure file exists
#       
#       url <- paste0(fileURL, ".dbf.gz")
#       destfile<- paste0(dataDir,"hms_",year,mon,day,".dbf.gz")
#       try_error <- try(  
#         download.file(url=url,destfile=destfile)
#       )
#       
#       if(class(try_error) == "try-error"){
#         # Do nothing because the data does not exist 
#         print("no data for day")
#       }  else {
#         
#         # Loop through file extensions that need to be downloaded 
#         extension <- c(".dbf.gz", ".shp.gz", ".shx.gz")
#         for(ext in extension){
#           
#           # Construct the final url of the three GIS files that must be downloaded
#           url <- paste0(fileURL, ext)
#           destfile<- paste0(dataDir,"hms_",year,mon,day,ext)
#           print(time[i])
#           #rint(destfile)
#           #print(gsub(".gz","",destfile) %in% list.files(dataDir))
#           download.file(url=url, destfile=destfile)
#           #print('before unzip')
#           #print(gsub(".gz","",destfile))
#           #print(list.files(dataDir))
#           #print(gsub(dataDir,"",gsub(".gz","",destfile)) %in% list.files(dataDir))
#           # unzip the file
#           gunzip(filename=destfile)
#           #print('after unzip')
#         } # end of GIS file extension loop 
#       }
#       
#     } # end of early year data retrieval 
#     
#     
#   } # End of looping through days you want to download GIS data for 
# }
# 
# 
# downLoadHMSSmoke(startYear=2005, endYear=2014,
#                  dataDir="Brey_Data\\FireData\\HMS_Smoke")
