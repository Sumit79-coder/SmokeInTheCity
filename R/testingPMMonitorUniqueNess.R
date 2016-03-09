##################


PMIDs <- insertZeros(PM$State.Code, PM$County.Code, PM$Site.Num, PM$POC)

uniquePMIDs <- unique(PMIDs)




PMIDs <- insertZeros(PM$State.Code, PM$County.Code, PM$Site.Num, PM$POC)

uniquePMIDs <- unique(PMIDs)

for (i in uniquePMIDs){
  
  # subset by this ID
  rowMask <- i == PMIDs
  PMSubset <- PM[rowMask,]
  
  sampleDurations <- unique(PMSubset$Sample.Duration)
  
  
  d <- str_detect(sampleDurations,"24")
  
  numberOf24 <- sum(d)
  if(numberOf24 > 1){
    
    paste(i, "contains more than 1 type of 24 hour average")
    
  }
  
  
  
}


PMIDs <- insertZeros(PM$State.Code, PM$County.Code, PM$Site.Num, PM$POC)

uniquePMIDs <- unique(PMIDs)

for (i in uniquePMIDs){
  
  # subset by this ID
  rowMask <- i == PMIDs
  PMSubset <- PM[rowMask,]
  
  sampleDurations <- unique(PMSubset$Sample.Duration)
  
  
  d <- str_detect(sampleDurations,"24")
  
  numberOf24 <- sum(d)
  if(numberOf24 > 1){
    # I am developing second forloop 
    paste(i, "contains more than 1 type of 24 hour average")
    
  }
  
  
  
}

