
# Get the analysis data packet tester 
load("~/projects/smokeInTheCity/testPacket.RData")
PM_df_subset <- L[['PM_df']]
PM_mdf_subset<- L[["PM_mdf"]]

PM_df_all <- get(load("timeSeries/mergedYears/PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_2005_2014.RData"))
rm(PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_2005_2014)

PM_mdf_all <- get(load("timeSeries/mergedYearsMetadata/PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_metadata_2005_2014.RData"))
rm(PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_metadata_2005_2014)


# Get the times
time_subset <- as.POSIXct(rownames(PM_df_subset))
time_all <- as.POSIXct(rownames(PM_df_all))


subsetIDs <- colnames(PM_df_subset)

# for loop
for (i in 1:length(subsetIDs)){
  ID <- subsetIDs[i]
  
  allIDs <- colnames(PM_df_all)
  
  subsetMask <- which(ID == subsetIDs)[1] # there can be more than one
  allMask <- ID == allIDs # there can be only one
  
  pdf(file=paste0("dump/",ID,".pdf"), width=14, height=7)
  plot(time_all, PM_df_all[, allMask], las=1, xlab="PM2.5", ylab="Time")
  points(time_subset, PM_df_subset[, subsetMask], col="red", pch=19, cex=.3)
  
  # build two titles with the metadata separately 
  # Get the packet metadata
  mdf_row<- PM_mdf_subset[PM_mdf_subset$ID == ID,]
  metaTag_subset <- paste(mdf_row$ID, mdf_row$State.Name,mdf_row$County.Name,mdf_row$City.Name)
  
  # Get the original metadata 
  mdf_all_row <- PM_mdf_all[PM_mdf_all$ID == ID, ]
  metaTag_all <- paste(mdf_all_row$ID, mdf_all_row$State.Name,mdf_all_row$County.Name,mdf_all_row$City.Name)
  
  
  title(metaTag_subset)
  title(line=.2, metaTag_all)
  dev.off()
}

################################################################################
# OZONE NEEDS TO BE CHECKED AS WELL
################################################################################
# 
# # Get the analysis data packet tester 
# load("~/projects/smokeInTheCity/testPacket.RData")
# PM_df_subset <- L[['Ozone_df']]
# PM_mdf_subset<- L[["Hybrid_mdf"]]
# 
# PM_df_all <- get(load("timeSeries/mergedYears/Ozone_X1st.Max.Value_8hr_2005_2014.RData"))
# rm(PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_2005_2014)
# 
# PM_mdf_all <- get(load("timeSeries/mergedYearsMetadata/Ozone_X1st.Max.Value_8hr_metadata_2005_2014.RData"))
# rm(PM2.5_non_FRM_Mass_Arithmetic.Mean_24hr_metadata_2005_2014)
# 
# 
# # Get the times
# time_subset <- as.POSIXct(rownames(PM_df_subset))
# time_all <- as.POSIXct(rownames(PM_df_all))
# 
# 
# subsetIDs <- colnames(PM_df_subset)
# 
# # for loop
# for (i in 1:length(subsetIDs)){
#   ID <- subsetIDs[i]
#   
#   allIDs <- colnames(PM_df_all)
#   
#   subsetMask <- which(ID == subsetIDs)# there should be only one
#   allMask <- ID == allIDs # there can be only one
#   
#   pdf(file=paste0("dump/",ID,".pdf"), width=14, height=7)
#   plot(time_all, PM_df_all[, allMask], las=1, ylab="O3", xlab="Time")
#   points(time_subset, PM_df_subset[, subsetMask], col="red", pch=19, cex=.3)
#   
#   # build two titles with the metadata separately 
#   # Get the packet metadata
#   mdf_row<- PM_mdf_subset[PM_mdf_subset$ID == ID,]
#   metaTag_subset <- paste(mdf_row$ID, mdf_row$State.Name,mdf_row$County.Name,mdf_row$City.Name)
#   
#   # Get the original metadata 
#   mdf_all_row <- PM_mdf_all[PM_mdf_all$ID == ID, ]
#   metaTag_all <- paste(mdf_all_row$ID, mdf_all_row$State.Name,mdf_all_row$County.Name,mdf_all_row$City.Name)
#   
#   
#   title(metaTag_subset)
#   title(line=.2, metaTag_all)
#   dev.off()
# }
