#Also see ExplanationSplicingCountyCensusFiles.md
rm(list=ls(all=TRUE)) #Clear variables
require(plyr)
require(reshape2)

pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets/Census/CountyTotal")

pathInputWide <- file.path(pathDirectoryData, "OklahomaCountyPopulationsByYearWide.csv")
pathOuputWide <- file.path(pathDirectoryData, "OklahomaCountyPopulationsByYear.csv")

dsWide <- read.csv(pathInputWide, stringsAsFactors=FALSE)
dsLong <- melt(dsWide, id.vars=c("CountyName"))
dsLong <- plyr::rename(dsLong, replace=c(variable="Year", value="PopTotal"))
dsLong$Year <-gsub(pattern="PopTotal",replacement="", x=dsLong$Year)
  
  
write.csv(dsLong, pathOuputWide, row.names=FALSE)
