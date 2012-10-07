rm(list=ls(all=TRUE))#[!(ls(all=TRUE) %in% c("spDataFrameCounty", "spDataFrameTract", "spDataFrameBlock", "spDataFrameLakes", "splDataFrameRivers", "splDataFrameHighways", "spNationalParks", "spMilitaryBases", "deviceWidth"))])
require(plyr)

pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathInputCensus <- file.path(pathDirectory, "PopByCounty-2012Aug.csv")
pathInputSummaryCounty <- file.path(pathDirectory, "CountCounty.csv")
pathInputSummaryCountyYear <- file.path(pathDirectory, "CountCountyYear.csv")
pathOutputSummaryCounty <- file.path(pathDirectory, "CountCountyFortified.csv")
pathOutputSummaryCountyYear <- file.path(pathDirectory, "CountCountyYearFortified.csv")

#Read in the necessary data files
dsCensus <- read.csv(pathInputCensus, stringsAsFactors=FALSE)
dsCounty <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsCountyYear <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)

dsCensus <- dsCensus[, c("CountyName", "PopTotal")] #Drop the unnecessary columns

dsCounty <- merge(x=dsCounty, y=dsCensus, by="CountyName")
dsCounty$CountPerCapita <- dsCounty$Count / dsCounty$PopTotal
dsCounty$CountRank <- rank(dsCounty$Count)
dsCounty$CountPerCapitaRank <- rank(dsCounty$CountPerCapita)


# dsCounty <- ddply(dsCounty, 


write.csv(dsCounty, pathOutputSummaryCounty, row.names=FALSE)
