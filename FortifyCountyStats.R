rm(list=ls(all=TRUE))#[!(ls(all=TRUE) %in% c("spDataFrameCounty", "spDataFrameTract", "spDataFrameBlock", "spDataFrameLakes", "splDataFrameRivers", "splDataFrameHighways", "spNationalParks", "spMilitaryBases", "deviceWidth"))])
require(plyr)
require(maps)
require(maptools)

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

## Indicate a point to label each county
dsCenterPoint <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
# countyIDs <- seq_along(dsCenterPoint$names) # cbind(seq_along(dsCenterPoint$names), order(dsCenterPoint$names))
countyIDs <- order(dsCenterPoint$names) #Using the 'order' fx accounts for the different alphabetical schemes
spForCenters <- map2SpatialPolygons(dsCenterPoint, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
spForCenters <- SpatialPolygonsDataFrame(spForCenters, data=dsCensus)
labelCoordinates <- coordinates(spForCenters)
labelCoordinates[which(dsCounty$CountyName=="Pottawatomie"), 2] <- coordinates(spForCenters)[which(dsCounty$CountyName=="Pottawatomie"), 2] + .1
labelCoordinates[which(dsCounty$CountyName=="Love"), 2] <- coordinates(spForCenters)[which(dsCounty$CountyName=="Love"), 2] + .05
colnames(labelCoordinates) <- c("LabelLongitude", "LabelLatitude")

dsCounty <- cbind(dsCounty, labelCoordinates)
rm(labelCoordinates, spForCenters, dsCenterPoint)
# dsCounty <- ddply(dsCounty, 

# spFortified <- fortify(sp, region="ID")


write.csv(dsCounty, pathOutputSummaryCounty, row.names=FALSE)
