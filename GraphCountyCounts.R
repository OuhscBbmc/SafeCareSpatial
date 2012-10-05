rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("spDataFrameCounty", "spDataFrameTract", "spDataFrameBlock", "spDataFrameLakes", 
                                           "splDataFrameRivers", "splDataFrameHighways", "spNationalParks", "spMilitaryBases", "deviceWidth"))])
require(maps)
require(maptools)
require(sp)
require(RColorBrewer)
require(colorspace)
# require(classInt)
require(fields)

# deviceWidth <- 10 #20 #10 #6.5
# if( names(dev.cur()) != "null device" ) dev.off()
# aspectRatio <- .5
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

pathInputDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCounty.csv")
pathInputSummaryCountyYear <- file.path(pathInputDirectory, "CountCountyYear.csv")

dvName <- "Count"
titleTopPlot <- "Unemployment\n2012 June"
legendTopPlot <- "Percentage Rates\n(Divided into Quartiles)"

ds <- read.csv(pathInputSummaryCounty)
stateMap <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
countyIDs <- seq_along(stateMap$names)

sp <- map2SpatialPolygons(stateMap, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
sp <- SpatialPolygonsDataFrame(sp, data=ds)

labelCoordinates <- coordinates(sp)
labelCoordinates[c(1:77)[ds$CountyName=="Pottawatomie"],2] <- coordinates(ok_poly_sp2)[c(1:77)[ds$CountyName=="Pottawatomie"],2] + .1
breaksQuartile <- quantile(ds[, dvName])#quantile(ds[, dvName], seq(0,1,length=5))
breaksDecile <- quantile(ds[, dvName], seq(from=0, to=1, length=11))#quantile(ds[, dvName], seq(0,1,length=5))
