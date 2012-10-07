#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

rm(list=ls(all=TRUE))
require(maps)
require(maptools)
# require(sp)
# require(RColorBrewer)
# require(colorspace)
# # require(classInt)
# require(fields)
require(grid)
require(ggplot2)
require(plyr)

# deviceWidth <- 10 #20 #10 #6.5
# if( names(dev.cur()) != "null device" ) dev.off()
# aspectRatio <- .5
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

pathInputDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCountyFortified.csv")

# dvName <- "CountPerCapita"
# roundedDigits <- 2
dvName <- "CountPerCapitaRank"
roundedDigits <- 0

dsValueAllVariables <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsValue <- data.frame(CountyID=dsValueAllVariables$CountyID, CountyNameLower=tolower(dsValueAllVariables$CountyName), CountyName=dsValueAllVariables$CountyName, DV=dsValueAllVariables[, dvName], stringsAsFactors=FALSE)
dsValue$ColorFill <- "tomato"

dsBoundary <- map_data(map="county", region="OK")
dsBoundary$region <- dsBoundary$subregion

dsCenterPoint <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
# countyIDs <- seq_along(dsCenterPoint$names) # cbind(seq_along(dsCenterPoint$names), order(dsCenterPoint$names))
countyIDs <- order(dsCenterPoint$names) #Using the 'order' fx accounts for the different alphabetical schemes
spForCenters <- map2SpatialPolygons(dsCenterPoint, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
spForCenters <- SpatialPolygonsDataFrame(spForCenters, data=dsValue)

labelCoordinates <- coordinates(spForCenters)
labelCoordinates[which(dsValue$CountyName=="Pottawatomie"), 2] <- coordinates(spForCenters)[which(dsValue$CountyName=="Pottawatomie"), 2] + .1
labelCoordinates[which(dsValue$CountyName=="Love"), 2] <- coordinates(spForCenters)[which(dsValue$CountyName=="Love"), 2] + .05
colnames(labelCoordinates) <- c("long", "lat")

dsValue <- cbind(dsValue, labelCoordinates)
rm(labelCoordinates, spForCenters, dsCenterPoint)

# spFortified <- fortify(sp, region="ID")

g <- ggplot(dsValue, aes(map_id=CountyNameLower)) 
g <- g + geom_map(aes(fill=ColorFill), map=dsBoundary, color="gray20")
# g <- g + geom_map(aes(fill=DV), map=dsBoundary, color="gray20")
#g <- g + geom_text(aes(label=CountyName, x=long, y=lat)) 
g <- g + geom_text(aes(label=CountyName, x=long, y=lat), vjust=-.2, size=4)
g <- g + geom_text(aes(label=round(DV, roundedDigits), x=long, y=lat), vjust=1)

g <- g + expand_limits(x=dsBoundary$long, y=dsBoundary$lat) 
# g <- g + scale_fill_gradient(name=dvName)
g <- g + coord_map()
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm"))# + theme(
g <- g + theme(plot.background=element_blank(), panel.background=element_blank())
g <- g + theme(legend.position=c(0,0), legend.justification=c("left","bottom"))
g <- g + theme(plot.margin=unit(c(0, 0, 0, 0), "cm")) #+ theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
print(g)

