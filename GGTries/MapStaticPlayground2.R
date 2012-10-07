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


dvName <- "CountPerCapita"
# dvName <- "CountPerCapitaRank"
dsValueAllVariables <- read.csv(pathInputSummaryCounty)
dsValue <- data.frame(CountyID=dsValueAllVariables$CountyID, CountyName=tolower(dsValueAllVariables$CountyName), DV=dsValueAllVariables[, dvName])
# dsValue$DV <- dsValue[, dvName]

dsBoundary <- map_data(map="county", region="OK")
dsBoundary$region <- dsBoundary$subregion
# countyIDs <- seq_along(unique(dsBoundary$region))

dsCenterPoint <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
countyIDs <- seq_along(dsCenterPoint$names)
spForCenters <- map2SpatialPolygons(dsCenterPoint, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
# sp <- SpatialPolygonsDataFrame(spForCenters, data=dsValue)
labelCoordinates <- coordinates(spForCenters)
colnames(labelCoordinates) <- c("long", "lat")
dsValue <- cbind(dsValue, labelCoordinates)


# dsBoundary <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
# countyIDs <- seq_along(dsBoundary$names)
# sp <- map2SpatialPolygons(dsBoundary, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
#  sp <- SpatialPolygonsDataFrame(sp, data=dsValue)

# spFortified <- fortify(sp, region="ID")

g <- ggplot(dsValue, aes(map_id=CountyName)) 
g <- g + geom_map(aes(fill=DV), map=dsBoundary, color="gray20")
#g <- g + geom_map(aes(fill=DV), map=spFortified, color="gray20") 
g <- g + geom_text(aes(label=CountyName, x=long, y=lat)) 
                       
                       
g <- g + expand_limits(x=dsBoundary$long, y=dsBoundary$lat) 
# g <- g + expand_limits(x=dsBoundary$x, y=dsBoundary$y)
g <- g + scale_fill_gradient(name=dvName)
g <- g + coord_map()
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm"))# + theme(
g <- g + theme(plot.background=element_blank(), panel.background=element_blank())
g <- g + theme(legend.position=c(0,0), legend.justification=c("left","bottom"))
g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #+ theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
print(g)

