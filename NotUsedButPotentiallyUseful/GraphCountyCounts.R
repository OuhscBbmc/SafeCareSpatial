rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("spDataFrameCounty", "spDataFrameTract", "spDataFrameBlock", "spDataFrameLakes", 
                                           "splDataFrameRivers", "splDataFrameHighways", "spNationalParks", "spMilitaryBases", "deviceWidth"))])
require(maps)
require(maptools)
require(sp)
require(RColorBrewer)
require(colorspace)
# require(classInt)
require(fields)

deviceWidth <- 10 #20 #10 #6.5
# if( names(dev.cur()) != "null device" ) dev.off()
# aspectRatio <- .5
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

pathInputDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCounty.csv")
pathInputSummaryCountyYear <- file.path(pathInputDirectory, "CountCountyYear.csv")

dvName <- "Count"
titleTopPlot <- "Raw Reports\n2002-2012"
legendTopPlot <- "Count of Reports\n!!replicates not accounted for!!\n(Divided into Quartiles)"

ds <- read.csv(pathInputSummaryCounty)
stateMap <- map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)
countyIDs <- seq_along(stateMap$names)

sp <- map2SpatialPolygons(stateMap, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))
sp <- SpatialPolygonsDataFrame(sp, data=ds)

labelCoordinates <- coordinates(sp)

labelCoordinates[which(ds$CountyName=="Pottawatomie"), 2] <- coordinates(sp)[which(ds$CountyName=="Pottawatomie"), 2] + .1
breaksQuartile <- quantile(ds[, dvName])#quantile(ds[, dvName], seq(0,1,length=5))
breaksDecile <- quantile(ds[, dvName], seq(from=0, to=1, length=11))#quantile(ds[, dvName], seq(0,1,length=5))

colorsQuantiles <- (brewer.pal(length(breaksQuartile), "OrRd"))
colorsQuantilesLabels <- gray(c(.4, .3, .2, 1))
colorsContinuousLabels <- gray(c(rep(1, length=length(breaksDecile)-2), 0))

colorsContinuous <- function( dv ) {
  colorsContinuousPalette <- terrain_hcl(max(dv)-min(dv)+1, c = c(65, 0), l = c(45, 90), power = c(1/2, 20))
  return( colorsContinuousPalette[dv-min(dv)+1] )
}


if( deviceWidth==6.5 ) { #Designed for portrait documents with 1" borders (fits two to a page)
  titleSize <- 1.3
  subtitleSize <- .7
  explanationSize <- .8 
  countyLabelSize <- .6
}
if( deviceWidth==10 ) { #Designed for landscape documents with .5" borders (fits one to a page)
  titleSize <- 1.5
  subtitleSize <- 1
  explanationSize <- 1  
  countyLabelSize <- .7  
}
if( deviceWidth==20 ) { #Designed to almost fill a 20" widescreen monitor (1680x1050 resolution)
  titleSize <- 2.5
  subtitleSize <- 1.5
  explanationSize <- 1.5 
  countyLabelSize <- 1.  
}

oldPar <- par(mfrow=c(1,1), mar=c(0,0,0,0))

plot(sp, col=colorsQuantiles[findInterval(sp@data[, dvName], breaksQuartile, all.inside=TRUE)], axes=F, border="gray70")
text(titleTopPlot, x=-101.5, y=36.2, cex=titleSize)
legend(x=-102.5,y=35.5, legend=leglabs(breaksQuartile), fill=colorsQuantiles, bty="n", title=legendTopPlot, cex=explanationSize)

countyLabelsLine1 <- paste(ds$CountyName, "\n", sep="")
countyLabelsLine2 <- paste("\n", format(ds[, dvName], big.mark=","), sep="")
text(labelCoordinates, labels=countyLabelsLine1, col="black", cex=countyLabelSize)
text(labelCoordinates, labels=countyLabelsLine2, col=colorsQuantilesLabels[findInterval(sp@data[, dvName], breaksQuartile, all.inside=TRUE)], cex=countyLabelSize)

text("CCAN and DHS", x=-99, y=33.45, pos=3, col="gray60", cex=explanationSize)
par(oldPar)
