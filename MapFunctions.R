#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

# rm(list=ls(all=TRUE))
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

# pathInputDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
# pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCountyFortified.csv")


#dvName <- "CountPerCapita"
# dvName <- "CountPerCapitaRank"
# 
# dsValue <- read.csv(pathInputSummaryCounty)

MapCounties <- function( dsValue ) {
  dsLocation <- map_data(map="county", region="OK")
  dsLocation$region <- dsLocation$subregion
  
  g <- ggplot(dsValue, aes(map_id=CountyName)) 
  g <- g + geom_map(aes(fill=DV), map=dsLocation, color="gray20") 
  # g <- g + geom_text(aes(label ##Need a center point, which map_data doesn't provide.
  g <- g + expand_limits(x=dsLocation$long, y=dsLocation$lat) 
  g <- g + scale_fill_gradient(name=dvName)
  g <- g + coord_map()
  g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm"))# + theme(
  g <- g + theme(plot.background=element_blank(), panel.background=element_blank())
  g <- g + theme(legend.position=c(0,0), legend.justification=c("left","bottom"))
  g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #+ theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
  return( g )
}

# MapCounties(dsValue)
