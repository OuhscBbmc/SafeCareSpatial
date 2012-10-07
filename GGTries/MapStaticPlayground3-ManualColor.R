#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

rm(list=ls(all=TRUE))
require(maps)
require(maptools)
# require(sp)
# require(RColorBrewer)
require(colorspace)
require(classInt)
# require(fields)
require(grid)
require(ggplot2)
require(plyr)

deviceWidth <- 10 #20 #10 #6.5
# if( names(dev.cur()) != "null device" ) dev.off()
# aspectRatio <- .5
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

pathInputDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCountyFortified.csv")

dvName <- "CountPerCapita"
roundedDigits <- 2
colorPower <- 1
# dvName <- "CountPerCapitaRank"
# roundedDigits <- 0
# colorPower <- 2

dsValue <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsValue$DV <- dsValue[, dvName]
dsValue$DVLabel <- round(dsValue$DV, roundedDigits)

dsValuePlot <- data.frame(
  CountyID=dsValue$CountyID, 
  CountyNameLower=tolower(dsValue$CountyName), 
  CountyName=dsValue$CountyName, 
  LabelLongitude=dsValue$LabelLongitude,
  LabelLatitude=dsValue$LabelLatitude,
  DV=dsValue$DV, 
  DVLabel=dsValue$DVLabel,
  stringsAsFactors=FALSE
)

intervalCount <- 3
#breakPoints <- pretty(dsValuePlot$DV, n=intervalCount)
breakPoints <- seq(from=min(dsValuePlot$DV),to=max(dsValuePlot$DV), length.out=intervalCount+1)

# highestFloor <- breakPoints[intervalCount]
# inHighestCategory <- (dsValuePlot$DV > highestFloor)
paletteResource <- rev(sequential_hcl(n=intervalCount, h=340, c.=c(80, 0), l=c(40, 90), power=colorPower))

DvInterval <- function( dv ) {
  return( classIntervals(dv, n=intervalCount, style="fixed", fixedBreaks=breakPoints))  
}
ColorsContinuous <- function( dv ) {
  return( findColours(DvInterval(dv), paletteResource) )
}
ContrastingColor <-function( color ){
  lightness <- c(0.2, 0.6, 0) %*% col2rgb(color)/255
  return( ifelse( lightness >= 0.4, "#0F0F0F", "#F0F0F0") )
}
dsValuePlot$ColorFill <- ColorsContinuous(dsValuePlot$DV)
dsValuePlot$ColorLabel <-t(ContrastingColor(dsValuePlot$ColorFill))#[!inHighestCategory])) 
  
dsBoundary <- map_data(map="county", region="OK")
dsBoundary$region <- dsBoundary$subregion

g <- ggplot(dsValuePlot, aes_string(map_id="CountyNameLower", color="ColorLabel")) 
g <- g + geom_map(aes_string(fill="ColorFill"), map=dsBoundary, color="gray20")
#g <- g + geom_text(aes(label=CountyName, x=long, y=lat)) 
g <- g + geom_text(aes_string(label="CountyName", x="LabelLongitude", y="LabelLatitude"), vjust=-.2, size=deviceWidth*.25)
g <- g + geom_text(aes_string(label="DVLabel", x="LabelLongitude", y="LabelLatitude"), vjust=1, size=deviceWidth*.35)

g <- g + expand_limits(x=dsBoundary$long, y=dsBoundary$lat) 
g <- g + scale_fill_identity(name=dvName)
g <- g + scale_color_identity()
g <- g + coord_map()
# g <- g + theme_bw(base_size=2)
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm"))# + theme(
g <- g + theme(plot.background=element_blank(), panel.background=element_blank())
g <- g + theme(legend.position=c(0,0), legend.justification=c("left","bottom"))
g <- g + theme(plot.margin=unit(c(0, 0, 0, 0), "cm")) #+ theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
print(g)

