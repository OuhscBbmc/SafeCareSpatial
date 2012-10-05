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

dvName <- "Unemployment2012June"
titleTopPlot <- "Unemployment\n2012 June"
legendTopPlot <- "Percentage Rates\n(Divided into Quartiles)"

