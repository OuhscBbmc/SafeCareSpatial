#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)

pathDirectoryData <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathDirectoryCode <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
pathInputSummaryCountyYear <- file.path(pathDirectoryData, "CountCountyYearFortified.csv")
pathInputMappingCode <- file.path(pathDirectoryCode, "MapFunctions.R")
pathOutputAnimation <- file.path(pathDirectoryCode, "MapAnimation.gif")

ds <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
source(pathInputMappingCode)

years <- sort(unique(ds$Year))
# saveMovie({
s <- saveGIF({
  for( year in years ) {
    dsSlice <- ds[ds$Year==year, ]
    
    dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
    dsSlice$DV <- dsSlice[, dvName]
    dsSlice$DVLabel <- scales::comma(dsSlice$DV)
    #print(MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=FALSE))
    title <- paste(dvName, year)
    g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=FALSE, mapTitle=title)
    print(g)
  }
}, movie.name ="MapAnimation.gif", interval = 1, nmax = 30, ani.width=2000, ani.height=1000)
# ggsave(filename=file.path(pathDirectoryCode, "Animated.png"), plot=g)
ss <- strsplit(s, split=" ")
ss[[length(ss)]][length(ss[[1]])]