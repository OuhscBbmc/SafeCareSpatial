rm(list=ls(all=TRUE)) #Clear variables

pathDirectoryData <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathDirectoryCode <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
pathInputSummaryCountyYear <- file.path(pathDirectoryData, "CountCountyYearFortified.csv")
pathInputMappingCode <- file.path(pathDirectoryCode, "MapFunctions.R")

ds <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
source(pathInputMappingCode)

years <- sort(unique(ds$Year))
for( year in years ) {
  dsSlice <- ds[ds$Year==year, ]
  
  dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
  dsSlice$DV <- dsSlice[, dvName]
  dsSlice$DVLabel <- scales::comma(dsSlice$DV)
  #print(MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=FALSE))
  g <-MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=FALSE)
  
}
ggsave(filename=file.path(pathDirectoryCode, "Animated.png"), plot=g)