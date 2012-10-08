#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
# require(animation)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
pathDirectory <- "C:/Users/wbeasley/Documents/SafeCare/SafeCareSpatial"
pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
pathDirectoryCode <- pathDirectory
pathDirectoryImages <-  file.path(pathDirectory, "AnimationImages")
pathInputSummaryCountyYear <- file.path(pathDirectoryData, "CountCountyYearFortified.csv")
pathInputMappingCode <- file.path(pathDirectoryCode, "MapFunctions.R")
pathOutputAnimation <- file.path(pathDirectoryCode, "MapAnimation.gif")

ds <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)


# deviceWidth <- 10 #20 #10 #6.5
# if( names(dev.cur()) != "null device" ) dev.off()
# aspectRatio <- .5
# deviceHeight <- deviceWidth * aspectRatio
# windows(width=deviceWidth, height=deviceHeight)

# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# ds$DV <- ds[, dvName]
# ds$DVLabel <- scales::comma(ds$DV)

dvName <- "CountPerCapitaAnnual" #The number of victims per county population; darker counties have more victims, adjusted for pop
ds$DV <- ds[, dvName]
ds$DVLabel <- gsub("^0.", ".", round(ds$DV,3)) #Remove leading zeros.

# dvName <- "CountPerCapitaRank" #The county's rank for the number of victims per county population; darker counties have more victims, adjusted for pop
# ds$DV <- ds[, dvName]
# ds$DVLabel <- ds$DV

# dvFloor <- min(ds$DV)
# dvCeiling <- max(ds$DV)
pretendYear <- 2005

g <- ggplot(ds, aes(x=Year, y=DV, group=CountyID, color=factor(CountyID)))
# g <- g + geom_vline(xintercept = pretendYear)
g <- g + geom_line(stat="identity")
g <- g + geom_smooth(aes(x=Year, y=DV, group=NA), size=2)
g <- g + scale_y_continuous(name=dvName)
g <- g + theme(legend.position = 'none')
g