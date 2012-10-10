#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(plyr)
# require(animation)

GraphLongitudinalTrend <- function( dsCounty, dsState, labelThreshold=.01, yearBand=NA ) {

  g <- ggplot(dsCounty, aes(x=ReferralYear, y=DV, ymin=0, group=CountyID, color=factor(CountyID)))
  if( !is.na(yearBand) )
    g <- g + geom_vline(xintercept = yearBand, alpha=.2, size=30)
  g <- g + geom_line(stat="identity")
  g <- g + geom_line(data=dsState, aes(x=ReferralYear, y=DV, group=NA, color=NA), stat="identity", size=1, color="black")
  g <- g + geom_smooth(data=dsState, aes(x=ReferralYear, y=DV, group=NA, color=NA), method="loess", size=3)
  if( !is.na(labelThreshold) )
    g <- g + geom_text(data=dsCounty[dsCounty$DV >labelThreshold, ], aes(x=ReferralYear,label=CountyName), vjust=1, size=4)
  
  g <- g + scale_x_continuous(name="", breaks=years)
  g <- g + scale_y_continuous(name=dvName, limits=c(0, max(dsCounty$DV)), expand=c(0,0))
  g <- g + theme(legend.position = 'none')
  return( g )
}




# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# # pathDirectory <- "C:/Users/wbeasley/Documents/SafeCare/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory
# pathDirectoryImages <-  file.path(pathDirectory, "AnimationImages")
# 
# pathInputSummaryCountyYear <- file.path(pathDirectoryData, "CountCountyYearFortified.csv")
# pathInputSummaryStateYear <- file.path(pathDirectoryData, "CountStateYearFortified.csv")
# 
# pathInputMappingCode <- file.path(pathDirectoryCode, "MapFunctions.R")
# pathOutputAnimation <- file.path(pathDirectoryCode, "MapAnimation.gif")
# 
# dsCounty <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
# dsState <- read.csv(pathInputSummaryStateYear, stringsAsFactors=FALSE)
# 
# #years <- sort(unique(dsCounty$ReferralYear))
# years <- 2002:2011
# dsCounty <- dsCounty[dsCounty$ReferralYear %in% years, ]
# dsState <- dsState[dsState$ReferralYear %in% years, ]
# 
# 
# # deviceWidth <- 10 #20 #10 #6.5
# # if( names(dev.cur()) != "null device" ) dev.off()
# # aspectRatio <- .5
# # deviceHeight <- deviceWidth * aspectRatio
# # windows(width=deviceWidth, height=deviceHeight)
# 
# # dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# # dsCounty$DV <- dsCounty[, dvName]
# 
# dvName <- "CountPerCapitaAnnual" #The number of victims per county population; darker counties have more victims, adjusted for pop
# dsCounty$DV <- dsCounty[, dvName]
# dsState$DV <- dsState[, dvName]
# 
# # dvName <- "CountPerCapitaRank" #The county's rank for the number of victims per county population; darker counties have more victims, adjusted for pop
# # dsCounty$DV <- dsCounty[, dvName]
# # dsCounty$DVLabel <- dsCounty$DV
# 
# #pretendYear <- 2005
# GraphLongitudinalTrend(dsCounty, dsState, yearBand=2002)