#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
# rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)

pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
pathDirectoryCode <- pathDirectory
pathDirectoryImages <-  file.path(pathDirectory, "AnimationImages")
pathInputSummaryCountyYear <- file.path(pathDirectoryData, "CountCountyYearFortified.csv")
pathInputSummaryStateYear <- file.path(pathDirectoryData, "CountStateYearFortified.csv")

pathInputMappingCode <- file.path(pathDirectoryCode, "MapFunctions.R")
pathOutputAnimation <- file.path(pathDirectoryCode, "MapAnimation.gif")


years <- 2002:2011 #years <- sort(unique(dsCountyAllYears$ReferralYear))

dsCountyAllYears <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
dsCounty <- dsCounty[dsCounty$ReferralYear %in% years, ]

# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
# dsCountyAllYears$DVLabel <- scales::comma(dsCountyAllYears$DV)

dvName <- "CountPerCapitaAnnual" #The number of victims per county population; darker counties have more victims, adjusted for pop
dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
dsCountyAllYears$DVLabel <- gsub("^0.", ".",round(dsCountyAllYears$DV,3)) #Remove leading zeros.

# dvName <- "CountPerCapitaRank" #The county's rank for the number of victims per county population; darker counties have more victims, adjusted for pop
# dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
# dsCountyAllYears$DVLabel <- dsCountyAllYears$DV

dvFloor <- min(dsCountyAllYears$DV)
dvCeiling <- max(dsCountyAllYears$DV)
source(pathInputMappingCode)

#These six lines are for the line graph
dsCounty <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
dsState <- read.csv(pathInputSummaryStateYear, stringsAsFactors=FALSE)
dsCounty <- dsCounty[dsCounty$ReferralYear %in% years, ]
dsState <- dsState[dsState$ReferralYear %in% years, ]
dsCounty$DV <- dsCounty[, dvName]
dsState$DV <- dsState[, dvName]


intervals <- rep(1, length(years))
intervals[1] <- 4
intervals[length(intervals)] <- 4

s <- saveGIF({
  for( year in years ) {
    dsSlice <- dsCountyAllYears[dsCountyAllYears$ReferralYear==year, ]
    
    title <- paste(dvName, year)
#     g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
#                      dvFloor=dvFloor, dvCeiling=dvCeiling)
    #     g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
    #                      dvFloor=dvFloor, dvCeiling=dvCeiling)    
#     print(g)
    

    
    MapCountiesWithInset(dsValueCountyOneYear=dsSlice,  deviceWidth=14, mapTitle=title, 
                         dsValueCountyAllYears=dsCountyAllYears, dsValueState=dsState, yearBand=year)
#     names(dsValueAllVariables)
    
  }
}, movie.name=paste0(dvName, ".gif"), outdir=pathDirectoryImages, interval=intervals,ani.width=1600, ani.height=800)

ss <- strsplit(s, split=" ")
ss[[length(ss)]][length(ss[[1]])]