rm(list=ls(all=TRUE))  #Clears variables
require(RODBC)
require(plyr) #For renaming columns
require(lubridate) #For dealing with dates

#If the parallel version is used:
# library(foreach)
# library(doParallel)
# workers <- makeCluster(4) #4 threads
# registerDoParallel(workers)

pathWorkingDatasets <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/SafeCareCostEffectiveness/WorkingDatasets"
# pathWorkingDatasets <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathOutputVictim <- file.path(pathWorkingDatasets, "Victim.csv")
pathOutputSummaryCounty <- file.path(pathWorkingDatasets, "CountCounty.csv")
pathOutputSummaryCountyYear <- file.path(pathWorkingDatasets, "CountCountyYear.csv")

#pathCountyLookupTable <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/LookupTables/CountyLookups.csv"
pathCountyLookupTable <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/SafeCareCostEffectiveness/ReadonlyDatabases/CountyLookups.csv"

msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04","MSUR 06-05","MSUR 06-06","MSUR 06-07","MSUR 06-08","MSUR 06-09","MSUR 06-10","MSUR 06-11","MSUR 06-12")
msurYear <- 2002 + seq_along(msurTableNames) - 1 
desiredColumns <- c("MsurSource", "MsurYear", "KK", "county", "dateFIND")
dsMsur <- data.frame(MsurSource=character(0), MsurYear=numeric(0), KK=numeric(0), County=character(0))

#This DSN points to \\dch-res\PEDS-FILE-SV\Data\CCAN\CCANResEval\SafeCareCostEffectiveness\ReadonlyDatabases\OCS2000.mdb
channelOcs2000 <- odbcConnect(dsn="SafeCareOcs2000")
#odbcGetInfo(channelOcs2000) #dsTables <- sqlTables(channelOcs2000)
for( tableID in seq_along(msurTableNames) ) {
  table <- msurTableNames[tableID]
  
  dsMsurYear <- sqlFetch(channelOcs2000, table, stringsAsFactors=FALSE)
  dsMsurYear$MsurSource <- table
  dsMsurYear$MsurYear <- msurYear[tableID]
  print(paste("Table", table, "has been retrieved with", nrow(dsMsurYear), "rows."))
  dsMsur <- rbind(dsMsur, dsMsurYear[, desiredColumns])
}
odbcClose(channelOcs2000)
rm(dsMsurYear)
dsMsur <- plyr::rename(dsMsur, replace=c(county="County", dateFIND="DateFind"))
dsMsur <- dsMsur[!is.na(dsMsur$County), ] #Drop the cases with missing counties.
# dsMsur <- dsMsur[!is.na(dsMsur$DateFind), ] #Drop the missing dates.

# dsSummaryKK <- count(dsMsur, c("KK"))
# dsSummaryKK <- dsSummaryKK[order(-dsSummaryKK$freq), ]
# count(df=dsSummaryKK, vars="freq")
# 
# dsSummaryKKYear <- count(dsMsur, c("KK", "MsurSource"))
# dsSummaryKKYear <- dsSummaryKKYear[order(-dsSummaryKKYear$freq), ]
# count(df=dsSummaryKKYear, vars="freq")

regexPattern <- "[a-z A-Z]"
dsMsur$CountyID <- as.integer(gsub(pattern=regexPattern, replacement="", x=dsMsur$County))
# sort(unique(dsMsur$CountyID))
# class(dsMsur$CountyID)
dsMsur <- dsMsur[, !(colnames(dsMsur) %in% c("County"))] #Drop the dirty county variable.

startTime <- Sys.time()
channelKids07 <- odbcConnect(dsn="SafeCareKids07")
# dsReferral <- sqlFetch(channelKids07, "Chrefer")
# dsAllegation <- sqlFetch(channelKids07, "Challeg")
dsReferral <- sqlQuery(channelKids07, query="SELECT ReferId, CaseId, ReferDt, CmpltDt, ReferTyp FROM Chrefer")
dsAllegation <- sqlQuery(channelKids07, query="SELECT ReferId, VctmId, AbuseFlg, NglctFlg, SexAbFlg FROM Challeg")
odbcClose(channelKids07)
Sys.time() - startTime 
# rm(dsReferral)

dsReferral <- plyr::rename(dsReferral, replace=c(ReferId="ReferralID", CaseId="KK", ReferDt="ReferralDate"))
# dsReferral$ReferralMonth <- as.Date(ISOdate(year(dsReferral$ReferralDate), month(dsReferral$ReferralDate), 1))

dsAllegation <- plyr::rename(dsAllegation, replace=c(ReferId="ReferralID", VctmId="VictimID"))
length(unique(dsAllegation$ReferralID))
length(unique(dsAllegation$VictimID))
#table(dsAllegation$ReferralID, dsAllegation$VictimID)
# count(dsAllegation. c("ReferralID", "VictimID"))
# count(dsAllegation, "ReferId")

startTime <- Sys.time()
CollapseAllegations <- function( df ) {
  with(df, data.frame(
    Abuse=any(df$AbuseFlg=="Y"),
    Neglect=any(df$NglctFlg=="Y"),
    SexualAbuse=any(df$SexAbFlg=="Y")
  ))
}

dsAllegationByVictimAndReferral <- ddply(dsAllegation, .variables=c("ReferralID", "VictimID"), CollapseAllegations, .parallel=FALSE)
Sys.time() - startTime #Serial: 2.664216 mins

# ds <- merge(x=dsReferral, y=dsAllegation, by="ReferralID", all.x=TRUE, all.y=FALSE)
ds <- merge(x=dsAllegationByVictimAndReferral, y=dsReferral, by="ReferralID", all.x=TRUE, all.y=FALSE)


dsCountyNames <- read.csv(pathCountyLookupTable)
dsCountyNames <- plyr::rename(dsCountyNames, replace=c(Name="CountyName"))
dsMsur <- merge(x=dsMsur, y=dsCountyNames, by.x="CountyID", by.y="ID")

# CollapseMsur <- function( df ) {
#   with(df, data.frame(
#     
#   ))
# }
startTime <- Sys.time()
dsMsurCollapsed <- ddply(dsMsur, "KK", subset, order(DateFind)==1)
Sys.time() - startTime 

ds <- merge(x=ds, y=dsMsurCollapsed, by="KK", all.x=TRUE, all.y=FALSE)

variablesToDrop <- c("CmpltDt", "MsurSource", "DateFind")
ds <- ds[, !(colnames(ds) %in% c("CmpltDt", "MsurSource", "DateFind"))]

dsSummaryCounty <- count(ds, c("CountyID", "CountyName"))
dsSummaryCountyYear <- count(ds, c("CountyID", "CountyName", "MsurYear"))

dsSummaryCounty <- plyr::rename(dsSummaryCounty, replace=c(freq="Count"))
dsSummaryCountyYear <- plyr::rename(dsSummaryCountyYear, replace=c(freq="Count"))

write.csv(ds, pathOutputVictim, row.names=FALSE)
write.csv(dsSummaryCounty, pathOutputSummaryCounty, row.names=FALSE)
write.csv(dsSummaryCountyYear, pathOutputSummaryCountyYear, row.names=FALSE)
