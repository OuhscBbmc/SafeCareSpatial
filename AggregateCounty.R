rm(list=ls(all=TRUE))  #Clears variables
require(RODBC)
require(plyr) #For renaming columns

pathWorkingDatasets <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/SafeCareCostEffectiveness/WorkingDatasets"
# pathWorkingDatasets <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathOutputSummaryCounty <- file.path(pathWorkingDatasets, "CountCounty.csv")
pathOutputSummaryCountyYear <- file.path(pathWorkingDatasets, "CountCountyYear.csv")
# pathToOcs2000 <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/Db-Files/OCS/OCS2000.mdb"
# channelOcs2000 <- odbcConnectAccess2007(odbcConnectAccess)

msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04","MSUR 06-05","MSUR 06-06","MSUR 06-07","MSUR 06-08","MSUR 06-09","MSUR 06-10","MSUR 06-11","MSUR 06-12")
msurYear <- 2002 + seq_along(msurTableNames) -1 
# msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04")
desiredColumns <- c("MsurSource", "Year", "KK", "county")
ds <- data.frame(MsurSource=character(0), Year=numeric(0), KK=numeric(0), County=character(0))

#This DSN points to \\dch-res\PEDS-FILE-SV\Data\CCAN\CCANResEval\SafeCareCostEffectiveness\ReadonlyDatabasesOCS2000.mdb
channel2000 <- odbcConnect(dsn="Ocs2000Dsn")
odbcGetInfo(channel2000) 
# dsTables <- sqlTables(channel2000)
for( tableID in seq_along(msurTableNames) ) {
  table <- msurTableNames[tableID]
  
  dsMsurYear <- sqlFetch(channel2000, table, stringsAsFactors=FALSE)
  dsMsurYear$MsurSource <- table
  dsMsurYear$Year <- msurYear[tableID]
  print(paste("Table", table, "has been retrieved with", nrow(dsMsurYear), "rows."))
  ds <- rbind(ds, dsMsurYear[, desiredColumns])
}
odbcClose(channel2000)
ds <- plyr::rename(ds, replace=c(county="County"))
ds <- ds[!is.na(ds$County), ] #Drop the cases with missing counties.

regexPattern <- "[a-z A-Z]"
ds$CountyID <- as.integer(gsub(pattern=regexPattern, replacement="", x=ds$County))
# sort(unique(ds$CountyID))
# class(ds$CountyID)
ds <- ds[, !(colnames(ds) %in% c("County"))] #Drop the dirty county variable.

dsSummaryCounty <- count(ds, c("CountyID"))
dsSummaryCountyYear <- count(ds, c("CountyID", "Year"))

write.csv(dsSummaryCounty, pathOutputSummaryCounty, row.names=FALSE)
write.csv(dsSummaryCountyYear, pathOutputSummaryCountyYear, row.names=FALSE)
