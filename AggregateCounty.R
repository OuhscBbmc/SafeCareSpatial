rm(list=ls(all=TRUE))  #Clears variables
require(RODBC)
require(plyr) #For renaming columns

# pathToOcs2000 <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/Db-Files/OCS/OCS2000.mdb"
# channelOcs2000 <- odbcConnectAccess2007(odbcConnectAccess)

msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04","MSUR 06-05","MSUR 06-06","MSUR 06-07","MSUR 06-08","MSUR 06-09","MSUR 06-10","MSUR 06-11","MSUR 06-12")
# msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04")
desiredColumns <- c("MsurSource", "KK", "county")
ds <- data.frame(MsurSource=character(0), KK=numeric(0), County=character(0))

#This DSN points to \\dch-res\PEDS-FILE-SV\Data\CCAN\CCANResEval\SafeCareCostEffectiveness\ReadonlyDatabasesOCS2000.mdb
channel2000 <- odbcConnect(dsn="Ocs2000Dsn")
odbcGetInfo(channel2000) 
# dsTables <- sqlTables(channel2000)
for( table in msurTableNames ) {
  dsMsurYear <- sqlFetch(channel2000, table, stringsAsFactors=FALSE)
  dsMsurYear$MsurSource <- table
  print(paste("Table", table, "has been retrieved with", nrow(dsMsurYear), "rows."))
  ds <- rbind(ds, dsMsurYear[, desiredColumns])
}
odbcClose(channel2000)
ds <- plyr::rename(ds, replace=c(county="County"))

regexPattern <- "[a-z A-Z]"
ds$CountyID <- as.integer(gsub(pattern=regexPattern, replacement="", x=ds$County))
# sort(unique(ds$CountyID))
# class(ds$CountyID)