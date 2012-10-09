rm(list=ls(all=TRUE))  #Clears variables
require(RODBC)
require(plyr) #For renaming columns
require(reshape2) #For melting
require(lubridate) #For dealing with dates

#If the parallel version is used:
# library(foreach)
# library(doParallel)
# workers <- makeCluster(4) #4 threads
# registerDoParallel(workers)

pathReadonlyDatasets <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/SafeCareCostEffectiveness/ReadonlyDatabases"
pathWorkingDatasets <- "//dch-res/PEDS-FILE-SV/Data/CCAN/CCANResEval/SafeCareCostEffectiveness/WorkingDatasets"
# pathWorkingDatasets <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets"
pathOutputAllegation <- file.path(pathWorkingDatasets, "Allegation.csv")
pathOutputSummaryCounty <- file.path(pathWorkingDatasets, "CountCounty.csv")
pathOutputSummaryCountyYear <- file.path(pathWorkingDatasets, "CountCountyYear.csv")
pathOutputSummaryCountyType <- file.path(pathWorkingDatasets, "CountCountyType.csv")

# pathOcs2000 <- file.path(pathReadonlyDatasets, "KIDS07_Backup.accdb")
pathKids07 <- file.path(pathReadonlyDatasets, "KIDS07_Backup.accdb")
pathCountyLookupTable <- file.path(pathReadonlyDatasets, "CountyLookups.csv")


startTime <- Sys.time()
# channelKids07 <- odbcConnect(dsn="SafeCareKids07")
channelKids07 <- odbcConnectAccess2007(pathKids07)
# dsReferral <- sqlFetch(channelKids07, "Chrefer")
# dsAllegation <- sqlFetch(channelKids07, "Challeg")
dsReferral <- sqlQuery(channelKids07, query="SELECT ReferId, CaseId, ReferDt, ReferTyp FROM Chrefer") #CmpltDt,
dsAllegation <- sqlQuery(channelKids07, query="SELECT ReferId, VctmId, AbuseFlg, NglctFlg, SexAbFlg FROM Challeg")
odbcClose(channelKids07)
Sys.time() - startTime 
# rm(dsReferral)

dsReferral <- plyr::rename(dsReferral, replace=c(ReferId="ReferralID", CaseId="KK", ReferDt="ReferralDate"))
dsReferral$ReferralYear <- as.integer(year(dsReferral$ReferralDate))
dsReferral$ReferralMonth <- as.Date(ISOdate(year(dsReferral$ReferralDate), month(dsReferral$ReferralDate), 15))
dsReferral <- dsReferral[, colnames(dsReferral) !="ReferralDate"]

dsAllegation <- plyr::rename(dsAllegation, replace=c(ReferId="ReferralID", VctmId="VictimID", AbuseFlg="Abuse", NglctFlg="Neglect", SexAbFlg="SexualAbuse"))
length(unique(dsAllegation$ReferralID))
length(unique(dsAllegation$VictimID))

if( !all(dsAllegation$Abuse %in% c("Y", "N")) ) stop("There was at least one value in dsAllegation$Abuse that wasn't 'Y' or 'N'")
if( !all(dsAllegation$Neglect %in% c("Y", "N")) ) stop("There was at least one value in dsAllegation$Neglect that wasn't 'Y' or 'N'")
if( !all(dsAllegation$SexualAbuse %in% c("Y", "N")) ) stop("There was at least one value in dsAllegation$SexualAbuse that wasn't 'Y' or 'N'")
dsAllegation$Abuse <- ifelse(dsAllegation$Abuse=="Y", 1, 0)
dsAllegation$Neglect <- ifelse(dsAllegation$Neglect=="Y", 1, 0)
dsAllegation$SexualAbuse <- ifelse(dsAllegation$SexualAbuse=="Y", 1, 0)



#table(dsAllegation$ReferralID, dsAllegation$VictimID)
# count(dsAllegation. c("ReferralID", "VictimID"))
# count(dsAllegation, "ReferId")

# startTime <- Sys.time()
# CollapseAllegations <- function( df ) {
#   with(df, data.frame(
#     Abuse=any(df$AbuseFlg=="Y"),
#     Neglect=any(df$NglctFlg=="Y"),
#     SexualAbuse=any(df$SexAbFlg=="Y")
#   ))
# }
# ### TODO: don't mapreduce on any variable
# dsAllegationByVictimAndReferral <- ddply(dsAllegation, .variables=c("ReferralID", "VictimID"), CollapseAllegations, .parallel=FALSE)
# Sys.time() - startTime #Serial: 2.664216 mins

# ds <- merge(x=dsReferral, y=dsAllegation, by="ReferralID", all.x=TRUE, all.y=FALSE)
#ds <- merge(x=dsAllegationByVictimAndReferral, y=dsReferral, by="ReferralID", all.x=TRUE, all.y=FALSE)
ds <- merge(x=dsAllegation, y=dsReferral, by="ReferralID", all.x=TRUE, all.y=FALSE)

dsCountyNames <- read.csv(pathCountyLookupTable)
dsCountyNames <- plyr::rename(dsCountyNames, replace=c(Name="CountyName"))

msurTableNames <- c("MSUR 06-02","MSUR 06-03","MSUR 06-04","MSUR 06-05","MSUR 06-06","MSUR 06-07","MSUR 06-08","MSUR 06-09","MSUR 06-10","MSUR 06-11","MSUR 06-12")
msurYear <- 2002 + seq_along(msurTableNames) - 1 
desiredMsurColumns <- c("MsurSource", "MsurYear", "KK", "county", "dateFIND")
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
  dsMsur <- rbind(dsMsur, dsMsurYear[, desiredMsurColumns])
}
odbcClose(channelOcs2000)
# rm(dsMsurYear)
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
dsMsur <- dsMsur[, colnames(dsMsur) != "County"] #Drop the dirty county variable.
dsMsur <- merge(x=dsMsur, y=dsCountyNames, by.x="CountyID", by.y="ID")

# sum(is.na(ds$KK))
# sum(is.na(ds$ReferralYear))
# sum(is.na(dsMsur$KK))
# sum(is.na(dsMsur$MsurYear))
sum(is.na(dsMsur$CountyName))
ds$Year <- ds$ReferralYear
dsMsur$Year <- dsMsur$MsurYear
ds <- ds[ order(ds$KK, ds$ReferralYear), ]
# ds <- plyr::join(x=ds, y=dsMsur, by=c("KK", "Year"), type="left", match="first") #115167 missing rows
ds <- plyr::join(x=ds, y=dsMsur, by=c("KK"), type="left", match="first") #Only 580 missing rows
ds <- ds[, (colnames(ds) !="Year")]
dsMsur <- dsMsur[, (colnames(dsMsur) !="Year")]
# sum(is.na(ds$CountyName))


# dsMsurCollapsed <- ddply(dsMsur, "KK", subset, rank(DateFind)==1)
# dsTest <- merge(x=ds, y=dsMsurCollapsed, 
#                 by.x=c("KK", "ReferralYear"), all.x=TRUE, 
#                 by.y=c("KK", "MsurYear"), all.y=FALSE)
# dsTest <- dsTest[ order(dsTest$KK, v$ReferralYear), ]


# startTime <- Sys.time()
# # dsMsurCollapsed <- ddply(dsMsur, "KK", subset, order(DateFind)==1) #Should this be rank?
# dsMsurCollapsed <- ddply(dsMsur, "KK", subset, rank(DateFind)==1)
# Sys.time() - startTime #20.0166 secs
# 
# ds <- merge(x=ds, y=dsMsurCollapsed, by="KK", all.x=TRUE, all.y=FALSE)

#variablesToDrop <- c("CmpltDt", "MsurSource", "DateFind")
variablesToDrop <- c("KK","ReferralID","VictimID", "MsurSource", "MsurYear", "DateFind", "ReferralMonth")
ds <- ds[, !(colnames(ds) %in% variablesToDrop)]

class(ds$CountyName)
ds$CountyName <- as.character(ds$CountyName)

dsSummaryCounty <- count(ds, c("CountyID", "CountyName"))
dsSummaryCountyYear <- count(ds, c("CountyID", "CountyName", "ReferralYear"))
dsSummaryCountyType <- count(ds, c("CountyID", "CountyName", "Abuse", "Neglect", "SexualAbuse"))


dsSummaryCounty <- plyr::rename(dsSummaryCounty, replace=c(freq="Count"))
dsSummaryCountyYear <- plyr::rename(dsSummaryCountyYear, replace=c(freq="Count"))
dsSummaryCountyType <- plyr::rename(dsSummaryCountyType, replace=c(freq="Count"))

write.csv(ds, pathOutputAllegation, row.names=FALSE, quote=FALSE)
write.csv(dsSummaryCounty, pathOutputSummaryCounty, row.names=FALSE)
write.csv(dsSummaryCountyYear, pathOutputSummaryCountyYear, row.names=FALSE)
write.csv(dsSummaryCountyType, pathOutputSummaryCountyType, row.names=FALSE)




# melt(dsSummaryCountyType, value.name="Count")
# typesID <-
# vaggregate(.value=dsSummaryCountyType$Count, .group=dsSummaryCountyType$Abuse, .fun=sum)
