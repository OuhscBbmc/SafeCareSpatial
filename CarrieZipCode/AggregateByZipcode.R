rm(list=ls(all=TRUE))  #Clear the variables from previous runs. 
require(plyr)

directory <- file.path(getwd(), "CarrieZipCode")
directoryData <- file.path(directory, "Data")
pathMonthly <- file.path(directoryData, "Kahd.csv")
pathSumByZip <- file.path(directoryData, "KahdSumByZip.csv")

#Read the dataset
ds <- read.csv(pathMonthly, stringsAsFactors=F)

#Groom the variables
ds$Month <- as.Date(paste0(ds$Month, "/15"), format="%Y/%m/%d")
if( any(is.na(ds$Month)) ) stop("The Month Variable shouldn't have any NA values.")
if( any(is.na(ds$Zip)) ) stop("The Zip Variable shouldn't have any NA values.")

lapply(ds, class)
# SummarizeZip <- function( df ) {
#   data.frame(
#         
# )}
# cat(colnames(ds), sep="', '")
# dsWithZeros <- colwise(function(x) ifelse(is.na(x), 0, x))(ds)
numericColumns <- c('ReferralsAll', 'NoDispositions', 'ScreenOuts', 'Assessments', 'InvestigationsOutOfHome', 'InvestigationsInHome', 'InvestigationsWithAtLeast1ChildAge5OrYounger', 'InvestigationsConfirmed', 'ChildrenConfirmed', 'ChildrenRemoved')
for( columnName in numericColumns ) {
 ds[, columnName] <-  ifelse(is.na(ds[, columnName]), 0, ds[, columnName])
}
dsZip <- ddply(ds, "Zip", numcolwise(sum))#, na.rm=T)

write.csv(dsZip, file=pathSumByZip,  row.names=F)