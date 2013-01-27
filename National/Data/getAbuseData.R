# TODO: Add comment
# 
# Author: dbard
###############################################################################

#getwd()
setwd("C:\\Users\\dbard\\Documents\\GitHub\\SafeCare CE\\SafeCareSpatial\\National\\Data")
fakeLong <- read.table("FakeLongitudinalData.csv",sep=",",header=T)


createDatDF <- function(fileName, datColNames, thisYear) {
	datLines <- readLines(fileName,warn=F)
	numDatCols <- length(datColNames)
	screenDat <- data.frame(matrix(NA,1,numDatCols))
	colnames(screenDat) <- datColNames
	
	for(i in 1:length(datLines)){
		i <- 1
		startStateName <- 1
		endStateName <- max(which(tolower(unlist(strsplit(datLines[i],""))) %in% letters))
		endLine <- nchar(datLines[i])
		stateNames <- tolower(substring(datLines[i],startStateName,endStateName))	
		keepOther <- unlist(strsplit(substring(datLines[i],endStateName+2,endLine),"\\s"))
		keepOther <- keepOther[keepOther!=""]
		if(length(keepOther)>0 & length(keepOther)!=(numDatCols-2)) stop(paste("Missing data present on row",i))
		if(length(keepOther)==0) keepOther <- rep(NA,numDatCols-2)
		screenDat[i,] <- c(stateNames,thisYear,keepOther)	
	}
	return(screenDat)
}

screenList <- list()

for(i in 1:5){
	fileName <- paste0("screen",c(2007:2011)[i],".txt")
	datColNames <- c("StateName","Year","childPop","screenInCount","screenInPercent","screenOutCount","screenOutPercent","totRefCount","totRefRate")
	
	screenList[[i]] <- createDatDF(fileName = fileName, datColNames = datColNames, thisYear = c(2007:2011)[i])
}
"Done"

screenDat <- do.call("rbind",screenList)

fileName <- paste0("investA",c(2007:2011)[5],".txt")
datColNames <- c("StateName","Year","substantiated","indicated","altRespVict","altRespNonVict","unsubstantiated","intentFalse")

practice <- createDatDF(fileName = fileName, datColNames = datColNames, thisYear = c(2007:2011)[5])


screenDat$StateName[!screenDat$StateName %in% fakeLong$StateName]
#[1] "alaska"      "hawaii"      "puerto rico"
fakeLong$StateName[!fakeLong$StateName %in% screenDat$StateName]

colnames(fakeLong)[colnames(fakeLong) %in% colnames(screenDat)]

realScreenDat <- merge(fakeLong,screenDat,all.x=T)

write.table(realScreenDat,"realScreenDat.csv",sep=",",row.names = F)







