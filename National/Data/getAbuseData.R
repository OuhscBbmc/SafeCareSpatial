# TODO: Add comment
# 
# Author: dbard
###############################################################################

#getwd()
setwd("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National/Data")
fakeLong <- read.table("FakeLongitudinalData.csv",sep=",",header=T)

#fix 2007 tables

#fileName <- paste0("refResponse",c(2007:2011)[1],".txt")
#fileName <- paste0("unqVictims",c(2007:2011)[1],"a.txt")
#datLines <- readLines(fileName,warn=F)
#screenDat <- data.frame(matrix(NA,1,4))
#
#for(i in 1:length(datLines)){
#	startStateName <- 1
#	endStateName <- max(which(tolower(unlist(strsplit(datLines[i],""))) %in% letters))
#	endLine <- nchar(datLines[i])
#	stateNames <- tolower(substring(datLines[i],startStateName,endStateName))	
#	keepOther <- unlist(strsplit(substring(datLines[i],endStateName+2,endLine),"\\s"))
#	keepOther <- keepOther[keepOther!=""]
#	lenKO <- length(keepOther)
#	if(length(keepOther)>0) screenDat[i,] <- c(stateNames,keepOther[4],keepOther[5],keepOther[6])	
#}
#
##colnames(screenDat) <- c("State","Substantiated","Total Report Dispositions")
#colnames(screenDat) <- c("State","childPop","numDup","rateDup")
##write.table(screenDat,"refResponse2007a.csv",sep=",",row.names = F)
#write.table(screenDat,"unqVictims2007a.csv",sep=",",row.names = F)

#unqData <- list()
#
#chk07 <- read.table(paste0("unqVictims",c(2007:2011)[1],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#colnames(chk07) <- c("StateName","childPop","numDup","rateDup","numUnq","rateUnq")
#chk07$Year <- 2007
#unqData[[1]] <- chk07[,c("StateName","Year","childPop","numDup","rateDup","numUnq","rateUnq")]
#
#chk08 <- read.table(paste0("unqVictims",c(2007:2011)[2],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#colnames(chk08) <- c("StateName","childPop","numDup","rateDup","numUnq","rateUnq")
#chk08$Year <- 2008
#unqData[[2]] <- chk08[,c("StateName","Year","childPop","numDup","rateDup","numUnq","rateUnq")]
#
#chk09 <- read.table(paste0("unqVictims",c(2007:2011)[3],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#colnames(chk09) <- c("StateName","childPop","numDup","rateDup","numUnq","rateUnq")
#chk09$Year <- 2009
#unqData[[3]] <- chk09[,c("StateName","Year","childPop","numDup","rateDup","numUnq","rateUnq")]
#
#
#numTrim <- function(x){ gsub("[^0-9^.]","",x) }
#numTrim(chk10[,"rateDup"])
#chk10 <- read.table(paste0("unqVictims",c(2007:2011)[4],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#colnames(chk10) <- c("StateName",paste0(rep(c("childPop","numDup","perDup"),8),rep(1:8,each=3)),"numUnk","perUnk","numUnq")
#chk10 <- chk10[2:nrow(chk10),]
#for(i in 2:ncol(chk10)){
#	chk10[,i] <- as.numeric(numTrim(chk10[,i]))
#}
#chk10$childPop <- apply(chk10[,paste0("childPop",1:8)],1,sum)
##cbind(chk10[!chk10$StateName %in% c("Alaska","Puerto Rico","District of Columbia"),c("StateName","testPop")],
##		realScreenDat[realScreenDat$Year==2010,c("StateName","ChildPopulation")])
##all.equal(chk10[!chk10$StateName %in% c("Alaska","Puerto Rico","District of Columbia"),c("testPop")],
##		realScreenDat[realScreenDat$Year==2010,c("ChildPopulation")])
#chk10$numDup <- apply(chk10[,paste0("numDup",1:8)],1,sum)
#chk10$rateDup <- (chk10$numDup/chk10$childPop)*1000
#chk10$rateUnq <- (chk10$numUnq/chk10$childPop)*1000
#chk10$Year <- 2010
#unqData[[4]] <- chk10[,c("StateName","Year","childPop","numDup","rateDup","numUnq","rateUnq")]
#
#chk11 <- read.table(paste0("unqVictims",c(2007:2011)[5],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#colnames(chk11) <- c("StateName","childPop","numUnq","rateUnq")
#chk11$Year <- 2011
#chk11[,c("numDup","rateDup")] <- NA
#unqData[[5]] <- chk11[,c("StateName","Year","childPop","numDup","rateDup","numUnq","rateUnq")]
#
#for(j in 1:5){
#	for(i in 2:ncol(unqData[[j]])){
#		unqData[[j]][,i] <- as.numeric(numTrim(unqData[[j]][,i]))
#	}
#}
#
#realUnqData <- do.call("rbind",unqData)
#realUnqData$StateName <- tolower(realUnqData$StateName)
#realUnqData$StateName <- gsub(' +$','',realUnqData$StateName)
#
#realUnqData$StateName[!realUnqData$StateName %in% fakeLong$StateName]
##[1] "alaska"      "hawaii"      "puerto rico"
#fakeLong$StateName[!fakeLong$StateName %in% realUnqData$StateName]
#
#colnames(fakeLong)[colnames(fakeLong) %in% colnames(realUnqData)]
#
#realUnqData <- merge(fakeLong,realUnqData,all.x=T)
#
#write.table(realUnqData,"realUnqData.csv",sep=",",row.names = F)

realUnqData <- read.table("realUnqData.csv",header=T,sep=",")

#refData <- list()
#
#for(i in 1:4){
#	chk <- read.table(paste0("refResponse",c(2007:2011)[i],".csv"),na.strings = "",sep=",",header=T)#fill=T,,colClasses = "character")
#	chk[,1] <- tolower(chk[,1])
#	chk$Year <- c(2007:2011)[i]
#	colnames(chk) <- c("StateName","Subst","Indic","Alt","totResponse","Year")
#	refData[[i]] <- chk[,c("StateName","Year","Subst","Indic","Alt","totResponse","Year")]
#}
#
#refData <- do.call("rbind",refData)
#
#refData$StateName[!refData$StateName %in% fakeLong$StateName]
##[1] "alaska"      "hawaii"      "puerto rico"
#fakeLong$StateName[!fakeLong$StateName %in% refData$StateName]
#
#colnames(fakeLong)[colnames(fakeLong) %in% colnames(refData)]
#
#realRespDat <- merge(fakeLong,refData,all.x=T)
#
#write.table(realRespDat,"realRespDat.csv",sep=",",row.names = F)

realRespDat <- read.table("realRespDat.csv",header=T,sep=",")

#createDatDF <- function(fileName, datColNames, thisYear) {
#	datLines <- readLines(fileName,warn=F)
#	numDatCols <- length(datColNames)
#	screenDat <- data.frame(matrix(NA,1,numDatCols))
#	colnames(screenDat) <- datColNames
#	
#	for(i in 1:length(datLines)){
#		startStateName <- 1
#		endStateName <- max(which(tolower(unlist(strsplit(datLines[i],""))) %in% letters))
#		endLine <- nchar(datLines[i])
#		stateNames <- tolower(substring(datLines[i],startStateName,endStateName))	
#		keepOther <- unlist(strsplit(substring(datLines[i],endStateName+2,endLine),"\\s"))
#		keepOther <- keepOther[keepOther!=""]
#		if(length(keepOther)>0 & length(keepOther)!=(numDatCols-2)) stop(paste("Missing data present on row",i))
#		if(length(keepOther)==0) keepOther <- rep(NA,numDatCols-2)
#		screenDat[i,] <- c(stateNames,thisYear,keepOther)	
#	}
#	return(screenDat)
#}
#
#screenList <- list()
#
#for(i in 1:5){
#	fileName <- paste0("screen",c(2007:2011)[i],".txt")
#	datColNames <- c("StateName","Year","childPop","screenInCount","screenInPercent","screenOutCount","screenOutPercent","totRefCount","totRefRate")
#	
#	screenList[[i]] <- createDatDF(fileName = fileName, datColNames = datColNames, thisYear = c(2007:2011)[i])
#}
#"Done"
#
#screenDat <- do.call("rbind",screenList)
#
#fileName <- paste0("investA",c(2007:2011)[5],".txt")
#datColNames <- c("StateName","Year","substantiated","indicated","altRespVict","altRespNonVict","unsubstantiated","intentFalse")
#
#practice <- createDatDF(fileName = fileName, datColNames = datColNames, thisYear = c(2007:2011)[5])
#
#
#screenDat$StateName[!screenDat$StateName %in% fakeLong$StateName]
##[1] "alaska"      "hawaii"      "puerto rico"
#fakeLong$StateName[!fakeLong$StateName %in% screenDat$StateName]
#
#colnames(fakeLong)[colnames(fakeLong) %in% colnames(screenDat)]
#
#realScreenDat <- merge(fakeLong,screenDat,all.x=T)
#
#write.table(realScreenDat,"realScreenDat.csv",sep=",",row.names = F)

realScreenDat <- read.table("realScreenDat.csv",header=T,sep=",")







