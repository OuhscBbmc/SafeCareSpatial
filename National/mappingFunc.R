# TODO: Add comment
# 
# Author: dbard
###############################################################################
require(sp)
require(rgdal)
require(rgeos)
library(spdep)
library(SpatialEpi)
require(maps)
require(maptools)
#require(UScensus2000)
require(colorspace)
library(classInt)
library(R2WinBUGS)
library(coda)

createMergedDataOthers <- function(ds=COIR60FLdat,regOutcome,dcluswr) {
	stratSampSize <- tapply(!is.na(ds[,regOutcome]),ds$STRATNUM,sum)
	dsStratSS <- data.frame(list(STRATNUM=as.numeric(names(stratSampSize)),nSize=unname(stratSampSize)))
	getStratReg <- unique(ds[,c("STRATNUM","STRATDOM","RESTYPE")]) #will have to make the STRATDOM variable an option, some countries use REGION instead
	if(!nrow(dsStratSS)==nrow(getStratReg)) stop("strata count and strata region datasets of different row sizes")
	dsStratSS2 <- merge(getStratReg,dsStratSS,by.x="STRATNUM",by.y="STRATNUM")
	rownames(dsStratSS2) <- c(1:nrow(dsStratSS2)) 
	
	ptime <- system.time({
				storeCOMILD <- svyby(make.formula(regOutcome), by=~STRATNUM, design=subset(dcluswr,INTYES==1),svymean,verbose=T,na.rm=T)
			})
	ptime/60
#warnings()
	
	if(!nrow(storeCOMILD)==nrow(dsStratSS3)) stop("strata count and strata mean datasets of different row sizes")
	storeCOMILD2 <- merge(storeCOMILD,dsStratSS3,by.x="STRATNUM",by.y="STRATNUM")
	storeCOMILD2$varMean <- storeCOMILD2[,paste("se.",regOutcome,sep="")]^2
	storeCOMILD2$varSRI <- (storeCOMILD2[,regOutcome]*(1-storeCOMILD2[,regOutcome])/storeCOMILD2[,"nSize"])
	storeCOMILD2$deff <- storeCOMILD2$varMean/storeCOMILD2$varSRI
	
#create a smoothed estimate of the design standard errors (refer to You and Zhou, 2011)
	meanDeff <- mean(storeCOMILD2$deff) #estimate the mean design effect for all strata 
	storeCOMILD2$smoothVar <- storeCOMILD2$varSRI*meanDeff
	plot(storeCOMILD2$STRATNUM,storeCOMILD2$smoothVar,type="l",col="green")
	lines(storeCOMILD2$STRATNUM,storeCOMILD2$varMean,lty=3,col="brown")
	
#merge in rates data with the admin boundaries data
	storeCOMILD2$DHSREGNAsc <- storeCOMILD2$DHSREGNA
	takeLook3@data$DHSREGNAtl <- takeLook3@data$DHSREGNA
	addAll <- merge(storeCOMILD2,takeLook3@data,by.x="DHSREGNA",by.y="DHSREGNA",all.x=T,all.y=T)
	if(!nrow(addAll)==nrow(storeCOMILD2)) stop("natural earth FIPS likely does not agree with DHS FIPS codings")
	return(addAll)
}

createMergedDataPE <- function(ds=COIR60FLdat,regOutcome,dcluswr,linkVar) {
	stratSampSize <- tapply(!is.na(ds[,regOutcome]),ds$STRATNUM,sum)
	dsStratSS <- data.frame(list(STRATNUM=as.numeric(names(stratSampSize)),nSize=unname(stratSampSize)))
	getStratReg <- unique(ds[,c("STRATNUM","RESTYPE")]) #will have to make the STRATDOM variable an option, some countries use REGION instead
	if(!nrow(dsStratSS)==nrow(getStratReg)) stop("strata count and strata region datasets of different row sizes")
	dsStratSS2 <- merge(getStratReg,dsStratSS,by.x="STRATNUM",by.y="STRATNUM")
	rownames(dsStratSS2) <- c(1:nrow(dsStratSS2)) 
	
	ptime <- system.time({
				storeCOMILD <- svyby(make.formula(regOutcome), by=~STRATNUM, design=subset(dcluswr,INTYES==1),svymean,verbose=T,na.rm=T)
			})
	ptime/60
#warnings()
	
	if(!nrow(storeCOMILD)==nrow(dsStratSS3)) stop("strata count and strata mean datasets of different row sizes")
	storeCOMILD2 <- merge(storeCOMILD,dsStratSS3,by.x="STRATNUM",by.y="STRATNUM")
	storeCOMILD2$varMean <- storeCOMILD2[,paste("se.",regOutcome,sep="")]^2
	storeCOMILD2$varSRI <- (storeCOMILD2[,regOutcome]*(1-storeCOMILD2[,regOutcome])/storeCOMILD2[,"nSize"])
	giveVar <- .5/storeCOMILD2[,"nSize"]
	storeCOMILD2$varSRI <- ifelse(storeCOMILD2$varSRI==0,(giveVar*(1-giveVar)/storeCOMILD2[,"nSize"]),storeCOMILD2$varSRI)
	storeCOMILD2$deff <- storeCOMILD2$varMean/storeCOMILD2$varSRI
	
#create a smoothed estimate of the design standard errors (refer to You and Zhou, 2011)
	meanDeff <- mean(storeCOMILD2$deff) #estimate the mean design effect for all strata 
	storeCOMILD2$smoothVar <- storeCOMILD2$varSRI*meanDeff
	plot(storeCOMILD2$STRATNUM,storeCOMILD2$smoothVar,type="l",col="green")
	lines(storeCOMILD2$STRATNUM,storeCOMILD2$varMean,lty=3,col="brown")
	
#merge in rates data with the admin boundaries data
	storeCOMILD2[,paste(linkVar,"sc",sep="")] <- storeCOMILD2[,linkVar]
	takeLook3@data[,paste(linkVar,"tl",sep="")] <- takeLook3@data[,linkVar]
	addAll <- merge(storeCOMILD2,takeLook3@data,by.x=linkVar,by.y=linkVar,all.x=T,all.y=T)
	if(!nrow(addAll)==nrow(storeCOMILD2)) stop("natural earth FIPS likely does not agree with DHS FIPS codings")
	return(addAll)
}

createMergedData <- function(ds=COIR60FLdat,regOutcome) {
	stratSampSize <- tapply(!is.na(ds[,regOutcome]),ds$STRATNUM,sum)
	dsStratSS <- data.frame(list(STRATNUM=as.numeric(names(stratSampSize)),nSize=unname(stratSampSize)))
	getStratReg <- unique(ds[,c("STRATNUM","STRATDOM","RESTYPE")]) #will have to make the STRATDOM variable an option, some countries use REGION instead
	if(!nrow(dsStratSS)==nrow(getStratReg)) stop("strata count and strata region datasets of different row sizes")
	dsStratSS2 <- merge(getStratReg,dsStratSS,by.x="STRATNUM",by.y="STRATNUM")
	rownames(dsStratSS2) <- c(1:nrow(dsStratSS2)) 
	
	ptime <- system.time({
				storeCOMILD <- svyby(make.formula(regOutcome), by=~STRATNUM, design=subset(dcluswr,INTYES==1),svymean,verbose=T,na.rm=T)
			})
	ptime/60
#warnings()
	
	if(!nrow(storeCOMILD)==nrow(dsStratSS3)) stop("strata count and strata mean datasets of different row sizes")
	storeCOMILD2 <- merge(storeCOMILD,dsStratSS3,by.x="STRATNUM",by.y="STRATNUM")
	storeCOMILD2$varMean <- storeCOMILD2[,paste("se.",regOutcome,sep="")]^2
	storeCOMILD2$varSRI <- (storeCOMILD2[,regOutcome]*(1-storeCOMILD2[,regOutcome])/storeCOMILD2[,"nSize"])
	storeCOMILD2$deff <- storeCOMILD2$varMean/storeCOMILD2$varSRI
	
#create a smoothed estimate of the design standard errors (refer to You and Zhou, 2011)
	meanDeff <- mean(storeCOMILD2$deff) #estimate the mean design effect for all strata 
	storeCOMILD2$smoothVar <- storeCOMILD2$varSRI*meanDeff
	plot(storeCOMILD2$STRATNUM,storeCOMILD2$smoothVar,type="l",col="green")
	lines(storeCOMILD2$STRATNUM,storeCOMILD2$varMean,lty=3,col="brown")
	
#merge in rates data with the admin boundaries data
	storeCOMILD2$ADM1FIPSsc <- storeCOMILD2$ADM1FIPS
	takeLook3@data$ADM1FIPStl <- takeLook3@data$ADM1FIPS
	addAll <- merge(storeCOMILD2,takeLook3@data,by.x="ADM1FIPS",by.y="ADM1FIPS",all.x=T,all.y=T)
	if(!nrow(addAll)==nrow(storeCOMILD2)) stop("natural earth FIPS likely does not agree with DHS FIPS codings")
	colnames(addAll)
	addAll[is.na(addAll$ADM1FIPSsc) | is.na(addAll$ADM1FIPStl),]
	return(addAll)
}

calcMoranIOthersRegion <- function(regOutcome,takeLookUrban,ordNum=5,probChild=c("") ) {
	takeLookUrbanNB <- poly2nb(takeLookUrban)
	#takeLookRuralNB <- poly2nb(takeLook4[!takeLook4@data[,linkVar] %in% probChild,]) #poly2nb(takeLookRural)
	
	if(!all.equal(takeLookUrban@data$stratID,sort(takeLookUrban@data$stratID))) stop("sp area IDs not sorted") #double-check ordering
	
	moranI <- print(sp.correlogram(takeLookUrbanNB,takeLookUrban@data[,regOutcome], order=ordNum, method="I", style="W",zero.policy=T))
#moranI2 <- moran.test(takeLookUrban@data[,regOutcome],nb2listw(takeLookUrbanNB,style="W",zero.policy=T),zero.policy=T,alternative="two.sided")
#moranI2$estimate[1]/sqrt(moranI2$estimate[3])
	pVal <- moranI[1,'Pr(I) two sided']
	prettyPV <- ifelse(pVal<0.001,"***",ifelse(pVal<0.01,"**",ifelse(pVal<0.05,"*",
							ifelse(pVal<0.10,paste("\u2020"),""))))
	corCoef <- moranI[1,'estimate'] 
	corVar <- moranI[1,'variance'] 
#MIoutputTabU <- rbind(prettyPV,paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",sep=""))
	MIoutputTabU <- paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",prettyPV,sep="")
return(MIoutputTabU)
}

calcMoranIOthers <- function(regOutcome,takeLookUrban,takeLookRural,ordNum=5,probChild=c(""),linkVar=linkVar ) {
	takeLookUrbanNB <- poly2nb(takeLookUrban)
	takeLookRuralNB <- poly2nb(takeLook4[!takeLook4@data[,linkVar] %in% probChild,]) #poly2nb(takeLookRural)
	
	if(!all.equal(takeLookUrban@data$stratID,sort(takeLookUrban@data$stratID))) stop("sp area IDs not sorted") #double-check ordering
	
	moranI <- print(sp.correlogram(takeLookUrbanNB,takeLookUrban@data[,regOutcome], order=ordNum, method="I", style="W",zero.policy=T))
#moranI2 <- moran.test(takeLookUrban@data[,regOutcome],nb2listw(takeLookUrbanNB,style="W",zero.policy=T),zero.policy=T,alternative="two.sided")
#moranI2$estimate[1]/sqrt(moranI2$estimate[3])
	pVal <- moranI[1,'Pr(I) two sided']
	prettyPV <- ifelse(pVal<0.001,"***",ifelse(pVal<0.01,"**",ifelse(pVal<0.05,"*",
							ifelse(pVal<0.10,paste("\u2020"),""))))
	corCoef <- moranI[1,'estimate'] 
	corVar <- moranI[1,'variance'] 
#MIoutputTabU <- rbind(prettyPV,paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",sep=""))
	MIoutputTabU <- paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",prettyPV,sep="")
	
	moranIr <- print(sp.correlogram(takeLookRuralNB,takeLookRural@data[,regOutcome], order=ordNum, method="I", style="W",zero.policy=T))
	#browser()
#moranI2 <- moran.test(takeLookRural@data[,regOutcome],nb2listw(takeLookRuralNB,style="W",zero.policy=T),zero.policy=T,alternative="two.sided")
#moranI2$estimate[1]/sqrt(moranI2$estimate[3])
	pValR <- moranIr[1,'Pr(I) two sided']
	prettyPVR <- ifelse(pValR<0.001,"***",ifelse(pValR<0.01,"**",ifelse(pValR<0.05,"*",
							ifelse(pValR<0.10,paste("\u2020"),""))))
	corCoefR <- moranIr[1,'estimate'] 
	corVarR <- moranIr[1,'variance'] 
	
#MIoutputTabR <- rbind(prettyPV,paste(sprintf("%.3f", corCoefR),"(",sprintf("%.3f", corVarR),")",sep=""))
	MIoutputTabR <- paste(sprintf("%.3f", corCoefR),"(",sprintf("%.3f", corVarR),")",prettyPVR,sep="")
	return(rbind(MIoutputTabU,MIoutputTabR))
}


modCompWB <- function(spNB, modelData2, regOutcome,modNums,yesNo=F) {
	wbNB <- nb2WB(spNB)
	N <- nrow(modelData2)
	y <- modelData2[,regOutcome]
	num <- wbNB$num
	adj <- wbNB$adj
	weights <- wbNB$weights
	urban <- modelData2$URBAN
	stratID <- modelData2$stratID
	stratIDR <- modelData2$stratID + max(modelData2$stratID)*(1-urban) #this is used when rural and urban autocor differs significantly
	J <- length(unique(stratID))
	vare <- c(modelData2$smoothVar)
	
	bym.data1 <- list("N","y","vare","urban")	
	bym.data2 <- list("N","J","y","vare","stratID","urban")
	bym.data3 <- list("N","y","vare","urban")
	bym.data4 <- list("N","J","y","num","adj","weights","vare","stratID","urban")
	bym.data5 <- list("N","J","y","num","adj","weights","vare","stratID","urban")
	bym.data6 <- list("N","J","y","num","adj","weights","vare","stratID","urban")
	bym.data7 <- list("N","J","y","num","adj","weights","vare","stratIDR","urban")
	bym.data8 <- list("N","J","y","num","adj","weights","vare","stratIDR","urban")
	bym.data9 <- list("N","J","y","num","adj","weights","vare","stratIDR","urban")
	bym.data10 <- list("N","J","y","num","adj","weights","vare","stratIDR","urban")
	bym.data11 <- list("N","J","y","num","adj","weights","vare","stratID","stratIDR","urban")
	bym.data12 <- list("N","J","y","num","adj","weights","vare","stratID","urban")
	
	bym.inits1 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1))}	
	bym.inits2 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100))}
	bym.inits3 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),v=rnorm(n=N),tau.v=runif(n=1,1,100))}
	bym.inits4 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100))}	
	bym.inits5 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100),
				mu=rnorm(n=J),tau.mu=runif(n=1,1,100))} 
	bym.inits6 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100),			
				v=rnorm(n=N),tau.v=runif(n=1,1,100))} 
	bym.inits7 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,1,100),tau.lambdaR=runif(n=1,1,100))}
	bym.inits8 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,1,100),tau.lambdaR=runif(n=1,1,100),
				mu=rnorm(n=N),tau.mu=runif(n=1,1,100),tau.muR=runif(n=1,1,100))}
	bym.inits9 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,1,100),tau.lambdaR=runif(n=1,1,100))}
	bym.inits10 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,1,100),tau.lambdaR=runif(n=1,1,100),
				mu=c(rep(NA,J),rnorm(n=J)),tau.muR=runif(n=1,1,100))}
	bym.inits11 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100),
				mu=rnorm(n=N),tau.mu=runif(n=1,1,100),tau.muR=runif(n=1,1,100))}
	bym.inits12 <- function(){list(inter=rnorm(n=1),beta=rnorm(n=1),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100),
				mu=rnorm(n=J),tau.muR=runif(n=1,1,100))}
	
	bym.parameters1 <- c("inter","beta","muy[]") #Common prev	
	bym.parameters2 <- c("inter","beta","sigma.lambda","lambda[]","muy[]") #Reg Het
	bym.parameters3 <- c("inter","beta","sigma.v","v[]","muy[]") #SubReg Het
	bym.parameters4 <- c("inter","beta","sigma.lambda","lambda[]","muy[]") #Reg Autocor	
	bym.parameters5 <- c("inter","beta","sigma.lambda","lambda[]","muy[]","mu[]","sigma.mu") #Reg Het & Reg Autocor
	bym.parameters6 <- c("inter","beta","sigma.lambda","lambda[]","muy[]","sigma.v","v[]") #SubReg Het & Reg Autocor
	bym.parameters7 <- c("inter","beta","sigma.lambda","sigma.lambdaR","lambda[]","muy[]") #Reg Autocor, differs for rural
	bym.parameters8 <- c("inter","beta","sigma.lambda","sigma.lambdaR","lambda[]","muy[]","mu[]","sigma.mu",
			"sigma.muR") #SubReg Het & Reg Autocor, autocor differs for rural
	bym.parameters9 <- c("inter","beta","sigma.lambda","sigma.lambdaR","lambda[]","muy[]") #Reg Autocor, no urban autocor
	bym.parameters10 <- c("inter","beta","sigma.lambda","sigma.lambdaR","lambda[]","muy[]","mu[]",
			"sigma.muR") #SubReg Het & Reg Autocor, no urban autocor 
	bym.parameters11 <- c("inter","beta","sigma.lambda","lambda[]","muy[]","mu[]","sigma.mu",
			"sigma.muR") #Reg Het & Reg Autocor, autocor differs for rural but common u/r reg het
	bym.parameters12 <- c("inter","beta","sigma.lambda","lambda[]","muy[]","mu[]",
			"sigma.muR") #Reg Het & Reg Autocor, no urban autocor but common u/r reg het
	
	#modNum <- 1
#	bym.data <- list(bym.data1,bym.data2,bym.data3,bym.data4,bym.data5,bym.data6,bym.data7,bym.data8,bym.data9,bym.data10)
#	bym.inits <- list(bym.inits1,bym.inits2,bym.inits3,bym.inits4,bym.inits5,bym.inits6,bym.inits7,bym.inits8,bym.inits9,bym.inits10)
#	bym.parameters <- list(bym.parameters1,bym.parameters2,bym.parameters3,bym.parameters4,bym.parameters5,bym.parameters6,bym.parameters7,
#			bym.parameters8,bym.parameters9,bym.parameters10)
	
#start.time1 <- Sys.time()
#map.sim <- bugs(data=bym.data[[modNum]], inits=bym.inits[[modNum]], parameters.to.save=bym.parameters[[modNum]], 
#		model.file=paste("P:\\SPSS\\Elizabeth\\Dissertation\\Dissertation\\BYM",modNum,".odc",sep=""), n.chains=2, 
#		n.iter=100000,n.thin=10,n.burnin=75000, debug=T, 
#		bugs.directory="C:\\Program Files\\winbugs14\\WinBUGS14\\", codaPkg=F)
#stop.time1 <- Sys.time()
#diff.time1 <- stop.time1 - start.time1
#print(map.sim, digits=4)
#print(diff.time1)
	
	
		
	storeRegOut <- data.frame(matrix(NA,max(modNums),ifelse(max(modNums)<7,6,8),byrow=T))
	possNames <- c("DIC","Intcpt","UrbEff","RegVar","SubRVar","RegAuto","RegAutoR","SubRVarR")
	colnames(storeRegOut) <- possNames[1:ncol(storeRegOut)] 
	#browser()
	allBYMlist <- list()
	for(i in modNums){
		#browser()
		start.time1 <- Sys.time()
		map.sim <- bugs(data=get(paste("bym.data",i,sep="")), inits=get(paste("bym.inits",i,sep="")), 
				parameters.to.save=get(paste("bym.parameters",i,sep="")), 
				model.file=paste("P:\\SPSS\\Elizabeth\\Dissertation\\Dissertation\\BYM",i,".odc",sep=""), n.chains=2, 
				n.iter=100000,n.thin=10,n.burnin=75000, debug=yesNo, 
				bugs.directory="C:\\Program Files\\winbugs14\\WinBUGS14\\", codaPkg=F)
		stop.time1 <- Sys.time()
		diff.time1 <- stop.time1 - start.time1
		#print(map.sim, digits=4)
		print(diff.time1)	
		
		BYMlist <- map.sim
		BYMlist$summary["inter","mean"]
		
		storeRegOut$DIC[i] <- paste(sprintf("%.0f", BYMlist$DIC),"(p.eff=",sprintf("%.0f", BYMlist$summary["deviance","n.eff"]),")",sep="")
		storeRegOut$Intcpt[i] <- paste(sprintf("%.3f", BYMlist$summary["inter","mean"]),"[",
				sprintf("%.3f", BYMlist$summary["inter","2.5%"]),",",
				sprintf("%.3f", BYMlist$summary["inter","97.5%"]),"]",sep="")
		storeRegOut$UrbEff[i] <- paste(sprintf("%.3f", BYMlist$summary["beta","mean"]),"[",
				sprintf("%.3f", BYMlist$summary["beta","2.5%"]),",",
				sprintf("%.3f", BYMlist$summary["beta","97.5%"]),"]",sep="")
		if(i %in% c(3,6)){
			sdSReg <- BYMlist$summary["sigma.v",c("mean","2.5%","97.5%")]
			storeRegOut$SubRVar[i] <- paste(sprintf("%.3f", sdSReg["mean"]),"[",
					sprintf("%.3f", sdSReg["2.5%"]),",",
					sprintf("%.3f", sdSReg["97.5%"]),"]",sep="")
		}
		if(i %in% c(2,5,11,12)){
			sdReg <- BYMlist$summary["sigma.lambda",c("mean","2.5%","97.5%")]
			storeRegOut$RegVar[i] <- paste(sprintf("%.3f", sdReg["mean"]),"[",
					sprintf("%.3f", sdReg["2.5%"]),",",
					sprintf("%.3f", sdReg["97.5%"]),"]",sep="")
		}
		if(i %in% c(8,9,10)){
			sdSReg <- BYMlist$summary["sigma.lambda",c("mean","2.5%","97.5%")]
			storeRegOut$SubRVar[i] <- paste(sprintf("%.3f", sdSReg["mean"]),"[",
					sprintf("%.3f", sdSReg["2.5%"]),",",
					sprintf("%.3f", sdSReg["97.5%"]),"]",sep="")
		}		
		if(i %in% c(4,6,7)){
			sdRAuto <- BYMlist$summary["sigma.lambda",c("mean","2.5%","97.5%")]
			storeRegOut$RegAuto[i] <- paste(sprintf("%.3f", sdRAuto["mean"]),"[",
					sprintf("%.3f", sdRAuto["2.5%"]),",",
					sprintf("%.3f", sdRAuto["97.5%"]),"]",sep="")
		}
		if(i %in% c(5,8,11)){
			sdRAuto <- BYMlist$summary["sigma.mu",c("mean","2.5%","97.5%")]
			storeRegOut$RegAuto[i] <- paste(sprintf("%.3f", sdRAuto["mean"]),"[",
					sprintf("%.3f", sdRAuto["2.5%"]),",",
					sprintf("%.3f", sdRAuto["97.5%"]),"]",sep="")
		}
		if(i %in% c(7,9)){
			sdRAutoR <- BYMlist$summary["sigma.lambdaR",c("mean","2.5%","97.5%")]
			storeRegOut$RegAutoR[i] <- paste(sprintf("%.3f", sdRAutoR["mean"]),"[",
					sprintf("%.3f", sdRAutoR["2.5%"]),",",
					sprintf("%.3f", sdRAutoR["97.5%"]),"]",sep="")
		}
		if(i %in% c(8,10)){
			sdSRegR <- BYMlist$summary["sigma.lambdaR",c("mean","2.5%","97.5%")]
			storeRegOut$SubRVarR[i] <- paste(sprintf("%.3f", sdSRegR["mean"]),"[",
					sprintf("%.3f", sdSRegR["2.5%"]),",",
					sprintf("%.3f", sdSRegR["97.5%"]),"]",sep="")
		}
		if(i %in% c(8,10,11,12)){
			sdRAutoR <- BYMlist$summary["sigma.muR",c("mean","2.5%","97.5%")]
			storeRegOut$RegAutoR[i] <- paste(sprintf("%.3f", sdRAutoR["mean"]),"[",
					sprintf("%.3f", sdRAutoR["2.5%"]),",",
					sprintf("%.3f", sdRAutoR["97.5%"]),"]",sep="")
		}		
		
#plot(BYMlist)
		print(paste("model number",i))
		keepvar <- rownames(BYMlist$summary)[grepl("inter|beta|sigma.",rownames(BYMlist$summary))]
		bcoda <- as.mcmc.list(BYMlist)[,keepvar,drop=F]
		print(summary(bcoda))
		print(storeRegOut)
		allBYMlist[[i]] <- BYMlist
	}
	"Done"
	return(list(storeRegOut=storeRegOut,allBYMlist=allBYMlist))
}




my.mapvariable <- function (y, spatial.polygon, ncut = 1000, nlevels = 10, lower = NULL, 
		upper = NULL, main = NULL, xlab = NULL, ylab = NULL,colorP="gray",
		h=240, c. = c(80, 0), l = c(10, 90), power=1) 
{
	if (is.null(lower)) 
		lower <- min(y)
	if (is.null(upper)) 
		upper <- max(y)
	id <- cut(y, breaks = seq(from = lower, to = upper, length = (ncut + 
								1)))
	id <- as.numeric(id)
	id[is.na(id)] <- 0
	id <- id + 1
	#ncut <- 1000
	#breakPoints <- seq(.20,.50,.05)#pretty(dv)
	paletteResource <- rev(sequential_hcl(n=ncut + 1, h=h, c.=c., l = l, power=power))
	#length(paletteResource)
	palette(paletteResource)	
	#ncut <- 10	
	#palette(gray(seq(1, 0, len = (ncut + 1))))
	xrnge <- spatial.polygon@bbox[1, ]
	yrnge <- spatial.polygon@bbox[2, ]
	xd <- xrnge[2] - xrnge[1]
	yd <- yrnge[2] - yrnge[1]
	if (xd > yd) {
		xplot <- xrnge
		yplot <- NULL
		yplot[1] <- ((yrnge[2] + yrnge[1])/2) - xd/2
		yplot[2] <- ((yrnge[2] + yrnge[1])/2) + xd/2
	}
	if (xd <= yd) {
		yplot <- yrnge
		xplot <- NULL
		xplot[1] <- ((xrnge[2] + xrnge[1])/2) - yd/2
		xplot[2] <- ((xrnge[2] + xrnge[1])/2) + yd/2
	}
	def.par <- par(no.readonly = TRUE)
	layout(matrix(c(1, 2), ncol = 2, nrow = 1), heights = c(0.3, 
					0.3), widths = c(0.4, 0.1))
	plot(spatial.polygon, axes = TRUE, col = id)
	if (!is.null(main)) {
		title(main = main)
	}
	if (!is.null(xlab)) {
		title(xlab = xlab)
	}
	if (!is.null(ylab)) {
		title(ylab = ylab)
	}
	plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", 
			ylab = "")
	xlims <- rep(0, nlevels)
	atpts <- rep(0, nlevels)
	for (i in 1:nlevels) {
		xlims[i] <- format(lower + (i - 1) * (upper - lower)/(nlevels - 
							1), digits = 2)
		atpts[i] <- (i - 1)/(nlevels - 1)
	}
	axis(2, at = c(atpts[1:nlevels]), labels = c(xlims[1:nlevels]))
	yb <- seq(0, (nlevels - 2)/(nlevels - 1), 1/(nlevels - 1))
	yt <- seq(1/(nlevels - 1), 1, 1/(nlevels - 1))
	xl <- rep(0, nlevels - 1)
	xr <- rep(1, nlevels - 1)
	#nlevels <- 10
	#nlevels2 <- nlevels+1
	gr <- seq(0, 1, 1/nlevels)
	gr <- max(gr) - gr
	levelCols <- round(seq(from = 1, to = length(paletteResource), length = (nlevels-1)))
	#hist(modelData2[,regOutcome],col=levelCols[10],cex=2)
	rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, col = levelCols, 
			border = TRUE)
	#rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, col = gray(gr), 
	#		border = TRUE)
	palette("default")
	par(def.par)
}




my.latlong2grid <- function (input) 
{
	toradians <- atan(1)/45
	radiusearth <- 0.5 * (6378.2 + 6356.7)
	sine51 <- sin(51.5 * toradians)
	#browser()
	if (is(input)[1] %in% c("SpatialPolygons","SpatialPolygonsDataFrame")) {
		for (i in 1:length(input@polygons)) {
			for (j in 1:length(input@polygons[[i]]@Polygons)) {
				new.coords <- as.matrix(cbind((input@polygons[[i]]@Polygons[[j]]@coords[, 
													1] * toradians) * radiusearth * sine51, (input@polygons[[i]]@Polygons[[j]]@coords[, 
													2] * toradians) * radiusearth))
				colnames(new.coords) <- NULL
				rownames(new.coords) <- NULL
				input@polygons[[i]]@Polygons[[j]]@coords <- new.coords
				input@polygons[[i]]@Polygons[[j]] <- Polygon(input@polygons[[i]]@Polygons[[j]])
			}
			input@polygons[[i]] <- Polygons(input@polygons[[i]]@Polygons, 
					ID = input@polygons[[i]]@ID)
		}
		output <- SpatialPolygons(input@polygons, proj4string = CRS("+proj=utm"))
	}
	else {
		output <- data.frame(cbind(x = (input[, 1] * toradians) * 
								radiusearth * sine51, y = (input[, 2] * toradians) * 
								radiusearth))
	}
	return(output)
}
