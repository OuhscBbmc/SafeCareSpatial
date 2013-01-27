#Run this plot first so that you see what the data in these two shapefiles look like
#rm(list=ls(all=TRUE))
install.packages(c("arm","sp","rgdal","rgeos","spdep","foreign"))
require(sp)
require(rgdal)
require(rgeos)
library(spdep)

# if( names(dev.cur()) != "null device" ) dev.off()
# deviceWidth <- 8 #20 #10 #6.5
# heightToWidthRatio <- 1.2
# windows(width=deviceWidth, height=deviceWidth*heightToWidthRatio)


#look at cluster sample sizes
cePredVars <- c("pCNoSxb","pCNoXPb","frcsex","pComb","pCNoPSb")
stratSampSize <- tapply(!is.na(COIR60FLdat[,cePredVars[1]]),COIR60FLdat$STRATNUM,sum)
dsStratSS <- data.frame(list(STRATNUM=as.numeric(names(stratSampSize)),nSize=unname(stratSampSize)))
getStratReg <- unique(COIR60FLdat[,c("STRATNUM","STRATDOM","RESTYPE")]) #will have to make the STRATDOM variable an option, some countries use REGION instead
if(!nrow(dsStratSS)==nrow(getStratReg)) stop("strata count and strata region datasets of different row sizes")
dsStratSS2 <- merge(getStratReg,dsStratSS,by.x="STRATNUM",by.y="STRATNUM")
rownames(dsStratSS2) <- c(1:nrow(dsStratSS2)) 

#go ahead and bring in FIPS Code now so that you can later merge mapping info
directory <- "C:\\Users\\dbard\\workspace\\Ediss"
fileName <- "COGE61FL_revised"
takeLook2 <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)
dhsSTRATCLUST <- unique(COIR60FLdat[,c("STRATNUM","CLUSTER")])
#nrow(takeLook2@data)==nrow(dhsSTRATCLUST)
dhsADMSTRATCLUST <- merge(takeLook2@data,dhsSTRATCLUST,by.x="DHSCLUST",by.y="CLUSTER")
mapLinks <- unique(dhsADMSTRATCLUST[,c("STRATNUM","ADM1FIPSNA","ADM1FIPS","ADM1DHS","ADM1NAME","DHSREGNA","URBAN_RURA")])
if(!nrow(mapLinks)==nrow(dsStratSS2)) stop("strata count and strata region datasets of different row sizes")
#dsStratSS2[dsStratSS2$STRATNUM %in% 1:10,]
#mapLinks[mapLinks$STRATNUM %in% 1:10,]
dsStratSS3 <- merge(dsStratSS2,mapLinks,by.x="STRATNUM",by.y="STRATNUM")
dsStratSS3$URBAN <- (dsStratSS3$URBAN_RURA=="U")*1

dcluswr <- svydesign(id=~CLUSTER, strata=~STRATNUM, weights=~SAMPWGT2, data=COIR60FLdat)#,nest=T) 
summary(dcluswr)


ptime <- system.time({
			storeCOMILD <- svyby(make.formula(cePredVars[2]), by=~STRATNUM, design=subset(dcluswr,INTYES==1),svymean,verbose=T,na.rm=T)
})
ptime/60
#warnings()

if(!nrow(storeCOMILD)==nrow(dsStratSS3)) stop("strata count and strata mean datasets of different row sizes")
storeCOMILD2 <- merge(storeCOMILD,dsStratSS3,by.x="STRATNUM",by.y="STRATNUM")
storeCOMILD2$varMean <- storeCOMILD2[,paste("se.",cePredVars[2],sep="")]^2
storeCOMILD2$varSRI <- (storeCOMILD2[,cePredVars[1]]*(1-storeCOMILD2[,cePredVars[1]])/storeCOMILD2[,"nSize"])
storeCOMILD2$deff <- storeCOMILD2$varMean/storeCOMILD2$varSRI

#create a smoothed estimate of the design standard errors (refer to You and Zhou, 2011)
meanDeff <- mean(storeCOMILD2$deff) #estimate the mean design effect for all strata 
storeCOMILD2$smoothVar <- storeCOMILD2$varSRI*meanDeff
plot(storeCOMILD2$STRATNUM,storeCOMILD2$smoothVar,type="l",col="green")
lines(storeCOMILD2$STRATNUM,storeCOMILD2$varMean,lty=3,col="brown")

#bring in spDep object and extract necessary info;
directory <- "C:\\Users\\dbard\\workspace\\Ediss"
fileName <- "ne_10m_admin_1_states_provinces_shp"
takeLook2 <- readOGR(dsn=file.path(directory,"10m-admin-1-states-provinces-shp"), layer=fileName)

#names(takeLook2@data)
#takeLook2@data$ADM0_A3[grepl("BOL",takeLook2@data$ADM0_A3,ignore.case=T)]
#takeLook3 <- takeLook2[grepl("Bolivia",takeLook2@data$NEV_Countr,ignore.case=T),]
# returns string w/o leading or trailing whitespace 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

takeLook3 <- takeLook2[takeLook2@data$ADM0_A3=="COL",]
takeLook3@data$ADM1FIPS <- paste(takeLook3@data$FIPS_1)
takeLook3@data$ADM1FIPS[is.na(takeLook3@data$FIPS_1)] <- "CO33"
takeLook3@data$NAME_1[takeLook3@data$ADM1FIPS=="CO33"]

dhsFIPS <- unique(storeCOMILD2[order(storeCOMILD2$ADM1FIPS),c("ADM1FIPSNA","ADM1FIPS")])
natearFIPS <- takeLook3@data[order(takeLook3@data$ADM1FIPS),c("NAME_1","ADM1FIPS")]
nrow(dhsFIPS)==nrow(natearFIPS)
cbind(dhsFIPS,natearFIPS)

#manually fix the FIPS codes in the natural earth datafile
takeLook3@data[takeLook3@data$NAME_1=="Valle del Cauca" & takeLook3@data$ADM1FIPS=="CO11",c("ADM1FIPS")] <- 
		paste(dhsFIPS[dhsFIPS$ADM1FIPSNA=="Valle del Cauca" & dhsFIPS$ADM1FIPS=="CO29",c("ADM1FIPS")])
takeLook3@data[takeLook3@data$NAME_1=="San Andrés y Providencia" & takeLook3@data$ADM1FIPS=="CO17",c("ADM1FIPS")] <- 
		paste(dhsFIPS[dhsFIPS$ADM1FIPSNA=="San Andres and Providencia" & dhsFIPS$ADM1FIPS=="CO25",c("ADM1FIPS")])
takeLook3@data[takeLook3@data$NAME_1=="Magdalena" & takeLook3@data$ADM1FIPS=="CO18",c("ADM1FIPS")] <- 
		paste(dhsFIPS[dhsFIPS$ADM1FIPSNA=="Magdalena" & dhsFIPS$ADM1FIPS=="CO38",c("ADM1FIPS")])

#all.equal(sort(takeLook3@data$ADM1FIPS),sort(unique(storeCOMILD2$ADM1FIPS)))

#notice the ordering of admin areas does not agree with ordering of FIPS code
#create new admin id variable that agrees with default ordering of admin boundary data 
takeLook3@data[,c("NAME_1","ADM1FIPS")]
takeLook3@data$stratID <- 1:nrow(takeLook3@data)
#takeLook3@data$stratID2 <- 1:nrow(takeLook3@data)


#merge in rates data with the admin boundaries data
storeCOMILD2$ADM1FIPSsc <- storeCOMILD2$ADM1FIPS
takeLook3@data$ADM1FIPStl <- takeLook3@data$ADM1FIPS
addAll <- merge(storeCOMILD2,takeLook3@data,by.x="ADM1FIPS",by.y="ADM1FIPS",all.x=T,all.y=T)
if(!nrow(addAll)==nrow(storeCOMILD2)) stop("natural earth FIPS likely does not agree with DHS FIPS codings")
colnames(addAll)
addAll[is.na(addAll$ADM1FIPSsc) | is.na(addAll$ADM1FIPStl),]
#windows()
#plot(takeLook3)#, col="gray70")
#title(main="Columbia")
#jitterCoord <- coordinates(takeLook3)
#labelD1 <- takeLook3@data$NAME_1
#text(jitterCoord, labels=labelD1, cex=.8)

spNB <- poly2nb(takeLook3) #transform sn object to neighbors list object
#spNB <- poly2nb(takeLook3[takeLook3@data$stratID!=7,])
summary(spNB)
getwd()
nbDir <- file.path("C:\\Users\\dbard\\workspace\\Ediss")
nbMatFile <- paste("col","NB.adj",sep="")
nb2INLA(file.path(nbDir,nbMatFile), spNB) #creates INLA neighorhood object and stores in file

#add cleaner plotting names to takeLook3
takeLook4 <- takeLook3
cleanNames <- unique(addAll[,c("ADM1FIPSNA","stratID")])
takeLook4@data <- merge(takeLook3@data,cleanNames,sort=F)
takeLook4@data[,c("ADM1FIPSNA","NAME_1")]
windows()
colQuin <- ifelse(takeLook4@data$stratID==11,"red","white")
plot(takeLook4,col=colQuin)#, col="gray70")
title(main="Columbia")
jitterCoord <- coordinates(takeLook4)
labelD1 <- takeLook4@data$ADM1FIPSNA
text(jitterCoord, labels=labelD1, cex=.8)

cleanNames[cleanNames$stratID %in% c(6),]


############create all model formulas
createFormulas <- function(regOutcome, nbMatFile) {
	formula1 <- as.formula(paste(regOutcome,
					"~ URBAN+f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T)))"))
	formula2 <- as.formula(paste(regOutcome,
					"~ URBAN+f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T)))+",
					"f(STRATNUM2, model='iid',param = c(prior.iid),initial = c(initial.iid))",sep=""))
	formula3 <- as.formula(paste(regOutcome,
					"~ URBAN+f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T))) +",
					"f(stratID, model='iid',param = c(prior.iid),initial = c(initial.iid))",sep=""))
	formula4 <- as.formula(paste(regOutcome,
					"~ URBAN+f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T))) +",
					"f(stratID2, model='besag',graph='",nbMatFile,"',param = c(prior.besag),initial = c(initial.besag)",
					",adjust.for.con.comp=T) + f(STRATNUM2, model='iid',param = c(prior.iid),initial = c(initial.iid))",sep=""))
	formula5 <- as.formula(paste(regOutcome,
					"~ URBAN+f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T))) +",
					"f(stratID2, model='besag',graph='",nbMatFile,"',param = c(prior.besag),initial = c(initial.besag)",
					",adjust.for.con.comp=T) + f(stratID, model='iid',param = c(prior.iid),initial = c(initial.iid))",sep=""))
return(c(formula1,formula2,formula3,formula4,formula5))
}




########set up function for changing out the outcome measure
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

#let's model the rates
#source("http://www.math.ntnu.no/inla/givemeINLA.R")
library(INLA)
inla.upgrade(testing=T)

 

runINLA <- function(addAll, regOutcome, nbDir, nbMatFile) {
	modelData <- subset(addAll,select= c("STRATNUM","stratID",regOutcome,"smoothVar","URBAN"))
	modelData$stratID2 <- modelData$stratID
	modelData$STRATNUM2 <- modelData$STRATNUM
	
	
#modelData <- subset(addAll,stratID!=7,select= c("STRATNUM","stratID",cePredVars[1]))
#modelData$stratID <- ifelse(modelData$stratID>7,modelData$stratID-1,modelData$stratID)
#sort(unique(modelData$stratID)) 
#create the known smoothed sampling variance matrix for all areas (strata)
	Q <- Diagonal(nrow(modelData),1/modelData$smoothVar)
	Q[1:10,1:10]
	
#run BYM model with You & Zhou (2011) smoothed sampling variance
	setwd(nbDir)
	prior.iid = c(1,0.01)
	prior.besag = c(1,0.001)
	prior.iid = c(.01,0.01)
	prior.besag = c(.001,0.001)
#prior.iid = c(2,0.01)
#prior.besag = c(2,0.001)
	initial.iid = 4
	initial.besag = 3
	
	formulas <- createFormulas(regOutcome = regOutcome, nbMatFile = nbMatFile) #c(formula1,formula2,formula3,formula4,formula5)
	storeRegOut <- data.frame(matrix(NA,5,6))
	colnames(storeRegOut) <- c("DIC","Intcpt","UrbEff","SubRVar","RegVar","RegAuto")
	for(i in 1:length(formulas)){
		#browser()
		inlaResult <- inla(formula=formulas[[i]],data=modelData,family="gaussian",control.data=list(initial=10,fixed=TRUE),
				control.compute=list(dic=T,cpo=T),control.inla=list(strategy="laplace",npoints=21,int.strategy="grid",diff.logdens=4))
		storeRegOut$DIC[i] <- paste(sprintf("%.1f", inlaResult$dic$dic),"(p.eff=",sprintf("%.1f", inlaResult$dic$p.eff),")",sep="")
		storeRegOut$Intcpt[i] <- paste(sprintf("%.3f", inlaResult$summary.fixed['(Intercept)',"mean"]),"[",
				sprintf("%.3f", inlaResult$summary.fixed['(Intercept)',"0.025quant"]),",",
				sprintf("%.3f", inlaResult$summary.fixed['(Intercept)',"0.975quant"]),"]",sep="")
		storeRegOut$UrbEff[i] <- paste(sprintf("%.3f", inlaResult$summary.fixed['URBAN',"mean"]),"[",
				sprintf("%.3f", inlaResult$summary.fixed['URBAN',"0.025quant"]),",",
				sprintf("%.3f", inlaResult$summary.fixed['URBAN',"0.975quant"]),"]",sep="")
		if(i %in% c(2,4)){
			sdSR <- getVarComp(inlaResult=inlaResult,'Precision for STRATNUM2')
			storeRegOut$SubRVar[i] <- paste(sprintf("%.3f", sdSR$m),"[",
					sprintf("%.3f", sdSR$q1),",",
					sprintf("%.3f", sdSR$q2),"]",sep="")
		}
		if(i %in% c(3,5)){
			sdReg <- getVarComp(inlaResult=inlaResult,'Precision for stratID')
			storeRegOut$RegVar[i] <- paste(sprintf("%.3f", sdReg$m),"[",
					sprintf("%.3f", sdReg$q1),",",
					sprintf("%.3f", sdReg$q2),"]",sep="")
		}
		if(i %in% c(4,5)){
			sdRegAuto <- getVarComp(inlaResult=inlaResult,'Precision for stratID2')
			storeRegOut$RegAuto[i] <- paste(sprintf("%.3f", sdRegAuto$m),"[",
					sprintf("%.3f", sdRegAuto$q1),",",
					sprintf("%.3f", sdRegAuto$q2),"]",sep="")
		}	
	}
return(storeRegOut)
}

regOutcome <- cePredVars[4]
addAll <- createMergedData(regOutcome = regOutcome)
storeRegOutnew <- runINLA(addAll = addAll, regOutcome = regOutcome, nbDir = nbDir, nbMatFile = nbMatFile)

write.table(storeRegOutnew,"clipboard",sep="\t",row.names=F,col.names=F,na="---")

MIoutputTab <- calcMoranI(takeLook4 = takeLook4, addAll = addAll, regOutcome = regOutcome)

write.table(,"clipboard",sep="\t",row.names=F,col.names=F,na="---")








##########look at Moran's I for rate data
#takeLook4@data[,c("ADM1FIPSNA","ADM1FIPS")]
calcMoranI <- function(takeLook4, addAll, regOutcome) {
	takeLookUrban <- takeLook4 
	takeLookRural <- takeLook4[takeLook4@data$ADM1FIPS!="CO34",] #get rid of areas without rural stratum
	nrow(takeLookUrban)
	nrow(takeLookRural)
	takeLookUrban@data <- merge(takeLookUrban@data,addAll[addAll$URBAN==1,c("ADM1FIPS",regOutcome)],by.x="ADM1FIPS",by.y="ADM1FIPS",sort = FALSE )
	takeLookRural@data <- merge(takeLookRural@data,addAll[addAll$URBAN==0,c("ADM1FIPS",regOutcome)],by.x="ADM1FIPS",by.y="ADM1FIPS",sort = FALSE )
	if(!all.equal(takeLook4@data$ADM1FIPS,paste(takeLookUrban@data$ADM1FIPS))) stop("order of urban df & sp objects mismatch")
	if(!all.equal(takeLook4@data$ADM1FIPS[takeLook4@data$ADM1FIPS!="CO34"],paste(takeLookRural@data$ADM1FIPS))) stop("order of rural df & sp objects mismatch")
	takeLookUrbanNB <- poly2nb(takeLookUrban)
	takeLookRuralNB <- poly2nb(takeLookRural)
	
	if(!all.equal(takeLookUrban@data$stratID,sort(takeLookUrban@data$stratID))) stop("sp area IDs not sorted") #double-check ordering
	
	moranI <- print(sp.correlogram(takeLookUrbanNB,takeLookUrban@data[,regOutcome], order=5, method="I", style="W",zero.policy=T))
#moranI2 <- moran.test(takeLookUrban@data[,regOutcome],nb2listw(takeLookUrbanNB,style="W",zero.policy=T),zero.policy=T,alternative="two.sided")
#moranI2$estimate[1]/sqrt(moranI2$estimate[3])
	pVal <- moranI[1,'Pr(I) two sided']
	prettyPV <- ifelse(pVal<0.001,"***",ifelse(pVal<0.01,"**",ifelse(pVal<0.05,"*",
							ifelse(pVal<0.10,paste("\u2020"),""))))
	corCoef <- moranI[1,'estimate'] 
	corVar <- moranI[1,'variance'] 
#MIoutputTabU <- rbind(prettyPV,paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",sep=""))
	MIoutputTabU <- paste(sprintf("%.3f", corCoef),"(",sprintf("%.3f", corVar),")",prettyPV,sep="")
	moranIr <- print(sp.correlogram(takeLookRuralNB,takeLookRural@data[,regOutcome], order=5, method="I", style="W",zero.policy=T))
#moranI2 <- moran.test(takeLookUrban@data[,regOutcome],nb2listw(takeLookUrbanNB,style="W",zero.policy=T),zero.policy=T,alternative="two.sided")
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


#####pull out the estimated "true" rates for best-fitting models

bestMod <- 4
modelData2 <- subset(addAll,select= c("STRATNUM","stratID",regOutcome,"smoothVar","URBAN"))
modelData2$stratID2 <- modelData2$stratID
modelData2$STRATNUM2 <- modelData2$STRATNUM
modelData2$Intercept <- 1
#genericMatrix <- matrix(NA,nrow(modelData2),nrow(modelData2))
dSTRATNUM2 <- matrix(NA,length(unique(modelData2$STRATNUM2)),length(unique(modelData2$STRATNUM2)))
dSTRATID <- matrix(NA,length(unique(modelData2$stratID)),length(unique(modelData2$stratID)))
dSTRATID2 <- matrix(NA,length(unique(modelData2$stratID2)),length(unique(modelData2$stratID2)))
diag(dSTRATNUM2) <- 1
diag(dSTRATID) <- 1
diag(dSTRATID2) <- 1

lcSTRATNUM2 <- dSTRATNUM2[modelData2$STRATNUM2,]
lcSTRATID <- dSTRATID[modelData2$stratID,]
lcSTRATID2 <- dSTRATID2[modelData2$stratID2,]

lcF1 <- inla.make.lincomb("(Intercept)"=1,URBAN=modelData2$URBAN)
lcF2 <- inla.make.lincombs("(Intercept)"=modelData2$Intercept,URBAN=modelData2$URBAN,STRATNUM2 = lcSTRATNUM2)
lcF3 <- inla.make.lincombs("(Intercept)"=modelData2$Intercept,URBAN=modelData2$URBAN,stratID = lcSTRATID)
lcF4 <- inla.make.lincombs("(Intercept)"=modelData2$Intercept,URBAN=modelData2$URBAN,STRATNUM2 = lcSTRATNUM2,stratID2 = lcSTRATID2)
lcF5 <- inla.make.lincombs("(Intercept)"=modelData2$Intercept,URBAN=modelData2$URBAN,stratID = lcSTRATID,stratID2 = lcSTRATID2)
names(lcF5)
lc <- list(lcF1,lcF2,lcF3,lcF4,lcF5)

formulas <- createFormulas(regOutcome = regOutcome, nbMatFile = nbMatFile)
inlaResult <- inla(formula=formulas[[bestMod]],data=modelData2,family="gaussian",control.data=list(initial=20,fixed=TRUE),
		control.compute=list(dic=T,cpo=T),control.inla=list(strategy="laplace",npoints=21,int.strategy="grid",diff.logdens=4),
		lincomb=lc[[bestMod]],
		control.predictor = list(hyper = list(prec = list(initial=20,fixed =T))))
summary(inlaResult)



#inlaResult$summary.fixed['(Intercept)',"mean"]+inlaResult$summary.fixed['URBAN',"mean"]*modelData$URBAN[1] +
#		inlaResult$summary.random$STRATNUM$mean[modelData$STRATNUM[1]]  + 
#		inlaResult$summary.random$STRATNUM2$mean[modelData$STRATNUM2[1]] + 
#		inlaResult$summary.random$stratID2$mean[modelData$stratID2[1]] 
#modelData2[1,regOutcome]
#inlaResult$summary.lincomb.derived$mean[[1]]
#inlaResult$summary.fitted.values$mean[[1]]


cbind(inlaResult$summary.lincomb.derived$mean,modelData2[,regOutcome])
max(abs(inlaResult$summary.lincomb.derived$mean-modelData2[,regOutcome]))



lcF1b <- inla.make.lincomb(URBAN=modelData2$URBAN)
lcF2b <- inla.make.lincombs(STRATNUM2 = lcSTRATNUM2)
lcF3b <- inla.make.lincombs(stratID = lcSTRATID)
lcF4b <- inla.make.lincombs(STRATNUM2 = lcSTRATNUM2,stratID2 = lcSTRATID2)
lcF5b <- inla.make.lincombs(stratID = lcSTRATID,stratID2 = lcSTRATID2)
names(lcF5b)
lcb <- list(lcF1b,lcF2b,lcF3b,lcF4b,lcF5b)

formulas <- createFormulas(regOutcome = regOutcome, nbMatFile = nbMatFile)
inlaResultPP <- inla(formula=formulas[[bestMod]],data=modelData2,family="gaussian",control.data=list(initial=20,fixed=TRUE),
		control.compute=list(dic=T,cpo=T),control.inla=list(strategy="laplace",npoints=21,int.strategy="grid",diff.logdens=4),
		lincomb=lcb[[bestMod]])
summary(inlaResultPP)

inlaResultPP$summary.lincomb.derived$mean[[1]]
inlaResultPP$marginals.lincomb.derived[[1]]
inla.qmarginal(c(0.2), inlaResultPP$marginals.lincomb.derived[[1]])
ppVals <- sapply(inlaResultPP$marginals.lincomb.derived,function(x) inla.qmarginal(c(0.01), x))

sum(ppVals>0) #way too many "hotspots" 

testm1 <- inlaResultPP$marginals.random$STRATNUM2[[modelData2$STRATNUM2[2]]]
testm2 <- inlaResultPP$marginals.random$stratID2[[modelData2$stratID2[2]]]
testlc <- inlaResultPP$marginals.lincomb.derived[[2]] 
windows()
#plot(inla.smarginal(testm), type="l")
s1 = inla.rmarginal(1000, testm1)
s2 = inla.rmarginal(1000, testm2)
var(s1)/(var(s1)+var(s2))
s3 <- s1 + s2
slc <- inla.rmarginal(1000, testlc)

hist(s3, prob=TRUE,col="gray",border="red")
lines(density(s3), lty=2,col="red")
hist(slc, add=TRUE, prob=TRUE,col="lightpink",border="blue")
lines(density(slc), lty=2,col="blue")


windows()
plot(inlaResultPP$marginals.lincomb.derived[[2]])

plot(modelData2$STRATNUM,modelData2[,regOutcome])
for(i in 1:nrow(modelData2)){
	topLine <- modelData2[i,regOutcome] + 2*sqrt(modelData2[i,"smoothVar"])
	botLine <- modelData2[i,regOutcome] - 2*sqrt(modelData2[i,"smoothVar"])
	lines(matrix(c(modelData2[i,"STRATNUM"],topLine,modelData2[i,"STRATNUM"],botLine),2,2,byrow=T))
}

lines(matrix(c(10,.2,20,.3),2,2,byrow=T))








1/inlaResult$summary.hyperpar['Precision for STRATNUM2',"mean"]
1/inlaResult$summary.hyperpar['Precision for STRATNUM2',"0.025quant"]
1/inlaResult$summary.hyperpar['Precision for STRATNUM2',"0.975quant"]






for(i in 1:length())
	lc1 <- inla.make.lincomb("(Intercept)" = 1,stratID=c(NA,1,rep(NA,nrow(modelData)-1)))
names(lc1) <- "lc1"



inlaResult3$summary.random$stratID$ID
sort(modelData$stratID)
dim(Q)

inlaResult3$formula

inlaResult3$summary.lincomb.derived$mean
#inlaResult3$summary.fitted.values$mean[2] == inlaResult3$summary.linear.predictor$mean[2] #true
linPred <- inlaResult3$summary.fixed[,1] + inlaResult3$summary.random$stratID$mean[2] #this basically equals the
inlaResult3$summary.random$STRATNUM$mean[2]
cbind(linPred,modelData[modelData$URBAN==1,cePredVars[1]])
merge(modelData,as.data.frame(list(STRATNUM=1:length(unique(modelData$STRATNUM)),linPred=linPred)))

inlaResult3$summary.random$STRATNUM$mean[3]
inlaResult3$summary.linear.predictor$mean[3]
rnorm(1,0,sqrt(modelData$smoothVar[1]))

idx.failure <- which(inlaResult3$cpo$failure>0) #cpo doesn't seem to be working in this context
#too many of the estimates fail, but if you exclude the fixed sampling variance all is well
#i think that forcing the error variance to be zero and using a fixed sampling variance throws cpo for a loop


#plan for INLA analysis
#1. Record DIC for common prevalence model
#2. Check sensitivity for besag and bym models (use 2-3 different theta priors) (try .01,.001; .001,.0001;.0005,.00005)
#3. Select best fitting (based on DIC) model and output the mean
#4. may want to check the estimation strategies (laplace approx techniques)
#5. check for observations that did not estimate well (ignore this one for now b/c inla's cpo estimation does not work well in this context)




unique(addAll[,c("ADM1FIPSNA","stratID")])
cleanNames[cleanNames$stratID %in% c(11,20,6),]
cleanNames[cleanNames$stratID %in% c(12,13,19),]
cleanNames[cleanNames$stratID %in% c(21,24),]


inla.debug.graph(nbMatFile)  
inla.debug.graph()  
g = inla.read.graph(nbMatFile)
windows()
library(Rgraphviz)
plot(g)


system.file("demodata/Bym-map.R", package="INLA")
Bym.map()

inla.make.lincomb( Ind = c(NA,1), rand = c(NA,-1))

1/199.36
hist(storeCOMILD2[,cePredVars[1]])
var(storeCOMILD2[,cePredVars[1]])

ptm <- proc.time()
formula1 <- as.formula(paste(cePredVars[1],
				"~ f(STRATNUM, model='generic0',Cmatrix=Q,constr=TRUE, diagonal=1e-05,hyper=list(theta=list(initial=1,fixed=T)))"))
inlaResult <- inla(formula1,data=storeCOMILD2,family="gaussian",control.data=list(initial=10,fixed=TRUE),control.compute=list(dic=T)) 
#control.data essentially eliminates the error term since exp(10), the fixed initial prec value, implies var(e) is approx 0
print(proc.time() - ptm)
summary(inlaResult)

-629.15
-167.59

#setwd("P:/SPSS/Elizabeth/Dissertation/")
#write.table(COIR60FLdat[,c("CASEID","CLUSTER","STRATNUM","SAMPWGT","SAMPWGT2","pCNoSxb","INTYES")],
#		"chkSudaan.dat",sep=",",row.names=F,na=".")

windows()
directory <- "P:\\SPSS\\Elizabeth\\Dissertation\\Columbia" ##NOTE TO SELF: readOGR IS FINICKY AND DOESN'T LIKE THE DIRECTORY TO END IN "\\"
fileName <- "COL_adm1"
takeLook <- readOGR(dsn=file.path(directory,"COL_adm1"), layer=fileName)
plot(takeLook)#, col="gray70")
title(main="Columbia")
jitterCoord <- coordinates(takeLook)
labelD1 <- takeLook@data$NAME_1
text(jitterCoord, labels=labelD1, cex=.8)
fileName <- "COGE61FL_revised"
takeLook2 <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)

nrow(unique(takeLook2@data[,c("ADM1FIPSNA","ADM1FIPS")]))

takeLook3 <- takeLook2[takeLook2@data$ADM1FIPSNA=="Bogota Capital District",]
#takeLook2$ADM1FIPSNA
#takeLook2@data$DHSID[2]
#takeLook2@data$DHSCLUST[2]
#length(unique(takeLook2@data$DHSCLUST))
#takeLook2@data$ADM1FIPS[1]
#takeLook2@data$ADM1FIPSNA[1]
#unique(takeLook2@data$ADM1FIPS)
#unqFIPS <- unique(takeLook2@data$ADM1FIPSNA)
#unqFIPSNUM <- unique(as.numeric(takeLook2@data$ADM1FIPSNA))
regCol <- c("black","red")
#subset(COIR60FLdat, subset= STRATDOM=="Santander",select= c(CLUSTER,STRATNUM,STRATDOM,REGION,RESTYPE,RSPTYPE))
jitterCoord2 <- coordinates(takeLook3)
regType <- as.numeric(takeLook3@data$URBAN_RURA)
points(jitterCoord2[,1],jitterCoord2[,2],col=regCol[regType]) #latitude has to come first, then longitude

# takeLook@bbox[1,]
# mask <- owin(xrange=takeLook@bbox[1,], yrange=takeLook@bbox[1,], poly=takeLook)
# str(takeLook)
# str(takeLook@polygons[[1]])
# slotNames(takeLook)
# takeLook@polygons[[1]]
# slot(takeLook@polygons[[1]]
#      
# for( polygonIndex in seq_len(length(slot(takeLook, "polygons"))) ) {
#   
#   sapply(slot(slot(takeLook, "polygons")[[polygonIndex]], "Polygons"), function(x) slot(x, "hole")<- TRUE)
# }
# slot(slot(slot(takeLook, "polygons")[[1]], "Polygons")[[1]], "hole") <- TRUE
# plot(takeLook, col="gray70")
#      
#   plot(mask,add=T)

#shapefile COGE61FL_revised was passed to OpenGeoDa to create a Thiessen Polygon plot of the sampled cluster points.
# OpenGeoDa creates its own rectangular bounding box for these points while it creates the Thiessen Polygons (TP).
#Here is what the TPs look like (requires a newly created shapefile named COGE61FL_revisedTP)
windows()
fileName <- "COGE61FL_revisedTP"
takeLook4 <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)
proj4string(takeLook4) <- proj4string(takeLook)
plot(takeLook4,lwd=.5,lty=2,col="gray90") #latitude has to come first, then longitude

#x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#colorado = readWKT("POLYGON ((-81.841530  -4.228429, -81.841530 15.91247, -66.87033 15.91247, -66.87033  -4.228429, -81.841530 -4.228429))")
colorado = readWKT("POLYGON ((-81.941530  -4.9228429, -81.941530 15.91247, -66.887033 15.91247, -66.887033  -4.9228429, 
                   -81.941530 -4.9228429))")
proj4string(colorado) <- proj4string(takeLook)
#plot(colorado)
difference <- gDifference(colorado, takeLook)
slot(slot(slot(difference, "polygons")[[1]], "Polygons")[[1]], "hole") <- F
plot(difference, col="gray", add=T , pbg='transparent')

#plot(d, col="gray", add=T , pbg='red', border='blue')

# xPoints <- seq(from=takeLook@bbox[1,1], to=takeLook@bbox[1,2], length=100)
# yPoints <- seq(from=takeLook@bbox[2,1], to=takeLook@bbox[2,2], length=100)
# dsColombiaGrid <- data.frame(x=rep(xPoints, times=length(yPoints)), y=rep(yPoints, each=length(xPoints))
# o <- over(takeLook, takeLook4)
# plot(o)

# res <-gIntersection(takeLook, takeLook4)
# plot(res)

#now the hard part
#overlay the national and regional boundaries of Columbia
#I want/need to get rid of any polygon areas that exceed the national boundaries (think cookie cutter analogy)
par(new=F)
plot(takeLook,lwd=2)#, col=countyColors)
title(main="Columbia")
