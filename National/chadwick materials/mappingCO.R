# TODO: Add comment
# 
# Author: dbard
###############################################################################

library(R2WinBUGS)
library(coda)
library(survey)
country <- c("Columbia","Bolivia","Peru","Dominican Republic","Haiti","Nicaragua")
countryNumber <- 1
countryData <- COIR60FLdat

#look at cluster sample sizes
cePredVars <- c("pCNoSxb","pCNoXPb","frcsex","pComb","pCNoPSb")
stratSampSize <- tapply(!is.na(countryData[,cePredVars[1]]),countryData$STRATNUM,sum)
dsStratSS <- data.frame(list(STRATNUM=as.numeric(names(stratSampSize)),nSize=unname(stratSampSize)))
getStratReg <- unique(countryData[,c("STRATNUM","STRATDOM","RESTYPE")]) #will have to make the STRATDOM variable an option, some countries use REGION instead
if(!nrow(dsStratSS)==nrow(getStratReg)) stop("strata count and strata region datasets of different row sizes")
dsStratSS2 <- merge(getStratReg,dsStratSS,by.x="STRATNUM",by.y="STRATNUM")
rownames(dsStratSS2) <- c(1:nrow(dsStratSS2)) 

#go ahead and bring in FIPS Code now so that you can later merge mapping info
directory <- "P:\\SPSS\\Elizabeth\\Dissertation\\Columbia"
fileName <- "COGE61FL_revised"
takeLook2 <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)
dhsSTRATCLUST <- unique(countryData[,c("STRATNUM","CLUSTER")])
#nrow(takeLook2@data)==nrow(dhsSTRATCLUST)
dhsADMSTRATCLUST <- merge(takeLook2@data,dhsSTRATCLUST,by.x="DHSCLUST",by.y="CLUSTER")
mapLinks <- unique(dhsADMSTRATCLUST[,c("STRATNUM","ADM1FIPSNA","ADM1FIPS","ADM1DHS","ADM1NAME","DHSREGNA","URBAN_RURA")])
if(!nrow(mapLinks)==nrow(dsStratSS2)) stop("strata count and strata region datasets of different row sizes")
#dsStratSS2[dsStratSS2$STRATNUM %in% 1:10,]
#mapLinks[mapLinks$STRATNUM %in% 1:10,]
dsStratSS3 <- merge(dsStratSS2,mapLinks,by.x="STRATNUM",by.y="STRATNUM")
dsStratSS3$URBAN <- (dsStratSS3$URBAN_RURA=="U")*1

dcluswr <- svydesign(id=~CLUSTER, strata=~STRATNUM, weights=~SAMPWGT2, data=countryData)#,nest=T) 
summary(dcluswr)

ptime <- system.time({
			storeCOMILD <- svyby(make.formula(cePredVars[1]), by=~STRATNUM, design=subset(dcluswr,INTYES==1),svymean,verbose=T,na.rm=T)
		})
ptime/60
#warnings()

if(!nrow(storeCOMILD)==nrow(dsStratSS3)) stop("strata count and strata mean datasets of different row sizes")
storeCOMILD2 <- merge(storeCOMILD,dsStratSS3,by.x="STRATNUM",by.y="STRATNUM")
storeCOMILD2$varMean <- storeCOMILD2[,paste("se.",cePredVars[1],sep="")]^2
storeCOMILD2$varSRI <- (storeCOMILD2[,cePredVars[1]]*(1-storeCOMILD2[,cePredVars[1]])/storeCOMILD2[,"nSize"])
storeCOMILD2$deff <- storeCOMILD2$varMean/storeCOMILD2$varSRI

#create a smoothed estimate of the design standard errors (refer to You and Zhou, 2011)
meanDeff <- mean(storeCOMILD2$deff) #estimate the mean design effect for all strata 
storeCOMILD2$smoothVar <- storeCOMILD2$varSRI*meanDeff
plot(storeCOMILD2$STRATNUM,storeCOMILD2$smoothVar,type="l",col="green")
lines(storeCOMILD2$STRATNUM,storeCOMILD2$varMean,lty=3,col="brown")

#bring in spDep object and extract necessary info;
#directory <- "C:\\Users\\dbard\\workspace\\Ediss"
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

natearFIPS <- takeLook3@data[order(takeLook3@data$ADM1FIPS),c("NAME_1","ADM1FIPS")]
nrow(dhsFIPS)==nrow(natearFIPS)
cbind(dhsFIPS,natearFIPS)

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
windows()
plot(takeLook3)#, col="gray70")
title(main="Columbia")
jitterCoord <- coordinates(takeLook3)
labelD1 <- takeLook3@data$NAME_1
text(jitterCoord, labels=labelD1, cex=.8)

spNB <- poly2nb(takeLook3) #transform sn object to neighbors list object
#spNB <- poly2nb(takeLook3[takeLook3@data$stratID!=7,])
summary(spNB)

#add cleaner plotting names to takeLook3
takeLook4 <- takeLook3
cleanNames <- unique(addAll[,c("ADM1FIPSNA","stratID")])
takeLook4@data <- merge(takeLook3@data,cleanNames,sort=F)
#takeLook5 <- takeLook3
#takeLook5@data <- merge(takeLook3@data,cleanNames)#,sort=F)
#all.equal(takeLook4@data,takeLook5@data)
takeLook4@data[,c("ADM1FIPSNA","NAME_1")]

##########################

figNames <- c("Mild to Severe Physical Non-Sexual IPV","Moderate to Severe Physical Non-Sexual IPV","Sexual IPV",
		"Mild to Severe Physical or Sexual IPV","Moderate to Severe Physical or Sexual IPV")

outcomeNumber <- 1

regOutcome <- cePredVars[outcomeNumber]
addAll <- createMergedDataPE(ds=countryData,regOutcome = regOutcome,dcluswr=dcluswr,linkVar="ADM1FIPS")

modelData <- subset(addAll,select= c("STRATNUM","stratID",regOutcome,"smoothVar","URBAN"))
modelData$stratID2 <- modelData$stratID
modelData$STRATNUM2 <- modelData$STRATNUM

modComps <- modCompWB(spNB = spNB, modelData2 = modelData, regOutcome = regOutcome, yesNo=T,modNums=c(6))
storeRegOutnew <- modComps$storeRegOut

write.table(storeRegOutnew,"clipboard",sep="\t",row.names=F,col.names=F,na="---")

allDICs <- unlist(mapply("[",modComps$allBYMlist,"DIC"))
print(paste("Best Model Number",which(allDICs==min(allDICs))))
bestMod <- modComps$allBYMlist[[which(allDICs==min(allDICs))]]

keepvar <- rownames(bestMod$summary)[grepl("muy",rownames(bestMod$summary))]
bcoda <- as.mcmc.list(bestMod)[,keepvar,drop=F]
sumCoda <- summary(bcoda)
wbEsts <- unname(sumCoda$statistics[,1])
#graphics.off()
#windows(record=TRUE)
#.SavedPlots <- NULL
#plot(bcoda2)

cbind(modelData[,regOutcome],wbEsts)
max(abs(wbEsts-modelData[,regOutcome]))
#max(abs(inlaResult6$summary.lincomb.derived$mean-modelData2[,regOutcome]))

sum(bestMod$sims.list$muy[,1]-bestMod$sims.list$inter>0)
ppvChk <- apply(bestMod$sims.list$muy,2,function(x) sum((x-(bestMod$sims.list$inter+bestMod$sims.list$beta))>0)/5000)
sum(ppvChk>.8)
highSTRAT <- modelData$STRATNUM[which(ppvChk>.8)]
addAll[highSTRAT,c("ADM1FIPSNA","stratID","URBAN")]

modPreds <- data.frame(list(STRATNUM=modelData$STRATNUM,wbEsts=wbEsts,ppvChk=ppvChk))
addAll2 <- merge(addAll,modPreds,by.x="STRATNUM",by.y="STRATNUM",sort=F)

linkVar <- "ADM1FIPS"

probChild <- addAll2[addAll2$URBAN==1,c(linkVar)][!addAll2[addAll2$URBAN==1,c(linkVar)] %in% addAll2[addAll2$URBAN==0,c(linkVar)]]
#fakeStrat <- data.frame(list("Callao",0))#,NA,NA,NA))
#colnames(fakeStrat) <- c(linkVar,"URBAN")#,"wbEsts","ppvChk",regOutcome)
#addAll2 <- merge(addAll2,fakeStrat,by.x=c(linkVar,"URBAN"),by.y=c(linkVar,"URBAN"),all.x=T,all.y=T)

bestModMapUrban <- takeLook4 
bestModMapRural <- takeLook4 [!takeLook4@data[,linkVar] %in% probChild,linkVar] #get rid of areas without rural stratum
nrow(bestModMapUrban)
nrow(bestModMapRural)
bestModMapUrban@data <- merge(bestModMapUrban@data,addAll2[addAll2$URBAN==1,c(linkVar,"wbEsts","ppvChk",regOutcome)],
		by.x=linkVar,by.y=linkVar,sort = FALSE )
bestModMapRural@data <- merge(bestModMapRural@data,addAll2[addAll2$URBAN==0,c(linkVar,"wbEsts","ppvChk",regOutcome)],
		by.x=linkVar,by.y=linkVar,sort = FALSE )
if(!all.equal(takeLook4@data[,linkVar],paste(bestModMapUrban@data[,linkVar]))) stop("order of urban df & sp objects mismatch")
if(!all.equal(takeLook4@data[!takeLook4@data[,linkVar] %in% probChild,linkVar],
		paste(bestModMapRural@data[,linkVar]))) stop("order of rural df & sp objects mismatch")
#if(!all.equal(takeLook4@data$ADM1FIPS[takeLook4@data$ADM1FIPS!="CO34"],paste(bestModMapRural@data$ADM1FIPS))) stop("order of rural df & sp objects mismatch")

windows()
bestModMapUrban2b <- my.latlong2grid(bestModMapUrban)
coordinates(bestModMapUrban2b) 
my.mapvariable(y=bestModMapUrban@data$wbEsts,spatial.polygon=bestModMapUrban2b,ncut=1000,nlevels=10,#lower=.25,upper=.5,
		h=240, c. = c(80, 0), l = c(10, 90), power=2,
		main=paste("Prevalence of\n",figNames[outcomeNumber],"\nin Urban Areas of ",country[countryNumber],sep=""),
		xlab="Eastings (km)",ylab="Northings (km)")
windows()
bestModMapRural2b <- my.latlong2grid(bestModMapRural)
coordinates(bestModMapRural2b) 
my.mapvariable(y=bestModMapRural@data$wbEsts,spatial.polygon=bestModMapRural2b,ncut=1000,nlevels=10,#lower=.25,upper=.5,
		h=240, c. = c(80, 0), l = c(10, 90), power=2,
		main=paste("Prevalence of\n",figNames[outcomeNumber],"\nin Rural Areas of ",country[countryNumber],sep=""),
		xlab="Eastings (km)",ylab="Northings (km)")
#mapvariable()

outcomeNumber <- outcomeNumber
regOutcome <- cePredVars[outcomeNumber]
linkVar2 <- "stratID"

countryData2 <- merge(countryData,addAll[,c("STRATNUM",linkVar2)],by.x="STRATNUM",by.y="STRATNUM",sort=F)

dcluswr2 <- svydesign(id=~CLUSTER, strata=~STRATNUM, weights=~SAMPWGT2, data=countryData2)#,nest=T) 
summary(dcluswr2)

#colnames(countryData2)[colnames(countryData2) %in% regOutcome]

ptime <- system.time({
			moranIdat <- svyby(make.formula(regOutcome), by=make.formula(linkVar2), design=subset(dcluswr2,INTYES==1),svymean,verbose=T,na.rm=T)
		})
ptime/60
#warnings()

moranIdat2 <- takeLook4 
moranIdat2@data <- merge(takeLook4@data,moranIdat,by.x=linkVar2,by.y=linkVar2,sort=F)

MIoutputTabRegion <- calcMoranIOthersRegion(regOutcome = regOutcome,takeLookUrban=moranIdat2,ordNum=2)
write.table(MIoutputTabRegion,"clipboard",sep="\t",row.names=F,col.names=F,na="---")


MIoutputTab <- calcMoranIOthers(regOutcome = regOutcome,takeLookUrban=bestModMapUrban,
		takeLookRural=bestModMapRural,ordNum=2,probChild=probChild)
write.table(MIoutputTab,"clipboard",sep="\t",row.names=F,col.names=F,na="---")
probChild <- NA

#quikChk <- storeCOMILD2[storeCOMILD2$URBAN==0,c("ADM1FIPSNA",regOutcome)]
#cbind(bestModMapRural@data[,c("ADM1FIPSNA","wbEsts",regOutcome)],quikChk[order(quikChk$ADM1FIPSNA),])
#windows()
#plot(bestModMapRural)#, col="gray70")
#title(main="Columbia")
#jitterCoord <- coordinates(bestModMapRural)
#labelD1 <- bestModMapRural@data$ADM1FIPSNA
#text(jitterCoord, labels=labelD1, cex=.8)

#is.na(bestModMapRural@data$wbEsts[bestModMapRural@data$ADM1FIPS=="C034"])
bestModMapRural@data$wbEsts[bestModMapRural@data$ADM1FIPSNA=="Cauca"]

