# TODO: Add comment
# 
# Author: dbard
###############################################################################

source("UtilitiesForCreatingFlow.R")
library(R2WinBUGS)
library(coda)
library(survey)

#get data for 2007 - 2011
setwd("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National/Data")
realUnqData <- read.table("realUnqData.csv",header=T,sep=",")
realRespDat <- read.table("realRespDat.csv",header=T,sep=",")
realScreenDat <- read.table("realScreenDat.csv",header=T,sep=",")
setwd("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National")

range((realScreenDat$ReferralCount/realScreenDat$ChildPopulation)*1000, na.rm=T)

#convert characters to numeric
numTrim <- function(x){ gsub("[^0-9^.]","",x) }
for(i in 2:ncol(realRespDat)){
	realRespDat[,i] <- as.numeric(numTrim(realRespDat[,i]))
}

library(ggplot2)

ggplot(data=realScreenDat[realScreenDat$Year==2011,],aes(x=ScreenOutPercent))+geom_histogram()

realRespDat$numRespVict <- apply(realRespDat[,c("Subst","Indic","Alt")],1,sum,na.rm=T)
realRespDat$rateRespVict <- (realRespDat$numRespVict/realRespDat$totResponse)*100

all.equal(unlist(lapply(statePolygonsForLabels@polygons,function(x) x@ID)),
		sort(as.character(realRespDat$StateName[realRespDat$Year==2011]),decreasing=F))

spPolyDF <- SpatialPolygonsDataFrame(statePolygonsForLabels,realRespDat[realRespDat$Year==2007,],match.ID="StateName")

dropStates <- spPolyDF@data[is.na(spPolyDF@data$rateRespVict),"StateName"]

spPolyDF <- spPolyDF[!spPolyDF@data$StateName %in% dropStates,]

spPolyDFNB <- poly2nb(spPolyDF)

spPolyLW <- nb2listw(spPolyDFNB)

nb3 <- nblag(spPolyDFNB,maxlag=2)
nb3b <- nblag_cumul(nb3)
all.equal(sort(unlist(lapply(nb3,"[",47))),unlist(nb3b[1:47][47]))

moran.test(spPolyDF@data[,"rateRespVict"],nb2listw(nb3b),randomisation=F)

#windows()
hist(spPolyDF@data$rateRespVict)

moranI <- print(sp.correlogram(spPolyDFNB,spPolyDF@data[,"rateRespVict"], order=5, method="I", style="W",zero.policy=T))

look <- sp.correlogram(spPolyDFNB,spPolyDF@data[,"rateRespVict"], order=5, method="I", style="W",zero.policy=T)
look$cardnos

#note that sp.correlogram does something strange: it acts just like time autocorrelation, looking at 
#correlations at greater distances (cycles in time); why would space act like that?  weird
#more helpful, in my mind, would be a look at correlations in space with a wider neighborhood net/definition
#must use the nblag and nblag_cumul to accomplish this
windows()
realScreenDat$refProp <- (realScreenDat$ReferralCount/realScreenDat$ChildPopulation)

realScreenDat$varProp <- (realScreenDat$refProp*(1-realScreenDat$refProp))/realScreenDat[,"ChildPopulation"]

realScreenDat$refRate <- realScreenDat$refProp*1000
realScreenDat$varRate <- realScreenDat$varProp*1000^2

realScreenDat$refRate <- realScreenDat$refProp*1000
realScreenDat$varRate <- realScreenDat$varProp*1000^2

getwd()
write.table(realScreenDat,"realScreen.csv",row.names=F,sep=",")

state <- "florida"
oneStateProp <- realScreenDat$refProp[realScreenDat$StateName==state]
oneStatePop <- realScreenDat$ChildPopulation[realScreenDat$StateName==state]
oneStateRate <- realScreenDat$refRate[realScreenDat$StateName==state]
oneStateRateVar <- realScreenDat$varRate[realScreenDat$StateName==state]

2*(.03*(.97)/660000)
simStateProp <- numeric(0)
for(i in 1:5){
	simStateProp[i] <- rnorm(n=1,mean=mean(oneStateProp),sd=sqrt(mean(oneStateProp[i])*(1-mean(oneStateProp[i]))/oneStatePop[i]))
}
mean(oneStateProp)

print(acf(simStateProp))

screen1Year <- realScreenDat[realScreenDat$Year==2007,] 
myBreaks <- seq(0,120,5)
hist((screen1Year$ReferralCount/screen1Year$ChildPopulation)*1000, breaks=myBreaks)




#merge with spatial polygon
spPolyDF <- SpatialPolygonsDataFrame(statePolygonsForLabels,screen1Year,match.ID="StateName")
#spPolyDF <- spPolyDF[!is.na(spPolyDF@data$refRate),]
		
spNB <- poly2nb(spPolyDF) #transform sn object to neighbors list object
summary(spNB)
modelData2 <- spPolyDF@data
regOutcome <- "ReferralCount"

spSub <- spPolyDF[!is.na(spPolyDF@data[,regOutcome]),]
spNBsub <- poly2nb(spSub)
moranI <- print(sp.correlogram(spNBsub,spSub@data[,regOutcome], order=5, method="I", style="W",zero.policy=T))
moran.test(spSub@data[,regOutcome],nb2listw(spNBsub,zero.policy=T,style="W"), randomisation=F,zero.policy=T)


wbNB <- nb2WB(spNB)
N <- nrow(modelData2)
y <- modelData2[,regOutcome]
num <- wbNB$num
adj <- wbNB$adj
weights <- wbNB$weights
stratID <- 1:length(unique(modelData2$StateName))
#stratIDR <- modelData2$stratID + max(modelData2$stratID)*(1-urban) #this is used when rural and urban autocor differs significantly
J <- length(unique(stratID))
vare <- c(modelData2$varRate)
vare <- ifelse(is.na(vare),median(vare,na.rm=T),vare)
lpop <- log(modelData2$ChildPopulation)
lpop <- ifelse(is.na(lpop),log(median(pop,na.rm=T)),lpop)

#modelData2$StateName[6]

bym.data1 <- list("N","y","lpop")#"vare")
bym.data2 <- list("N","J","y","lpop","stratID")
bym.data3 <- list("N","J","y","num","adj","weights","vare","stratID")
bym.data4 <- list("N","J","y","num","adj","weights","vare","stratID")

bym.inits1 <- function(){list(inter=log(runif(n=1,0,.1)),y=ifelse(is.na(y),rnorm(n=N,mean(y,na.rm=T),.1),NA))}
bym.inits2 <- function(){list(inter=log(runif(n=1,.499,.5)),y=ifelse(is.na(y),rnorm(n=N,mean(y,na.rm=T),.1),NA),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100))}
bym.inits3 <- function(){list(inter=runif(n=1,.499,.5)*1000,y=ifelse(is.na(y),rnorm(n=N,mean(y,na.rm=T),.1),NA),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100))}	
bym.inits4 <- function(){list(inter=runif(n=1,.499,.5)*1000,y=ifelse(is.na(y),rnorm(n=N,mean(y,na.rm=T),.1),NA),lambda=rnorm(n=J),tau.lambda=runif(n=1,1,100),
			mu=rnorm(n=J),tau.mu=runif(n=1,1,100))} 

bym.parameters1 <- c("inter","muy[]") #Common prev
bym.parameters2 <- c("inter","sigma.lambda","lambda[]","muy[]") #State Het
bym.parameters3 <- c("inter","sigma.lambda","lambda[]","muy[]") #State Autocor
bym.parameters4 <- c("inter","sigma.lambda","lambda[]","muy[]","mu[]","sigma.mu") #State Het & State Autocor


#resultList <- list()
#resultListb <- list()

for(i in 2){
	start.time1 <- Sys.time()
	map.sim <- bugs(data=get(paste("bym.data",i,sep="")), inits=get(paste("bym.inits",i,sep="")), 
			parameters.to.save=get(paste("bym.parameters",i,sep="")), 
			model.file=paste("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National/cpsBYM",i,".odc",sep=""), n.chains=3, 
			n.iter=1000,n.thin=10,n.burnin=500, debug=T, 
			bugs.directory="C:/Program Files/winbugs14/WinBUGS14/", codaPkg=F)
	stop.time1 <- Sys.time()
	diff.time1 <- stop.time1 - start.time1
	#print(map.sim, digits=4)
	print(diff.time1)	
	
	BYMlist <- map.sim
	BYMlist$summary["inter","mean"]
	
	keepvar <- rownames(BYMlist$summary)[grepl("inter|beta|sigma.",rownames(BYMlist$summary))]
	bcoda <- as.mcmc.list(BYMlist)[,keepvar,drop=F]
	print(summary(bcoda))
	
	myDIC <- paste(sprintf("%.0f", BYMlist$DIC),"(p.eff=",sprintf("%.0f", BYMlist$summary["deviance","n.eff"]),")",sep="")

	resultListb[[i]] <- BYMlist
}

write.table(modelData2,"forSas.csv",row.names=F,sep=",",na=".")

names(resultListb) <- c("interceptOnly","interceptPlusHetero","interceptPlusSpatial","interceptPlusHeteroPlusSpatial")
resultList[[3]]$DIC

summary(bcoda)


