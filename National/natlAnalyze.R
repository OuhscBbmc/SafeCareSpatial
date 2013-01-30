# TODO: Add comment
# 
# Author: dbard
###############################################################################

#get data for 2007 - 2011
setwd("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National/Data")
realUnqData <- read.table("realUnqData.csv",header=T,sep=",")
realRespDat <- read.table("realRespDat.csv",header=T,sep=",")
realScreenDat <- read.table("realScreenDat.csv",header=T,sep=",")
setwd("C:/Users/dbard/Documents/GitHub/SafeCareSpatial/National")

#convert characters to numeric
numTrim <- function(x){ gsub("[^0-9^.]","",x) }
for(i in 2:ncol(realRespDat)){
	realRespDat[,i] <- as.numeric(numTrim(realRespDat[,i]))
}

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


screen1Year <- realScreenDat[realScreenDat$Year==2007,] 
myBreaks <- seq(0,120,5)
hist((screen1Year$ReferralCount/screen1Year$ChildPopulation)*1000, breaks=myBreaks)

screen1Year$refProp <- screen1Year$ReferralCount/screen1Year$ChildPopulation

screen1Year$varProp <- (screen1Year$refProp*(1-screen1Year$refProp))/screen1Year[,"ChildPopulation"]

#merge with spatial polygon

spNB <- poly2nb(takeLook3) #transform sn object to neighbors list object
#spNB <- poly2nb(takeLook3[takeLook3@data$stratID!=7,])
summary(spNB)

library(R2WinBUGS)
library(coda)
library(survey)





