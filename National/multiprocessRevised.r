 
library(foreign)
library(car)
library(R2WinBUGS)
library(lme4)

library(maps)
library(maptools)
library(spdep)
library(RColorBrewer)
library(classInt)

chfatal = read.spss("P:/SPSS/Nelson/fatality_agg3_wagegrps_new.sav",to.data.frame =T,use.missings=F,use.value.labels=F)
colnames(chfatal)


setwd("P:/SPSS/Nelson/")

createAggData <- function(yearsNeeded){
chfatalnew <- chfatal[chfatal$yrindex %in% c(yearsNeeded),]

datsets <- list(chfatalnew) #list(chfatal9194,chfatal9598,chfatal9902,chfatal0306)
ndatsets <- list(NA)
for(i in 1:length(datsets)){
  countnum <- aggregate(datsets[[i]]$countnum,by=list(countnum = datsets[[i]]$countnum),min)[,2]
  n_deaths <- aggregate(datsets[[i]]$n_deaths,by=list(countnum = datsets[[i]]$countnum),sum)[,2]
  chpop <- aggregate(datsets[[i]]$chpop,by=list(countnum = datsets[[i]]$countnum),sum)[,2] #person years
  popd <- sum(n_deaths)/sum(chpop)
  expd <- chpop*popd
  smr <- n_deaths/expd
  ndatsets[[i]] <- as.data.frame(cbind(countnum,n_deaths,chpop,popd,expd,smr))
}

return(ndatsets[[1]])
}


collect2yr <- createAggData(c(1:16))
colnames(collect2yr) <- paste(colnames(collect2yr),'ALL',sep='')
collect2yr$countnumALL


for(i in 1:8){
  yearPairs <- matrix(1:16,8,2,byrow=T)
  newCollect <- createAggData(yearPairs[i,])
  colnames(newCollect) <- paste(colnames(newCollect),i,sep='')
  #colnames(newCollect) <- paste(colnames(newCollect),yearPairs[i,1],'_',yearPairs[i,2],sep='')
  collect2yr <- as.data.frame(cbind(collect2yr,newCollect))
  collect2yr[,paste('smrA',i,sep='')] <- newCollect$n_deaths/(collect2yr$popdALL*newCollect$chpop)
  #collect2yr[,paste('smrA',yearPairs[i,1],'_',yearPairs[i,2],sep='')] <- newCollect$n_deaths/(collect2yr$popdALL*newCollect$chpop)
  #colnames(collect2yr[ncol(collect2yr)]) <- paste('smrA',yearPairs[i,1],'_',yearPairs[i,2],sep='')
}

source("P:\\WinBugs\\Mapping\\getPoverty.r")
setwd("P:/SPSS/Nelson/")
collect2yr <- merge(collect2yr,storePov,by.x="countnumALL",by.y="countnumPov") 

  yearPairs2 <- matrix(1991:2006,8,2,byrow=T)
  yearPairs3 <- paste(yearPairs2[,1],'-',yearPairs2[,2],sep='')

  rm(ok_poly_sp)
  ok_poly <- map("county", "oklahoma", fill=TRUE, col="transparent",plot=FALSE)
  IDnames <- sapply(strsplit(ok_poly$names, ","), function(x) x[2])
  length(IDnames)
  IDs <- c(1:77)
  ok_poly_sp <- map2SpatialPolygons(ok_poly, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84")) #had to change the datum all of the sudden to capital WGS84
  rownames(collect2yr) <- collect2yr$countnumALL
  ok_poly_sp1 <- SpatialPolygonsDataFrame(ok_poly_sp,data=collect2yr)
  adjmatOK1 <- poly2nb(ok_poly_sp1)
  
  useSMRAs <- colnames(collect2yr)[grep("(smrA)(\\d)",colnames(collect2yr))]

  useNDs <- colnames(collect2yr)[grep("(n_deaths)(\\d)",colnames(collect2yr))]    #Number of deaths variables
  useCPs <- colnames(collect2yr)[grep("(chpop)(\\d)",colnames(collect2yr))]    #Number of person years variables
  useCOVs <- colnames(collect2yr)[grep("(Poverty)(\\d)",colnames(collect2yr))]    #Number of person years variables
  N <- nrow(collect2yr)
  T <- 8

  adjmatOK1 <- poly2nb(ok_poly_sp1)
  #adjmatOK2 <- source("adjmatOK2.txt") #adjacency matrix produced within WinBUGS
  adjmatOK <- nb2WB(adjmatOK1) #adjacency matrix produced within spdep (indentical to the WB version)
  num <- adjmatOK$num
  adj <- adjmatOK$adj
  weights <- adjmatOK$w
  
  T <- 8
  weights.t <- NA
  num.t <- NA
  adj.t <- NA
  for(t in 1:1) {
   weights.t[t] <- 1;
   adj.t[t] <- t+1;
   num.t[t] <- 1
  }
  for(t in 2:(T-1)) {
    weights.t[2+(t-2)*2] <- 1;
    adj.t[2+(t-2)*2] <- t-1
    weights.t[3+(t-2)*2] <- 1;
    adj.t[3+(t-2)*2] <- t+1;
    num.t[t] <- 2
  }
  for(t in T:T) {
    weights.t[(T-2)*2 + 2] <- 1;
    adj.t[(T-2)*2 + 2] <- t-1;
    num.t[t] <- 1
  }      

ARBCovlist <- list(NA)
CovCPUtimes <- list(NA)

cm <- as.matrix(collect2yr[,useNDs])
births <- as.matrix(collect2yr[,useCPs])
povper <- as.matrix(collect2yr[,useCOVs])

ARBC.data <- list("N","T","cm","births","povper","num","adj","weights","num.t","adj.t","weights.t")
ARBC.inits <- function(){list(inter=rnorm(n=1),betaP=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,50,100),
                mu=rnorm(n=N),tau.mu=runif(n=1,50,100),xi=rnorm(n=T),tau.xi=runif(n=1,50,100),
                gamma=rnorm(n=T),tau.gamma=runif(n=1,50,100),                
                nu=matrix(rnorm(n=N*T),N,T),sigma.nu=c(runif(n=1,0,1),NA),kappa=runif(n=1,0,1),
                P=c(.95,.05),ind=matrix(rbinom(n=N*T,1,.05)+1,N,T),
                ypred=matrix(rbinom(n=N*T,1,.25),N,T))}
ARBC.parameters <- c("inter","betaP","sigma.lambda","sigma.mu","sigma.xi","sigma.gamma","sigma.nu","kappa","P[]",
                "mape","PPL[,]","lambda[]","mu[]","xi[]","gamma[]","nu[,]","ind[,]",
                "ypred[,]")
ARBC.inits()
start.time1 <- Sys.time()
map.sim <- bugs(data=ARBC.data, inits=ARBC.inits, parameters.to.save=ARBC.parameters,
model.file="C:\\Users\\dbard\\winBUGScode\\Nelson\\ARBCovspacetime3.odc", n.chains=2, n.iter=50000,n.thin=10,
n.burnin=10000, debug=F, bugs.directory="C:\\Program Files\\winbugs14\\WinBUGS14\\", codaPkg=F)
stop.time1 <- Sys.time()
diff.time1 <- stop.time1 - start.time1
print(map.sim, digits=4)
print(diff.time1)

#map.sim$summary[c("kappa","sigma.nu[1]","sigma.nu[2]"),]
 
ARBCovlist[[1]] <- map.sim
CovCPUtimes[[1]] <- diff.time1
save(list=c("ARBCovlist","CovCPUtimes"),file="ARBCov.RData")
#rm(list=c("ARBlist","ACPUtimes"))
#load(file="ARB.RData")
#ls()

#library(coda)
#map.simD <- ARBCovlist[[1]]
#plot(map.simD)
#keepvar <- rownames(map.simD$summary)  #[!grepl("bSMR|D.i",rownames(map.simD$summary))]
#bcoda <- as.mcmc.list(map.simD)[,keepvar,drop=F]
#bcoda2 <- window(bcoda,start=14991,end=19991) #distributions didn't really converge until 15000ish iteration
#summary(bcoda2)
#graphics.off()
#windows(record=TRUE)
#.SavedPlots <- NULL
#plot(bcoda2)   


#Hetrolist <- list(NA)
#HCPUtimes <- list(NA)
#
#for(i in 8){
#cm <- collect2yr[,useNDs[i]]
#births <- collect2yr[,useCPs[i]]  #these are actually person years (using births so ARB code need not be changed)
#
#bym.data <- list("N","cm","births")#,"num","adj","weights")
#bym.inits <- function(){list(inter=rnorm(n=1),lambda=rnorm(n=N),tau.lambda=runif(n=1,1,100),
#                ypred=rbinom(n=N,1,.25))}
#bym.parameters <- c("inter","sigma.lambda","mape","PPL[]","lambda[]","ypred[]")
#
#start.time1 <- Sys.time()
#map.sim <- bugs(data=bym.data, inits=bym.inits, parameters.to.save=bym.parameters,
#model.file="C:\\Users\\dbard\\winBUGScode\\Nelson\\HETROnew.odc", n.chains=2, n.iter=20000,n.thin=10,
#n.burnin=10000, debug=F, bugs.directory="C:\\Program Files\\winbugs14\\WinBUGS14\\", codaPkg=F)
#stop.time1 <- Sys.time()
#diff.time1 <- stop.time1 - start.time1
#print(map.sim, digits=4)
#print(diff.time1)
#
#Hetrolist[[i]] <- map.sim
#HCPUtimes[[i]] <- diff.time1
#save.image(file="HETRO8.RData")
#}

