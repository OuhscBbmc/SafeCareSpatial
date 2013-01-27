# TODO: Add comment
# 
# Author: dbard
###############################################################################

#simulate data for Columbia

#Run the first half of code inside mappingCO.R and create everything up to the takeLook4 object 

#Now go ahead and bring in FIPS Code now so that you can later merge mapping info
windows()
directory <- "P:\\SPSS\\Elizabeth\\Dissertation\\Columbia" ##NOTE TO SELF: readOGR IS FINICKY AND DOESN'T LIKE THE DIRECTORY TO END IN "\\"
fileName <- "COL_adm1"
regCO <- readOGR(dsn=file.path(directory,"COL_adm1"), layer=fileName)
plot(regCO)#, col="gray70")
title(main="Columbia")
jitterCoord <- coordinates(regCO)
labelD1 <- regCO@data$NAME_1
text(jitterCoord, labels=labelD1, cex=.8)
fileName <- "COGE61FL_revised"
clustCO <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)

clustCO3 <- clustCO[clustCO@data$ADM1FIPSNA=="Bogota Capital District",]
regCol <- c("black","red")
jitterCoord2 <- coordinates(clustCO3)
regType <- as.numeric(clustCO3@data$URBAN_RURA)
points(jitterCoord2[,1],jitterCoord2[,2],col=regCol[regType]) #latitude has to come first, then longitude

windows()
fileName <- "COGE61FL_revisedTP"
clustCO4 <- readOGR(dsn=file.path(directory,"COGE61FL_revised"), layer=fileName)
proj4string(clustCO4) <- proj4string(regCO)
plot(clustCO4,lwd=.5,lty=2,col="gray90") #latitude has to come first, then longitude
#colnames(clustCO4@data)
#length(unique(clustCO4@data$DHSCLUST))
#nrow(clustCO4@data)
#length(unique(COIR60FLdat$CLUSTER))
#COIR60FLdat[COIR60FLdat$CLUSTER %in% clustCO4@data$DHSCLUST,c("stratID")]
#COIR60FLdat$stratID
CO4plot <- merge(COIR60FLdat[,c("STRATNUM","RESTYPE","CLUSTER")],addAll[,c("STRATNUM","RESTYPE","stratID")],
		by.x=c("STRATNUM","RESTYPE"),by.y=c("STRATNUM","RESTYPE"),sort=F)
CO4plot <- CO4plot[!duplicated(CO4plot$CLUSTER),]
clustCO4@data$rowOrder <- 1:nrow(clustCO4@data)
clustCO4dat <- merge(clustCO4@data,CO4plot,by.x="DHSCLUST",by.y="CLUSTER",all.x=T,sort=F)
clustCO4dat <- clustCO4dat[order(clustCO4dat$rowOrder),]
probClusts <- clustCO4dat$DHSCLUST[is.na(clustCO4dat$stratID)]
#clean up these clusters where no women were sampled; let's assign them to the closest corresponding cluster
#probClusts 1901 2574 2282 1903 3048  133 1873 1926 4888 4981 4966 4652 1879 1875 3049  258  371 3054
fixClusts <- c(1902,2575,2283,1902,3050,134,1874,1927,4889,4982,4967,4653,1880,1876,3050,257,372,3055)
length(probClusts)==length(fixClusts)
clustCO4@data$DHSCLUST[clustCO4@data$DHSCLUST %in% probClusts] <- fixClusts
#try merging again and reassess problem clusters
clustCO4dat <- merge(clustCO4@data,CO4plot,by.x="DHSCLUST",by.y="CLUSTER",sort=F)
clustCO4dat <- clustCO4dat[order(clustCO4dat$rowOrder),]
probClusts <- clustCO4dat$DHSCLUST[is.na(clustCO4dat$stratID)]
probClusts
#all.equal(order(clustCO4@data$DHSCLUST),order(clustCO4dat$DHSCLUST))
#clustCO4@data$DHSCLUST[clustCO4@data$DHSCLUST != clustCO4dat$DHSCLUST]
clustCO4dat$DHSCLUST2 <- clustCO4dat$DHSCLUST
clustCO4@data[,c("DHSCLUST2","STRATNUM","RESTYPE","stratID")] <- 
		clustCO4dat[,c("DHSCLUST2","STRATNUM","RESTYPE","stratID")]
all.equal(clustCO4@data$DHSCLUST,clustCO4@data$DHSCLUST2)

#rainbow(length(unique(clustCO4@data$stratID)))
stratCols <- rainbow(length(unique(clustCO4@data$STRATNUM)[!is.na(unique(clustCO4@data$STRATNUM))]))
plot(clustCO4,lwd=.5,lty=0,col=stratCols[clustCO4@data$STRATNUM])#"gray90")
#jitterCoord <- coordinates(clustCO4)
#labelD1 <- clustCO4@data$STRATNUM
#text(jitterCoord, labels=labelD1, cex=.5)

#x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#colorado = readWKT("POLYGON ((-81.841530  -4.228429, -81.841530 15.91247, -66.87033 15.91247, -66.87033  -4.228429, -81.841530 -4.228429))")
colorado = readWKT("POLYGON ((-81.941530  -4.9228429, -81.941530 15.91247, -66.887033 15.91247, -66.887033  -4.9228429, 
				-81.941530 -4.9228429))")
proj4string(colorado) <- proj4string(regCO)
#plot(colorado)
difference <- gDifference(colorado, regCO)
slot(slot(slot(difference, "polygons")[[1]], "Polygons")[[1]], "hole") <- F
plot(difference, col="gray", add=T , pbg='transparent')
plot(regCO,add=T, pbg='transparent')

chkRegs <- unique(clustCO4@data[,c("STRATNUM","RESTYPE","stratID","ADM1FIPSNA")])
chkRegs[order(chkRegs$STRATNUM),]

sum(is.na(clustCO4@data$STRATNUM))

#okay now simulate data for all six models
plotVars <- c("yCP","yRH","ySRH","yRC","yRHRC","ySRHRC")
plotVars2 <- paste(plotVars,"pure",sep="")

simMaps <- function(addAll, spNB, T, sum, clustCO4, F, ncut, h, c., l, power, regCO,popMu=0.37,hetSD=0.12,hetSD2=0.06,
		autoSD=.09,plotVars=plotVars) {

	effectiveSS <- addAll$nSize/mean(addAll$deff) #use smooth estimate of effective sample size to calculate SE's
	seBase <- rnorm(n=nrow(addAll),0,1)
	
#Common Prevalence model
	for(i in 1:nrow(addAll)){
		addAll$ypredCP[i] <- popMu 
		addAll$seYPredCP[i] <- sqrt(addAll$ypredCP[i]*(1-addAll$ypredCP[i])/effectiveSS[i])
		addAll$yCP[i] <- addAll$ypredCP[i] + seBase[i]*addAll$seYPredCP[i] #rnorm(n=1,0,addAll$seYPredCP[i])
		addAll$yCPpure[i] <- addAll$ypredCP[i] 
	}
	
	
#simulate data for the Reg Het model
	regHet <- rnorm(n=length(unique(addAll$stratID)),0,hetSD)
	for(i in 1:nrow(addAll)){
		addAll$ypredRH[i] <- popMu + regHet[addAll$stratID[i]]
		addAll$seYPredRH[i] <- sqrt(addAll$ypredRH[i]*(1-addAll$ypredRH[i])/effectiveSS[i])
		addAll$yRH[i] <- addAll$ypredRH[i] + seBase[i]*addAll$seYPredRH[i] #rnorm(n=1,0,addAll$seYPredRH[i])	
		addAll$yRHpure[i] <- addAll$ypredRH[i] 
	}
	
#simulate data for the Sub-Reg Het model
	subregHet <- rnorm(n=length(unique(addAll$STRATNUM)),0,hetSD)
	for(i in 1:nrow(addAll)){
		addAll$ypredSRH[i] <- popMu + subregHet[addAll$STRATNUM[i]]
		addAll$seYPredSRH[i] <- sqrt(addAll$ypredSRH[i]*(1-addAll$ypredSRH[i])/effectiveSS[i])
		addAll$ySRH[i] <- addAll$ypredSRH[i] + seBase[i]*addAll$seYPredSRH[i] #rnorm(n=1,0,addAll$seYPredSRH[i])
		addAll$ySRHpure[i] <- addAll$ypredSRH[i] 
	}
	
#simulate data for the Reg AutoCor model
	set.seed(runif(1))	
#set.seed()	
	n<-length(unique(addAll$stratID))			# Number of blocks
	nis<-rep(1,n) 		# Number of individuals per block; here it's balanced  
	id<-rep(1:n,nis)		
	N<-length(id) 		# Total number of subjects
	
# Generate symmetric adjaceny matrix, A  
	wbNB <- nb2WB(spNB) #the spNB nb obj comes from mappingCO.R file
	A <- nb2mat(spNB,style="B",zero.policy=T)
#attributes(A)
	attr(A,"dimnames")[[1]] <- NULL
	isSymmetric(A)
	mi<-apply(A,1,sum)	# No. neighbors
	
# Spatial effects, phi
	rho<-1			# Spatial dependence parameter = 1 for intrinsic CAR
	s2phi<-autoSD^2		# Spatial Variance
	sdphi<-sqrt(s2phi)	# Spatial SD
	Q<-diag(mi)-rho*A + diag(.0001,n)	# NOTE 1: Add small constant to make Q non-singular
	Q <- Q[-7,-7] #as per WinBUGS, set CAR random effect = 0 for islands (stratID=7)
# NOTE 2: Independent random effects ==> m=1 for all i and rho=0
	covphi<-sdphi^2*solve(Q) # Covariance of phis
	phi <-c(rmvnorm(1,sigma=covphi)) # Spatial Random Effects
	diag(covphi)
	regCorNo7 <- phi-mean(phi)
	regCor <- c(regCorNo7[1:6],0,regCorNo7[7:length(regCorNo7)]) #as per WinBUGS, set CAR random effect = 0 for islands (stratID=7)
#regCor[spNB[[2]]]
	
	for(i in 1:nrow(addAll)){
		addAll$ypredRC[i] <- popMu + regCor[addAll$stratID[i]]
		addAll$seYPredRC[i] <- sqrt(addAll$ypredRC[i]*(1-addAll$ypredRC[i])/effectiveSS[i])
		addAll$yRC[i] <- addAll$ypredRC[i] + seBase[i]*addAll$seYPredRC[i] #rnorm(n=1,0,addAll$seYPredRC[i])
		addAll$yRCpure[i] <- addAll$ypredRC[i] 
	}
	
#simulate data for the Reg Het + Reg AutoCor model
	regHet2 <- rnorm(n=length(unique(addAll$stratID)),0,hetSD2)
	for(i in 1:nrow(addAll)){
		addAll$ypredRHRC[i] <- popMu + regHet2[addAll$stratID[i]] + regCor[addAll$stratID[i]]
		addAll$seYPredRHRC[i] <- sqrt(addAll$ypredRHRC[i]*(1-addAll$ypredRHRC[i])/effectiveSS[i])
		addAll$yRHRC[i] <- addAll$ypredRHRC[i] + seBase[i]*addAll$seYPredRHRC[i] #rnorm(n=1,0,addAll$seYPredRHRC[i])	
		addAll$yRHRCpure[i] <- addAll$ypredRHRC[i] 
	}
	
#simulate data for the Sub-Reg Het + Reg AutoCor model
	subregHet2 <- rnorm(n=length(unique(addAll$STRATNUM)),0,hetSD2)
	for(i in 1:nrow(addAll)){
		addAll$ypredSRHRC[i] <- popMu + subregHet2[addAll$STRATNUM[i]] + regCor[addAll$stratID[i]]
		addAll$seYPredSRHRC[i] <- sqrt(addAll$ypredSRHRC[i]*(1-addAll$ypredSRHRC[i])/effectiveSS[i])
		addAll$ySRHRC[i] <- addAll$ypredSRHRC[i] + seBase[i]*addAll$seYPredSRHRC[i] #rnorm(n=1,0,addAll$seYPredSRHRC[i])	
		addAll$ySRHRCpure[i] <- addAll$ypredSRHRC[i] 	
	}
	
	
	
	
#Plot the simulated data
#length(unique(addAll$STRATNUM))
	clustCO5 <- clustCO4
	simPlot <- merge(clustCO5@data,addAll[,c("STRATNUM",plotVars)],by.x=c("STRATNUM"),by.y=c("STRATNUM"),sort=F)
	simPlot <- simPlot[order(simPlot$rowOrder),]
	simPlot$DHSCLUST3 <- simPlot$DHSCLUST
	clustCO5@data[,c("DHSCLUST3",plotVars)] <- simPlot[,c("DHSCLUST3",plotVars)]
	all.equal(clustCO5@data$DHSCLUST,clustCO5@data$DHSCLUST3)
	
	layout(matrix(c(1:3,7,4:6,7),2,4,byrow=T),width=c(rep(.4,3),.1))
#layout.show(n = 7)
	
	modNames <- c("Common Prevalence","Region Heterogeneity","Sub-Region Heterogeneity","Region Autocorrelation",
			"Reg Het + Reg Autocor","Sub-Reg Het + Reg Autocor")
	
	def.par <- par(no.readonly = TRUE)
	paletteResource <- rev(sequential_hcl(n=ncut + 1, h=h, c.=c., l = l, power=power))
	palette(paletteResource)
	lower <- min(clustCO5@data[,plotVars])
	upper <- max(clustCO5@data[,plotVars])
	nlevels=10; ncut<-1000; h<-240; c.<-c(80, 0); l<-c(10, 90); power<-1
	for(i in 1:6){
		y<-clustCO5@data[,plotVars[i]]; 
		
		id <- cut(y, breaks = seq(from = lower, to = upper, length = (ncut + 1)))
		id <- as.numeric(id)
		id[is.na(id)] <- 0
		id <- id + 1
		
		plot(clustCO4,lwd=.5,lty=0,col=id)#"gray90")
		colorado = readWKT("POLYGON ((-81.941530  -4.9228429, -81.941530 15.91247, -66.887033 15.91247, -66.887033  -4.9228429, 
						-81.941530 -4.9228429))")
		proj4string(colorado) <- proj4string(regCO)
		difference <- gDifference(colorado, regCO)
		slot(slot(slot(difference, "polygons")[[1]], "Polygons")[[1]], "hole") <- F
		plot(difference, col="white", add=T , pbg='transparent')
		#plot(regCO,add=T, pbg='transparent')
		title(main=modNames[i])
	}
	
	plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
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
	rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt, col = levelCols,	border = TRUE)
	
	palette("default")
	par(def.par)
}

simMaps(addAll = addAll, spNB = spNB, T = T, sum = sum, clustCO4 = clustCO4, F = F, ncut = ncut, h = h, c. = c., l = l, 
		power = power, regCO = regCO,popMu=0.37,hetSD=0.12,hetSD2=0.10,autoSD=.10,plotVars=plotVars)

#Got the following block of code from http://www.duke.edu/~neelo003/r/

# CAR02.r
# Gibbs sampler for intrinsic CAR (ICAR) model
# Nested Data (150 blocks, 25 subjects per group)
# Generates Data from "pseudo-intrinsic" CAR by either
#   setting spatial smoothing parameter rho=.9999 or add constant to Q matrix
# Sept 8, 2010
##################################
###################
# Load Packages   #
# and Functions	#
###################
library(mvtnorm)
library(lme4)  	# To compare with nonspatial random effects model

##########################
#   SIMULATE DATA UNDER	 #
#   NON-INTRINSIC CAR  	 #
#   WITH RHO ~ 1	 	 #
##########################
set.seed(090810)		
n<-150			# Number of blocks
nis<-rep(25,n) 		# Number of individuals per block; here it's balanced  
id<-rep(1:n,nis)		
N<-length(id) 		# Total number of subjects

# Generate symmetric adjaceny matrix, A  
A<-matrix(0,n,n)
A[upper.tri(A,diag=F)]<-rbinom(n*(n-1)/2,1,.05)
A<-A+t(A) 
mi<-apply(A,1,sum)	# No. neighbors

# Spatial effects, phi
rho<-1			# Spatial dependence parameter = 1 for intrinsic CAR
s2phi<-625		# Spatial Variance
sdphi<-sqrt(s2phi)	# Spatial SD
Q<-diag(mi)-rho*A + diag(.0001,n)	# NOTE 1: Add small constant to make Q non-singular
# NOTE 2: Independent random effects ==> m=1 for all i and rho=0
covphi<-sdphi^2*solve(Q) # Covariance of phis
phi<-c(rmvnorm(1,sigma=covphi)) # Spatial Random Effects
beta<-c(100,10)    	   # Fixed Effects
x<-rnorm(N,5,2)
X<-cbind(rep(1,N),x)
p<-ncol(X)
se<-20			   # Error SD (i.e., SD of Y|phi)
y<-X%*%beta+rep(phi-mean(phi),times=nis)+rnorm(N,0,se)
fit<-lmer(y~X-1+(1|id))  # Non-spatial random effect fit

###############
# Priors      #
###############
m0<-beta				# Prior Mean for beta
prec0<-diag(.001,p)		# Prior Precision Matrix of beta (vague), independent
a<-d<-b<-g<-.001			# Gamma hyperparms for updating taue and tauphi

#########
# Inits #
#########
tauphi<-1
taue<-1
beta<-rep(0,p)	
phi<-rep(0,n)		
Z<-matrix(0,N,n)			# Random effect "design matrix" used in updating phi
for (i in 1:n) Z[id==i,i]<-1

#################
# Store Results #
#################
nsim<-2000
Beta<-matrix(0,nsim,p)			# Fixed Effects Coeffs
S2e<-S2phi<-rep(0,nsim)		# Variance Parms
Phis<-matrix(0,nsim,n)
###################
# GIBBS SAMPLER	#
###################
tmp<-proc.time()
for (i in 1:nsim) {
	
	# Update Beta 
	Phic<-rep(phi-mean(phi),nis)		# Center to reduce autocorrelation with intercept
	vbeta<-solve(prec0+taue*crossprod(X,X))
	mbeta<-vbeta%*%(prec0%*%m0 + taue*crossprod(X,y-Phic))
	Beta[i,]<-beta<-c(rmvnorm(1,mbeta,vbeta))
	
	# Update phi from joint posterior (can be slow for large n)
	v<-solve(taue*diag(nis)+tauphi*Q)
	m<-v%*%(taue*crossprod(Z,y-X%*%beta))
	phi<-c(rmvnorm(1,m,v))
	
	# Udpate tauphi
	tauphi<-rgamma(1,a+(n-1)/2,b+t(phi)%*%Q%*%phi/2)  # n-1 for number of islands
	S2phi[i]<-1/tauphi
	
	# Udpate taue
	Phi<-rep(phi,nis)
	taue<-rgamma(1,d+N/2,g+crossprod(y-X%*%beta-Phi)/2)
	S2e[i]<-1/taue
	
	if (i%%100==0) print(i)
} 
proc.time()-tmp

###########
# Results #
###########
mbeta<-apply(Beta[1001:nsim,],2,mean)
sdphi<-mean(sqrt(S2phi[1001:nsim]))
sde<-mean(sqrt(S2e[1001:nsim]))

# Indpendent Random effects fit
cat("Indpendent Random Effects Fit","\n")
summary(fit)

cat("Posterior Estimates","\n","beta = ", mbeta,"\n","SDphi = ",sdphi,"\n","SDe = ",sde)   

par(mfrow=c(2,1))

plot(1001:nsim,Beta[1001:nsim,1],type="l",col="lightgreen")
abline(h=mbeta[1],col="blue")

plot(1001:nsim,sqrt(S2phi[1001:nsim]),type="l",col="lightgreen")
abline(h=sdphi,col="blue")

#n","SDphi = ",sdphi,"\n","SDe = ",sde)   
		
#		par(mfrow=c(2,1))
#		
#		plot(1001:nsim,Beta[1001:nsim,1],type="l",col="lightgreen")
#		abline(h=mbeta[1],col="blue")
#		
#		plot(1001:nsim,sqrt(S2phi[1001:nsim]),type="l",col="lightgreen")
#		abline(h=sdphi,col="blue")
		
		

