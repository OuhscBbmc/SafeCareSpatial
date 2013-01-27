# TODO: Add comment
# 
# Author: DBARD
###############################################################################
library(survey)

setwd("P:\\SPSS\\Elizabeth\\Dissertation")
load("mofo3.RData")
ls()

#setwd("P:\\SPSS\\Elizabeth\\Dissertation")
#save.image("mofo3.RData")

cePredVars <- c("pCNoSxb","pCNoXPb","frcsex","pComb","pCNoPSb")
allNewVars <- c("AGENEW","EDUNEW","WRK12MNEW","MARNEW","WLTHNEW","PTEDUNEW","PTOCPNEW","OUTERNSNEW","TOTBIRNEW","SONSNEW","DGTRSNEW","WHOMONNEW",
		"HLTHCARNEW","LHHPRCHNEW","SHHPRCHNEW","VSFAMNEW","FDCOOKNEW","HUSMONNEW","Urb")

COIR60FLdat$Urb <- ifelse(COIR60FLdat[,"RESTYPE"]=="Urban",rep(1,nrow(COIR60FLdat)),
ifelse(COIR60FLdat[,"RESTYPE"]=="Rural",rep(0,nrow(COIR60FLdat)),NA))
factor

storePred <- ipvOnRiskFactor(cePredVars = cePredVars, ds = COIR60FLdat, prdVar = allNewVars[19], trend=0, nestVal=F)
names(storePred) <- cePredVars

allDomMeans <- mapply("[",storePred,"domMeans",USE.NAMES = T)
#write.table(t(do.call("rbind",allDomMeans)),"clipboard",sep="\t",row.names=F,col.names=F)
allDomMeans2 <- t(do.call("rbind",allDomMeans))
allTrend <- mapply("[",storePred,"trendTest",USE.NAMES = T)
#write.table(t(do.call("rbind",allTrend)),"clipboard",sep="\t",row.names=F,col.names=F)
allTrend2 <- t(do.call("rbind",allTrend))
allSampSize <- mapply("[",storePred,"sampSize",USE.NAMES = T)
medAllSampSize <- apply(do.call("rbind",allSampSize),2,median,na.rm=T)
sampSizeMargProp <- c(paste("n=",medAllSampSize,sep=""),mapply("[",storePred,"margProps",USE.NAMES = T)[[1]])
#write.table(medAllSampSize,"clipboard",sep="\t",row.names=F,col.names=F)
allOut <- cbind(sampSizeMargProp,rbind(allTrend2,allDomMeans2))
write.table(allOut,"clipboard",sep="\t",row.names=F,col.names=F,na="")

cePredVars = cePredVars; ds = COIR60FLdat; prdVar = allNewVars[18]; trend=0; nestVal=F

dcluswr <- svydesign(id=~CLUSTER, strata=~STRATNUM, weights=~SAMPWGT2, data=ds,nest=nestVal) 
summary(dcluswr)

chkReg <- svyglm(as.formula(paste(cePredVars[1],"~","as.factor(",prdVar,")")),design=subset(dcluswr,INTYES==1),family=quasibinomial())
smryReg <- regTermTest(chkReg, as.formula(paste("~","as.factor(",prdVar,")")))
smryReg <- regTermTest(chkReg, as.formula("~'as.factor(HUSMONNEW)1'-'as.factor(HUSMONNEW)2'"))
pVal <- smryReg$p

ctest01 <- svycontrast(chkReg,list(diff=c('as.factor(HUSMONNEW)1'=1)))
ctest02 <- svycontrast(chkReg,list(diff=c('as.factor(HUSMONNEW)2'=-1)))
ctest12 <- svycontrast(chkReg,list(diff=c('as.factor(HUSMONNEW)1'=1,'as.factor(HUSMONNEW)2'=-1)))

table(ds[,prdVar],factor(ds[,prdVar],levels=c(2,1,0)))


cePredVars <- c("pCNoSxb","pCNoXPb","frcsex","pComb","pCNoPSb")
cePredVars = cePredVars; ds = COIR60FLdat; prdVar = allNewVars[18]; trend=0; nestVal=F
allNewVars <- c("AGENEW","EDUNEW","WRK12MNEW","MARNEW","WLTHNEW","PTEDUNEW","PTOCPNEW","OUTERNSNEW","TOTBIRNEW","SONSNEW","DGTRSNEW","WHOMONNEW",
		"HLTHCARNEW","LHHPRCHNEW","SHHPRCHNEW","VSFAMNEW","FDCOOKNEW","HUSMONNEW")
dcluswr <- svydesign(id=~CLUSTER, strata=~STRATNUM, weights=~SAMPWGT2, data=COIR60FLdat)#,nest=T) 
summary(dcluswr)

chkReg <- svyglm(as.formula(paste(cePredVars[1],"~","as.factor(",prdVar,")")),design=subset(dcluswr,INTYES==1),family=quasibinomial())
smryReg <- regTermTest(chkReg, as.formula(paste("~","as.factor(",prdVar,")")))	






