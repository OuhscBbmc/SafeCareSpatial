#Starting example is from https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(maps)
library(maptools)
library(spdep)

######################################
### Write a fake dataset to help with structure
######################################
dsPolygon = ggplot2::map_data('state')
dsStateDV <- data.frame(StateName = unique(dsPolygon$region), CountPerCapita=NA_integer_, Proportion=NA_real_)
dsStateDV$CountPerCapita <-  rpois(49, lambda=100)
dsStateDV$CountPerCapita[-sample(1:49, 10)] <- NA

dsStateDV$Proportion <-  runif(runif(49, 0, 1)) #I don't know why the example uses two 'runif's.
dsStateDV$Proportion[-sample(1:49, 10)] <- NA

# write.csv(dsStateDV, "National/Data/FakeData.csv", row.names=FALSE)
######################################
### Create dataset for labels
######################################

# The next six lines are just for writing labels on the non-missing states.  Labels may be unnecessary.
dsStatesForLabeles <- maps::map('state', fill=TRUE, col="transparent", plot=FALSE)
stateNamesForLabels <- sapply(strsplit(dsStatesForLabeles$names, ":"), function(x) x[1])
statePolygonsForLabels <- maptools::map2SpatialPolygons(dsStatesForLabeles, IDs=stateNamesForLabels, proj4string=CRS("+proj=longlat +datum=wgs84"))
dsStateLabels <- data.frame(StateName = unique(stateNamesForLabels), coordinates(statePolygonsForLabels))
dsStateLabels$Abbreviations <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", "IO", "KA", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NH", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VY", "VA", "WA", "WV", "WI", "WY")
dsStateLabels <- plyr::rename(dsStateLabels, replace=c("X1"="X", "X2"="Y"))

# write.csv(dsStateLabels, "National/Data/StateLabels.csv", row.names=FALSE)

# dsLabels <- read.csv("National/Data/StateLabels.csv", stringsAsFactors=FALSE)
# paste(dsLabels$Abbreviation, collapse="\", \"")

######################################
### Create neighbors object (nb) with labels
######################################
stateNBForLabels <- spdep::poly2nb(statePolygonsForLabels)
adjMatrix <- spdep::nb2mat(hh,style="B") #check that adjacency matrix can be created

