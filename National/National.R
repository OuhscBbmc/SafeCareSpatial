#Starting example is from https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
rm(list=ls(all=TRUE))
library(ggplot2)
library(scales)
library(maps)
library(maptools)
# windows() #If you want to pop out the window

dsPolygon = map_data('state')
ds <- read.csv("National/Data/FakeData.csv")

#Write a fake dataset to help with structure
# ds <- data.frame(StateName = unique(dsPolygon$region), CountPerCapita = runif(runif(49, 0, 1)))
# ds$CountPerCapita[-sample(1:49, 10)] <- NA
# write.csv(ds, "National/Data/FakeData.csv", row.names=FALSE)


#Drat the real map
g <- ggplot(ds, aes(map_id = StateName)) 
g <- g + geom_map(aes(fill=CountPerCapita), colour='grey90', map=dsPolygon, size=.1)
g <- g + scale_fill_continuous(na.value = "grey75") 
g <- g + expand_limits(x = dsPolygon$long, y = dsPolygon$lat)
g <- g + coord_map() #Set the correct aspect ratio.
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm")) #Turn of coordinates
g <- g + theme(plot.background=element_blank(), panel.background=element_blank()) #Clear background grid
g <- g + theme(legend.position=c(.8,.05), legend.justification=c("left","bottom")) #Move the legend
g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #Remove blank border

# The next six lines are just for writing labels on the non-missing states.  Labels may be unnecessary.
dsStatesForLabeles <- map('state', fill=TRUE, col="transparent", plot=FALSE)
stateNamesForLabels <- sapply(strsplit(dsStatesForLabeles$names, ":"), function(x) x[1])
statePolygonsForLabels <- map2SpatialPolygons(dsStatesForLabeles, IDs=stateNamesForLabels, proj4string=CRS("+proj=longlat +datum=wgs84"))
stateLabel <- data.frame(StateName = unique(stateNamesForLabels), coordinates(statePolygonsForLabels))
dsSelectedNames <- stateLabel[stateLabel$StateName %in% ds$StateName[!is.na(ds$CountPerCapita)], ]
g <- g + geom_text(data=dsSelectedNames, aes(X1, X2, label=StateName), size=3  )

g #Draw it to the window, but looking at the png (written below) is more important.

width <- 10 #Set this to the page/slide width
aspectRatio <- .52 #Keep this fixed.
resolution <- 600 #The boundaries are 200 for drafts and 1200 for publications.  Watch out, b/c it throws off the legend proportion.

ggsave("National/NationalMap.png", plot=g,  width=width, height=width*aspectRatio, units="in", dpi=resolution)