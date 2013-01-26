#Starting example is from https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
rm(list=ls(all=TRUE))
library(ggplot2)
library(scales)
library(maps)
library(maptools)
# windows() #If you want to pop out the window

#Read in the three necessary datasets.
dsPolygon = map_data('state')
ds <- read.csv("National/Data/FakeData.csv") #Change this name.
dsLabels <- read.csv("National/Data/StateLabels.csv", stringsAsFactors=FALSE)

#Draw the real map
g <- ggplot(ds, aes(map_id = StateName)) 
g <- g + geom_map(aes(fill=CountPerCapita), colour='grey90', map=dsPolygon, size=.1)
g <- g + scale_fill_continuous(na.value = "grey75") 
g <- g + expand_limits(x = dsPolygon$long, y = dsPolygon$lat)
g <- g + coord_map() #Set the correct aspect ratio.
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm")) #Turn of coordinates
g <- g + theme(plot.background=element_blank(), panel.background=element_blank()) #Clear background grid
g <- g + theme(legend.position=c(.8,.05), legend.justification=c("left","bottom")) #Move the legend
g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #Remove blank border

# The next two lines are just for writing labels on the non-missing states. Labels may be unnecessary.
dsLabels <- dsLabels[dsLabels$StateName %in% dsLabels$StateName[!is.na(ds$CountPerCapita)], ]
g <- g + geom_text(data=dsLabels, aes(X, Y, label=Abbreviation), size=3, colour="gray30")

g #Draw it to the window, but looking at the png (written below) is more important.

width <- 10 #Set this to the page/slide width
aspectRatio <- .52 #Keep this fixed.
resolution <- 600 #The boundaries are 200 for drafts and 1200 for publications.  Watch out, b/c it throws off the legend proportion.

ggsave("National/NationalMap.png", plot=g,  width=width, height=width*aspectRatio, units="in", dpi=resolution)