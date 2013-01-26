rm(list=ls(all=TRUE))
library(ggplot2)
library(scales)
library(maps)
# windows()

dsPolygon = map_data('state')

#Create a fake dataset to help with structure
# ds <- data.frame(StateName = unique(dsPolygon$region), CountPerCapita = runif(runif(49, 0, 1)))
# ds$CountPerCapita[-sample(1:49, 10)] <- NA
# write.csv(ds, "National/Data/FakeData.csv", row.names=FALSE)



# a) set instead of map colour 
#    don't put it in aes() if it takes a constant value
# b) change NA to light grey
g <- ggplot(ds, aes(map_id = StateName)) 
g <- g + geom_map(aes(fill=CountPerCapita), colour='grey90', map=dsPolygon)
g <- g + scale_fill_continuous(na.value = "grey75") 
g <- g + expand_limits(x = dsPolygon$long, y = dsPolygon$lat)
g <- g + coord_map() #Set the correct aspect ratio.
g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm")) #Turn of coordinates
g <- g + theme(plot.background=element_blank(), panel.background=element_blank()) #Clear background grid
g <- g + theme(legend.position=c(.8,.05), legend.justification=c("left","bottom")) #Move the legend
g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #Remove blank border

g

width <- 10
aspectRatio <- .52
resolution <- 600 #The boundaries are 200 for drafts and 1200 for publications.  Watch out, b/c it throws off the legend proportion.
# png(filename="National/NationalMap.png", width=width, height=width*aspectRatio, units="in", bg="white", res=resolution)
# g
# dev.off()
ggsave("National/NationalMap.png", plot=g,  width=width, height=width*aspectRatio, units="in", dpi=resolution)#, bg=NA)