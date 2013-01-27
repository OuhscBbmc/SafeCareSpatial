#Starting example is from https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
rm(list=ls(all=TRUE))
library(plyr)
library(ggplot2)
library(scales)
library(maps)
library(maptools)
# windows() #If you want to pop out the window

#Read in the three necessary datasets.
dsPolygon = ggplot2::map_data('state')
ds <- read.csv("National/Data/realScreenDat.csv") #Change this name.
ds$ScreenInCountPerThousand <- round(ds$ScreenInCount / ds$ChildPopulation * 1000, digits=0) 
ds$ScreenOutCountPerThousand <- round(ds$ScreenOutCount / ds$ChildPopulation * 1000, digits=0)
ds$ReferralCountPerThousand <- round(ds$ReferralCount / ds$ChildPopulation * 1000, digits=0)

dsLabels <- read.csv("National/Data/StateLabels.csv", stringsAsFactors=FALSE)
ds <- plyr::join(x=ds, y=dsLabels, by="StateName")
rm(dsLabels)

# dvName <- "ScreenInCountPerThousand"
# dvNamePretty <- "Screen In Count (per thousand)"
dvName <- "ScreenOutCountPerThousand"
dvNamePretty <- "Screen Out Count (per thousand)"
# dvName <- "ReferralCountPerThousand"
# dvNamePretty <- "Referral Count (per thousand)"

years <- sort(unique(ds$Year))

colorLow <- grDevices::hcl(h=45, c=20, l=90) #colorLow <- "#56B1F7"
colorHigh <- grDevices::hcl(h=25, c=60, l=50) #colorHigh <- "#132B43"

xLimits <- c(-122.5, -70) #range(dsPolygon$long)
yLimits <- c(26, 48.3) #range(dsPolygon$lat)

dvRangeAcrossYears <- range(ds[, dvName], na.rm=T)

for( year in years ) {
  #   year <- 2010
  dsYear <- ds[ds$Year==year, ]
  plotTitle <- paste0(dvNamePretty, "\n", year) #plotTitle <- paste0(dvNamePretty, ": ", year)
  fileName <- paste0("National/", dvName, year, ".png")
  #Draw the real map
  g <- ggplot(dsYear, aes(map_id = StateName)) 
  g <- g + geom_map(aes_string(fill=dvName), colour='grey90', map=dsPolygon, size=.1)
  g <- g + scale_fill_continuous(na.value = "grey75", limits=dvRangeAcrossYears, low=colorLow , high=colorHigh, name="", guide=guide_colorbar(nbin=5)) 
  g <- g + expand_limits(x=xLimits, y=yLimits)
  g <- g + coord_map() #Set the correct aspect ratio.
  g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm")) #Turn of coordinates
  g <- g + theme(plot.background=element_blank(), panel.background=element_blank()) #Clear background grid
  g <- g + theme(legend.position=c(.7,1.05), legend.justification=c("left","top")) #Move the legend
  g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #Remove blank border
  
  g <- g + theme(legend.background = element_rect(fill=NA))
  g <- g + annotate("text", x=xLimits[1], y=yLimits[1], label=plotTitle, hjust=0, vjust=0)
  
  #g <- g + geom_text(aes(X, Y, label=Abbreviation), size=3, colour="gray30")
  g <- g + geom_text(aes_string(x="X", y="Y", label=dvName), size=3, colour="gray30")
  
  g #Draw it to the window, but looking at the png (written below) is more important.
  
  width <- 10 #Set this to the page/slide width
  aspectRatio <- .54 #Keep this fixed.
  resolution <- 600 #The boundaries are 200 for drafts and 1200 for publications.  Watch out, b/c it throws off the legend proportion.
  
  ggsave(fileName, plot=g,  width=width, height=width*aspectRatio, units="in", dpi=resolution)#, bg="pink")
}