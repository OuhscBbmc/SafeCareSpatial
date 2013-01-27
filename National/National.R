#Starting example is from https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
rm(list=ls(all=TRUE))
library(ggplot2)
library(scales)
library(maps)
library(maptools)
library(colorspace)
# windows() #If you want to pop out the window

#Read in the three necessary datasets.
dsPolygon = map_data('state')
ds <- read.csv("National/Data/realScreenDat.csv") #Change this name.
# ds <- read.csv("National/Data/FakeStaticData.csv") #Change this name.
dsLabels <- read.csv("National/Data/StateLabels.csv", stringsAsFactors=FALSE)

dvName <- "ChildPopulation"
dvName <- "ScreenInCount"
dvName <- "ScreenInPercent"
dvName <- "ScreenOutCount"
dvName <- "ScreenOutPercent"
dvName <- "TotalReferralCount"
dvName <- "TotalReferralRate"
years <- sort(unique(ds$Year))

# colorLow <- "#56B1F7"
# colorHigh <- "#132B43"
colorLow <- hcl(h=45, c=20, l=90)
colorHigh <- hcl(h=25, c=60, l=50)

# for( year in years ) {
  year <- 2010
  dsYear <- ds[ds$Year==year, ]
  #Draw the real map
  g <- ggplot(dsYear, aes(map_id = StateName)) 
  g <- g + geom_map(aes_string(fill=dvName), colour='grey90', map=dsPolygon, size=.1)
  g <- g + scale_fill_continuous(na.value = "grey75", low=colorLow , high=colorHigh, name="", guide=guide_colorbar()) 
  g <- g + expand_limits(x = dsPolygon$long, y = dsPolygon$lat)
  g <- g + coord_map() #Set the correct aspect ratio.
  g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm")) #Turn of coordinates
  g <- g + theme(plot.background=element_blank(), panel.background=element_blank()) #Clear background grid
  g <- g + theme(legend.position=c(.8,1), legend.justification=c("right","top")) #Move the legend
#   g <- g + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) #Remove blank border
  g <- g + labs(main="dssd", x="xxxx")
#   g <- g + ggtitle("dssd")
  


  # # dsLabels <- dsLabels[dsLabels$StateName %in% dsLabels$StateName[!is.na(ds$CountPerCapita)], ]
  g <- g + geom_text(data=dsLabels, aes(X, Y, label=Abbreviation), size=3, colour="gray30")
  
  g #Draw it to the window, but looking at the png (written below) is more important.
  
  width <- 10 #Set this to the page/slide width
  aspectRatio <- .52 #Keep this fixed.
  resolution <- 600 #The boundaries are 200 for drafts and 1200 for publications.  Watch out, b/c it throws off the legend proportion.
  
  ggsave("National/NationalMap.png", plot=g,  width=width, height=width*aspectRatio, units="in", dpi=resolution)
# }