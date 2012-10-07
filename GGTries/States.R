
rm(list=ls(all=TRUE))
library(reshape2) # for melt
dsValue <- data.frame(s = tolower(rownames(USArrests)), USArrests)


dsLocation <- map_data("state")
ggplot(dsValue, aes(map_id = s)) + geom_map(aes(fill = Murder), map = dsLocation) + expand_limits(x = dsLocation$long, y = dsLocation$lat) + coord_map()
  
head(dsLocation)

# plot(map_data("state"))
# plot(map_data(map="county"))

dsLocation <- map_data(map="county", region="OK")
plot(dsLocation$long, dsLocation$lat, type="l")#, xlim=c(30, 40),)