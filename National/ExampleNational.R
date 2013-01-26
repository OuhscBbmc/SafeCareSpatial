#Example: https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/Fw8WOZV72SQ
library(ggplot2)
library(scales)
library(maps)

states = map_data('state')
fake <- data.frame(us.states = unique(states$region),
                   area_percent = runif(runif(49, 0, 1)))
# fake$area_percent[-sample(1:49, 10)] <- NA

# reproduce your map
ggplot(fake, aes(map_id = us.states)) + 
  geom_map(aes(fill = area_percent, colour = 'grey90'), map = states) +
  expand_limits(x = states$long, y = states$lat)

# a) set instead of map colour 
#    don't put it in aes() if it takes a constant value
# b) change NA to light grey
P1 <- ggplot(fake, aes(map_id = us.states)) + 
  geom_map(aes(fill = area_percent), colour = 'grey90', map = states) +
  scale_fill_continuous(na.value = "grey75") + 
  expand_limits(x = states$long, y = states$lat)
P1

# c) add text labels to all states - compute centroids first
library(maptools)
states2 <- map('state', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(states2$names, ":"), function(x) x[1])
states_sp <- map2SpatialPolygons(states2, IDs=IDs,
                                 proj4string=CRS("+proj=longlat +datum=wgs84"))
state.label <- data.frame(us.states = unique(IDs), coordinates(states_sp))
P1 + geom_text(aes(X1, X2, label = us.states), data = state.label)

# just label the states with a value for your fill variable
P1 + geom_text(aes(X1, X2, label = us.states), 
               data = state.label[state.label$us.states %in% 
                                    fake$us.states[!is.na(fake$area_percent)], ])