############################
# Name: Aiden Metz         #
# STAT 331: Handout 16     #
############################


library(leaflet)
library(plyr)
library(dplyr)

setwd("C:/Users/student/Desktop/Stat331")
load("CA and NV.rdata")
strong <- CAandNV



cali_lat <- 36.732391
cali_lng <- -119.789400
leaflet() %>%
  setView(lat = cali_lat, lng = cali_lng, zoom = 7) %>%
  addProviderTiles("Esri.WorldStreetMap")

leaflet() %>%
  setView(lng = cali_lng, lat = cali_lat, zoom = 5) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = strong,
    radius = strong$mag
  )

# Code for handout #
bigs <- data.frame(name=c("Eureka","Loma Prieta","Cape Mendo",
                                "Ludlow","Ridgecrest","Northridge"),
                         latitude=c(41.13,37.04, 40.33,
                                      34.54,35.766,34.213),
                         longitude=c(-124.44,-121.877,-124.23,
                                      -116.39,-117.605,-118.537),
                         mag=c(7.4,6.9,7.2,
                               7.1,7.1,6.7))
leaflet() %>%
  setView(lat = cali_lat, lng = cali_lng, zoom = 5) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(data=strong, radius=5) %>%
  addCircleMarkers(data=bigs, fillColor = "green" , color="green")
?addCircleMarkers

library(ggplot2)
library(maps)
library(maptools)
library(ggmap)

states <- map_data("state")
ca <- subset(states, region %in% c("california"))

ggplot(data = ca) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black") 
ggplot(data = ca) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)


ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()
ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top


latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}
SDlat <- 32.720268
sdlng <- -117.150940
nclat <- 40.160508
nclng <- -121.576665
testpoints <- data.frame(x=c(-117, -121, -119), y=c(32.7, 40, 37))
testpoints
stcounty <- latlong2county(testpoints)

?strsplit
stcounty <- latlong2county(testPoints)
sapply(strsplit(stcounty, ","), FUN=function(x) x[1])
sapply(strsplit(stcounty, ","), FUN=function(x) x[2])
locs <- latlong2county(data.frame(x=strong[,4], y=strong[,3]))
strong$county <- sapply(strsplit(locs, ","), FUN=function(x) x[2])
strong$state <- sapply(strsplit(locs, ","), FUN=function(x) x[1])
head(strong)
as.logical(1-T)
strong <- strong[as.logical(1-is.na(strong)),]
ca <- strong[strong$state=="california",]
table(ca$county)
cc <- data.frame(table(ca$county))
colnames(cc) <- c("subregion", "numQuakes")
cacopa <- inner_join(ca_county, cc, by = "subregion")

elbow_room1 <- ca_base + 
  geom_polygon(data = cacopa, aes(fill = numQuakes), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw()

elbow_room1 
elbow_room1 + scale_fill_gradient(trans = "log10")
eb2 <- elbow_room1 + 
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(2, 4, 10, 100, 1000, 10000),
                       trans = "log10")
eb2
