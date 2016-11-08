library(raster)#, lib.loc="/home/slingsby/Rlib")
library(rgdal)#, lib.loc="/home/slingsby/Rlib")
library(rgeos)#, lib.loc="/home/slingsby/Rlib")
library(dplyr)#, lib.loc="/home/slingsby/Rlib")
library(maps)
library(maptools)

setwd("/Users/jasper/Documents/GIS/VegToolsRaw")

#Get renewable energy polygons
reA <- readOGR("Rasters/REEA_OR_2016_Q2.shp", layer = "REEA_OR_2016_Q2")
reA <- spTransform(reA, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
reA <- reA[-which(reA@data$PROJ_STATU == "Withdrawn/Lapsed"),] #Remove "Withdrawn/Lapsed" development proposals
reA <- gBuffer(reA, byid=TRUE, width=0)

#Calculate the area
reA@data$AREA <- sapply(slot(reA, "polygons"), slot, "area")
#p <- lapply(reA@polygons, slot, "Polygons")
#lapply(p[[1]], function(x) slot(x, "area"))

#Get RSA map
#Fix errors by overlaying points and polygon
map <- map("world", region = "south africa", fill = TRUE, plot=F)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
map <- spTransform(map, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
map <- gBuffer(map, byid=TRUE, width=0)

map <- crop(map, extent(reA))
maparea <- sapply(slot(map, "polygons"), slot, "area")

#Calculate
sum(reA@data$AREA/maparea)*100

#%Cover change by technology
tech <- aggregate((reA@data$AREA/maparea)*100, by = list(reA@data$TECHNOLOGY), "sum")
appr <- aggregate((reA@data$AREA/maparea)*100, by = list(reA@data$PROJ_STATU), "sum")


barplot(height = tech$x, names.arg = tech$Group.1, cex.names=.75, las=2, ylab = "%Area of RSA")
barplot(height = appr$x, names.arg = appr$Group.1, cex.names=.75, las=2, ylab = "%Area of RSA")
