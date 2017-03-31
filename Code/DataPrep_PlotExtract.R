##############################################################################
######## Extract NVD data for Felix
##############################################################################
######## Compiled by Jasper Slingsby 2017
######## Last edited: 13 March 2017
##############################################################################

##########################################
###2) Get libraries and datwd
##########################################

library(gdata)
library(sp)
library(maps)
library(maptools)
library(raster)
library(ggplot2)
library(gstat)
library(rgdal)
library(rgeos)
#library(gjam)

###Set data working directory
datwd <- "/Users/jasper/Documents/Databases/NVD/NVD_16_09_2016/"

##########################################
###2) Get data
##########################################

###Spatial data
#Get 2006 vegmap shapefile
veg <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/VM2006/vegmap2006_geo.shp", layer = "vegmap2006_geo") #Vegmap
#pen <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/pentads/pentads/PENT_PRO.SHP", layer = "PENT_PRO")

#HD <- read.csv(paste0(datwd, "HeaderData_NVD2014.txt")) #Header Data
HD <- read.csv(paste0(datwd, "HeaderData_NVD2014.csv")) #Header Data
#HD2 <- read.xls(paste0(datwd, "HeaderData_NVD2014.xlsx"), sheet=1) #Header Data
SD <- read.csv(paste0(datwd, "SpeciesData.txt")) #Species Data
SN <- read.csv(paste0(datwd, "SpeciesList.txt")) #Species Names
MD <- read.csv(paste0(datwd, "MetadataActionsEtc_Head.txt")) #Meta Data

##########################################
###3) Spatial checking and subsetting
##########################################

#Make new data frame to use for spatial analyses
d <- HD[!is.na(HD$Lat) & !is.na(HD$Long),] # remove plots with no Lat or Long

#Convert data frame to a SpatialPointsDataFrame
coordinates(d) <- cbind(d$Long, d$Lat)
proj4string(d) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #Assign a coordinate reference system (Geographic WGS84)

#Fix errors by overlaying points and polygon
map <- map("world", region = "south africa", fill = TRUE, plot=F)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Crop map to the ectent of the points
map <- crop(map, d)

#Overlay points, map and vegmap
#x <- d %over% map #Points that are on land (1) vs in the see (0)
v <- d %over% veg #Extract vegmap data for each plot (no guarantees of spatial accuracy though!)
d$Veg <- v$NAME #Add veg type name to plot data
d$Biome <- v$BIOME #Add biome name to plot data

##########################################
###4) Trim plots based on various criteria
##########################################

#Confidence in the spatial information
d <- d[-which(d$Loc_Conf<0),]
d <- d[-which(is.na(d$Loc_Conf)),]

#Fix Lat-Long errors
d <- d[-which(d$Long<0), ] #drop TRANSK-B which all duplicated, -ve longitudes...
d$Lat[which(d$Lat>0)] <- d$Lat[which(d$Lat>0)] * -1 #Fix a bunch of plots that have +ve Lat

#Replot once we've fixed the coords and reproject to UTM34S
prjd <- proj4string(d)
d <- as.data.frame(d)
coordinates(d) <- cbind(d$Long, d$Lat)
proj4string(d) <- prjd
d <- spTransform(d, CRS("+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
map <- spTransform(map, CRS("+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#Points that are on land (1) vs in the sea (0)
y <- as.vector(gDistance(d, map, byid=TRUE))
hist(y[which(y>0)])


#x <- d %over% map 
hmm <- d[is.na(x),]


datt <- dat[which(dat$PlotAreaConf == "Use" & dat$SpeciesBias %in% c("All species present", "Full floristics", "Permanently recognisable species")),]


#Biomes of interest
#d <- d[which(d$Biome %in% c("Fynbos Biome", "Succulent Karoo Biome","Forests")),]

#levels(as.factor(d$Biome))


###
#Pull out key data for uncertainty analysis (need to add Acocks data, ID uncertainty, ...what else?)
dat <- d[, which(names(d) %in% c("RELEVE_NR", "ORIG_DB", "Veg", "Loc_Conf", "Lat", "Long", "SURF_AREA", "PlotAreaConf", "SpeciesBias"))]@data

datt <- dat[which(dat$PlotAreaConf == "Use" & dat$SpeciesBias %in% c("All species present", "Full floristics", "Permanently recognisable species")),]

datt <- datt[which(datt$Loc_Conf < 20),]

DS <- SD[which(SD$COVER_PERC>0 & SD$COVER_PERC<101),] #Trim spp w cover <0 or >100%
DS <- DS[which(DS$RELEVE_NR %in% datt$RELEVE_NR),]

y <- tapply(DS$COVER_PERC, list(DS$RELEVE_NR, DS$SPECIES_NR), sum)
y[is.na(y)] <- 0
spp <- as.data.frame(y)

rownames(datt) <- datt$RELEVE_NR
#rownames(spp) <- spp$RELEVE_NR

datt <- datt[which(rownames(datt)%in%rownames(spp)),]
