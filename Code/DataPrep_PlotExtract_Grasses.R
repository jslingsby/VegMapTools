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
library(vegan)
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

#SAPIA <- read.csv("/Users/jasper/Documents/Databases/SAPIA/SAPIA.csv", stringsAsFactors = FALSE)

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
#x <- d %over% map #Points that are on land (1) vs in the sea (0)
v <- d %over% veg #Extract vegmap data for each plot (no guarantees of spatial accuracy though!)
d$Veg <- v$NAME #Add veg type name to plot data
d$Biome <- v$BIOME #Add biome name to plot data

##########################################
###4) Trim plots based on various criteria
##########################################

#Confidence in the spatial information - remove plots with no confidence (0 or NA)
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

#Points that are on land (1) vs in the sea (0), or use gDistance to trim by distance from the coast
#x <- d %over% map 
x <- as.vector(gDistance(d, map, byid=TRUE))
#hist(y[which(x>0)])
d <- d[-which(x>1000),] #Trim points >1km from the coast

##########################################
###5) Explore other criteria
##########################################

#dat <- d[, which(names(d) %in% c("RELEVE_NR", "ORIG_DB", "Veg", "Loc_Conf", "Lat", "Long", "SURF_AREA", "PlotAreaConf", "SpeciesBias"))]@data

#Trim to plots with good surface area information
d <- d[which(d$PlotAreaConf == "Use" & d$SpeciesBias %in% c("All species present", "Full floristics", "Permanently recognisable species", "Grasses, extremely few other")),]

#Trim to plots with certain surface areas
#d <- d[which(d$SURF_AREA %in% c(16,25,50,100,200,400,900)),]

##########################################
###6) Match with plot species lists, calculate SR and generate Genus table
##########################################

#Extract species lists
DS <- SD[which(SD$RELEVE_NR %in% d$RELEVE_NR),] 

#Make genus level community matrix by matching species names and extracting genus
SSN <- SN[,c("SPECIES_NR", "SPECIESNAME", "FamName")] #Extract Genus names
DS <- merge(DS, SSN, all.x = TRUE)
#DS$Genus <- apply(as.data.frame(DS$SPECIESNAME), MARGIN = 1, FUN = function(x){strsplit(x, split = " ")[[1]][1]})

#Remove non-"higher" plants
#exfam <- c("ANEMIACEAE", "ASPLENIACEAE", "AZOLLACEAE", "BARTRAMIACEAE", "BLECHNACEAE", "BRACHYTHECIACEAE", "BRYACEAE", "CYATHEACEAE", "DENNSTAEDTIACEAE", "DICRANACEAE", "DRYOPTERIDACEAE", "ELAPHOGLOSSACEAE", "EQUISETACEAE", "FABRONIACEAE", "FISSIDENTACEAE", "FONTINALACEAE", "GLEICHENIACEAE", "HYMENOPHYLLACEAE", "HYPNACEAE", "ISOETACEAE", "JUNGERMANNIACEAE", "LYCOPODIACEAE", "MARATTIACEAE", "MARCHANTIACEAE", "MARSILEACEAE", "MNIACEAE", "OPHIOGLOSSACEAE", "OSMUNDACEAE", "PALLAVICINIACEAE", "PARMELIACEAE", "PLAGIOCHILACEAE", "POLYPODIACEAE", "POTTIACEAE", "PTERIDACEAE", "RICCIACEAE", "SCHIZAEACEAE", "SELAGINELLACEAE", "SEMATOPHYLLACEAE", "SPHAGNACEAE", "THELYPTERIDACEAE", "THUIDIACEAE", "VITTARIACEAE", "WOODSIACEAE")

#DS <- DS[-which(DS$FamName %in% exfam),]

#Remove alien species
#SAPIA <- trim(SAPIA)
#SAPIA <- SAPIA[-which(SAPIA$species %in% c("cf.", "sp.", "cf")),]
#SAPIA$Names <- apply(SAPIA[,1:2], MARGIN = 1, FUN = "paste0", collapse = " ")

#DS <- DS[-which(DS$SPECIESNAME %in% SAPIA$Names),]

DS <- DS[which(DS$FamName == "POACEAE"),] #Extract grasses only

#Make  matrix
DS$SPECIESNAME <- as.character(DS$SPECIESNAME)
y <- tapply(DS$COVER_PERC, list(DS$RELEVE_NR, DS$SPECIESNAME), sum)
y[is.na(y)] <- 0
spp1 <- as.data.frame(y)
rownames(spp1) <- unique(DS$RELEVE_NR)

#Calculate species number
#Trim plot data to those with species lists
d <- d[-which(!d$RELEVE_NR %in% DS$RELEVE_NR),] 
#d$SpeciesRichness <- aggregate(DS$SPECIES_NR, by = list(DS$RELEVE_NR), FUN = function(x){length(unique(x))})[,2] 

##########################################
###7) Trim and export data
##########################################

spp <- decostand(spp1, "pa")

ed <- d[, c("RELEVE_NR", "ORIG_DB", "PlotYear", "Lat", "Long", "Loc_Conf", "Biome", "MapName50k", "SURF_AREA")]

write.csv(spp, "Data/grassmatrix_PA.csv")
write.csv(ed, "Data/grass_PA_plotdat.csv")

##########################################
##################END#####################
##########################################


##########################################
###8) Explore data
##########################################

#Plot area issue...
summary(as.factor(d$SURF_AREA)) #100m2 looks best...

#Explore surface areas by biome
table(d$Biome, d$SURF_AREA)

#Plot species richness by area
dat <- d@data

ggplot(data = dat, aes(x = SURF_AREA, y = SpeciesRichness, colour = Biome)) + geom_point() + facet_wrap(~Biome) + geom_smooth(method = "loess")

ggplot(data = dat, aes(x = as.factor(SURF_AREA), y = SpeciesRichness, colour = Biome)) + geom_boxplot() + facet_wrap(~Biome)




#Explore cutoffs for location confidence...
summary(as.factor(d$Loc_Conf))
sum(d$Loc_Conf<5001) #5km
sum(d$Loc_Conf<2501)
sum(d$Loc_Conf<2001) #2km


#Explore surface areas by biome
table(d$Biome, d$SURF_AREA)

#Plot plots on a map coloured by biome
plot(map)
points(d[which(d$Biome=="Fynbos Biome"),], col="purple")
points(d[which(d$Biome=="Succulent Karoo Biome"),], col="yellow")
points(d[which(d$Biome=="Nama-Karoo Biome"),], col="red2")
points(d[which(d$Biome=="Savanna Biome"),], col="goldenrod")
points(d[which(d$Biome=="Grassland Biome"),], col="green")
points(d[which(d$Biome=="Indian Ocean Coastal Belt"),], col="maroon")
points(d[which(d$Biome=="Albany Thicket Biome"),], col="forestgreen")
points(d[which(d$Biome=="Azonal Vegetation"),], col="blue")
points(d[which(d$Biome=="Forests"),])

#Biomes of interest
#d <- d[which(d$Biome %in% c("Fynbos Biome", "Succulent Karoo Biome","Forests")),]

#levels(as.factor(d$Biome))





y <- tapply(DS$COVER_PERC, list(DS$RELEVE_NR, DS$SPECIES_NR), sum)
y[is.na(y)] <- 0
spp <- as.data.frame(y)

rownames(datt) <- datt$RELEVE_NR
#rownames(spp) <- spp$RELEVE_NR

datt <- datt[which(rownames(datt)%in%rownames(spp)),]
