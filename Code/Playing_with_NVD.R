##########################################
######## R code for playing with the National Vegetation Database
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 19 September 2016
##########################################

##########################################
###1) Get libraries and setwd
##########################################
###Install libraries
library(gdata)
library(sp)
library(maps)
library(maptools)
library(raster)
library(ggplot2)
library(gstat)
library(rgdal)
library(gjam)

###Set data working directory
datwd <- "/Users/jasper/Documents/Databases/NVD/NVD_16_09_2016/"

##########################################
###2) Get data
##########################################

###Spatial data
#Get 2006 vegmap shapefile
veg <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/VM2006/vegmap2006_geo.shp", layer = "vegmap2006_geo") #Vegmap
#pen <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/pentads/pentads/PENT_PRO.SHP", layer = "PENT_PRO")

rP <- read.csv("/Users/jasper/Dropbox/SAEON/Grants/FBIP2016/rePhoto/Index to database 16.iv.2013.csv", stringsAsFactors = F) #rePhoto database

#HD <- read.csv(paste0(datwd, "HeaderData_NVD2014.txt")) #Header Data
HD <- read.csv(paste0(datwd, "HeaderData_NVD2014.csv")) #Header Data
#HD2 <- read.xls(paste0(datwd, "HeaderData_NVD2014.xlsx"), sheet=1) #Header Data
SD <- read.csv(paste0(datwd, "SpeciesData.txt")) #Species Data
SN <- read.csv(paste0(datwd, "SpeciesList.txt")) #Species Names
MD <- read.csv(paste0(datwd, "MetadataActionsEtc_Head.txt")) #Meta Data

##########################################
###3) Summary of fields
##########################################

HeadSummary <- data.frame(
  Complete = sapply(HD, function(x){sum(!is.na(x))}), #Non-NA values
  LengthUniqueVals = sapply(HD, function(x){length(levels(as.factor(x)))}), #Number of unique values or factor levels
  UniqueVals = sapply(HD, function(x){if(length(levels(as.factor(x)))<100) {paste(levels(as.factor(x)), collapse=",")} else {">100"}}))
#write.csv(HeadSummary, paste0(datwd,"SummaryStats.csv"))

MDSummary <- data.frame(
  Complete = sapply(MD, function(x){sum(!is.na(x))}), #Non-NA values
  LengthUniqueVals = sapply(MD, function(x){length(levels(as.factor(x)))}), #Number of unique values or factor levels
  UniqueVals = sapply(MD, function(x){if(length(levels(as.factor(x)))<100) {paste(levels(as.factor(x)), collapse=",")} else {">100"}}))

SDSummary <- data.frame(
  Complete = sapply(SD, function(x){sum(!is.na(x))}), #Non-NA values
  LengthUniqueVals = sapply(SD, function(x){length(levels(as.factor(x)))}), #Number of unique values or factor levels
  UniqueVals = sapply(SD, function(x){if(length(levels(as.factor(x)))<100) {paste(levels(as.factor(x)), collapse=",")} else {">100"}}))

SNSummary <- data.frame(
  Complete = sapply(SN, function(x){sum(!is.na(x))}), #Non-NA values
  LengthUniqueVals = sapply(SN, function(x){length(levels(as.factor(x)))}), #Number of unique values or factor levels
  UniqueVals = sapply(SN, function(x){if(length(levels(as.factor(x)))<100) {paste(levels(as.factor(x)), collapse=",")} else {">100"}}))

##########################################
###4) Visualize plots and repeat photos
##########################################

p <- rP
p$DD..S. <- -as.numeric(p$DD..S.)
p$DD..E. <- as.numeric(p$DD..E.)
p <- p[!is.na(p$DD..S.) & !is.na(p$DD..E.),]
coordinates(p) <- cbind(p$DD..E., p$DD..S.)
proj4string(p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #Assign a coordinate reference system (Geographic WGS84)

#Make new data frame to use for spatial analyses
d <- HD[!is.na(HD$Lat) & !is.na(HD$Long),] # remove plots with no Lat or Long

#Convert data frame to a SpatialPointsDataFrame
coordinates(d) <- cbind(d$Long, d$Lat)
proj4string(d) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #Assign a coordinate reference system (Geographic WGS84)

#Fix errors by overlaying points and polygon
map <- map("world", region = "south africa", fill = TRUE, plot=F)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

map <- crop(map, d)

#plot(map)
#points(d, cex=.25) #, cex=log(d$Loc_Conf)/2)
#points(p, cex=.25, col="blue")


d <- d[-which(d$Loc_Conf<0),]
d <- d[-which(is.na(d$Loc_Conf)),]

x <- d %over% map
v <- d %over% veg
d$Veg <- v$NAME
#d <- d[which(x==1), ]
#x <- d %over% map
y <- p %over% map

#plot(map)
#points(d[which(x==1), ], cex=.1*log(d[which(x==1), ]$Loc_Conf+1)) #Plots
#points(d[is.na(x),], cex=.25, col="red") #Plots at sea (or next door)
#points(p[which(y==1),], cex=.25, col="blue") #rePhoto pics

#hist(MD$SelectYear, xlab = "Year", main = "", ylab = "# of Studies")
#hist(d$SURF_AREA, main="", xlab="Plot area (m^2)", ylab="# of plots", breaks=seq(0, 5000, 10))

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

save(datt, spp, file = "NVD_Clark.Rdata")

#########################
#Inverse distance weighting of "plot occurrence"
d$Certainty <- 1/d$Loc_Conf
d$PlotLoc <- 1

r <- raster(ext = extent(map), crs = CRS(proj4string(map)), resolution = 1/10, vals=NULL)
mr <- rasterize(map, r)

dr <- rasterize(d, r, field = "PlotLoc", fun="sum")
plot(dr) #Plots per cell...

mg <- as(mr, "SpatialGrid")

cm1 <- idw(log(Loc_Conf) ~ 1, d, mg, idp = 1) #Unweighted
plot(cm1)
plot(map, add=T)
points(d[which(x==1), ], cex=.1*log(d[which(x==1), ]$Loc_Conf+1), col= grey(.5, alpha=.25)) #Plots

cm2 <- idw(PlotLoc ~ 1, d, mg, idp = Certainty*25) #inverse weighted by plot location confidence

###################
HK <- d[which(d$ORIG_DB=="HANGKLIP"),]
plot(HK, cex=.25)
plot(map, add=T)

dem <- getData("SRTM", lon=19, lat=-34)
demHK <- crop(dem, extent(HK)+.25)

plot(demHK)
points(HK, cex=.25, col="blue")

#proj4string(HK) <- "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=0 +y_0=0 +axis=enu +a=6378249.145 +b=6356514.966398753 +towgs84=-136,-108,-292,0,0,0,0 +units=m +no_defs" #Cape Datum

"+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +axis=wsu +a=6378249.145 +b=6356514.966398753 +units=m +no_defs "

#Hartebeeshoek94 #showP4(showWKT("+init=epsg:2048")) #"+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +axis=wsu +ellps=WGS84 +units=m +no_defs "

HK_lo19 <- spTransform(HK, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(demHK)
points(HK_lo19, cex=.25, col="blue")

x <- read.csv("/Users/jasper/Desktop/HangKlipDMS.csv")
x$Lat <- -as.numeric(substr(x$Latitude, 1, 2)) - as.numeric(substr(x$Latitude, 3, 4))/60 - as.numeric(substr(x$Latitude, 5, 6))/3600
x$Long <- as.numeric(substr(x$Longitude, 1, 2)) + as.numeric(substr(x$Longitude, 3, 4))/60 + as.numeric(substr(x$Longitude, 5, 6))/3600

coordinates(x) <- cbind(x$Long, x$Lat)
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #Assign a coordinate reference system (Geographic WGS84)


y <- spTransform(x, CRS("+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "))

#z <- spTransform(y, CRS("+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=0 +y_0=0 +axis=enu +a=6378249.145 +b=6356514.966398753 +towgs84=-136,-108,-292,0,0,0,0 +units=m +no_defs"))

proj4string(y) <- "+proj=tmerc +lat_0=0 +lon_0=19 +k=1 +x_0=0 +y_0=0 +axis=wsu +a=6378249.145 +b=6356514.966398753 +units=m +no_defs " #Cape Datum Lo19

y <- spTransform(z, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

pdf("Output/HangKlip_Location.pdf", height=7, width=7)
plot(is.na(demHK), col=c("white", "light blue"), legend=F)
plot(demHK, add=T)#, col=terrain.colors(30, alpha=.8))
points(x, cex=.5, col="blue")
points(y, cex=.5, col="black")
points(HK, cex=.5, col="red")
dev.off()


##

#Get basemap data
map <- map_data("world", region = "south africa")

#Plot
ggplot(as.data.frame(d),aes(x=Long,y=Lat))+
  geom_polygon(aes(x=long,y=lat,group=group,order=order),data=map)+
  geom_point(col="red")+
  coord_equal()