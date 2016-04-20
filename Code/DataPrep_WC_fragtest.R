##########################################
######## R code for setting up vegmap, transformation 
######## and protected area data for VegMapTools analyses
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 14 April 2016
##########################################

##########################################
###1) Get libraries and setwd
##########################################
library(raster)
library(gdalUtils)
library(rgdal)
library(dplyr)
library(SDMTools)


if(Sys.getenv("USER")=="jasper") {datwd <- "/Users/jasper/Documents/GIS/VegToolsRaw/"}
if(Sys.getenv("USERNAME")=="MatlalaM") {datwd <- "Your data directory"}
if(Sys.getenv("USERNAME")=="Receptionist") {datwd <- "C:/Users/Receptionist/Dropbox/Academics/PhD/Data/"}

##########################################
###2) Get and process data
##########################################

#VT <- stack(c("/Users/jasper/Documents/GIS/VegToolsRaw/Rasters/LC13test_web.tif", "/Users/jasper/Documents/GIS/VegToolsRaw/Rasters/PA_npaes_test_web.tif", "/Users/jasper/Documents/GIS/VegToolsRaw/Rasters/VEG12test_web.tif"))

#Land cover
lc <- raster(paste(datwd,"Rasters/LC13test_web.tif", sep=""))
proj4string(lc) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #EPSG3857 - Web Mercator

## crop lc data and subset
nex <- extent(2020000, 2140000, -4100000, -3950000)
lc <- crop(lc, nex)

## Trim lc down to natural/transformed only (using two to compare differences in ClassStat based on the different categories)
LC3V <- deratify(lc, att="LC3V", layer=1, complete=TRUE, drop=TRUE) 
LC3 <- deratify(lc, att="LC3", layer=1, complete=TRUE, drop=TRUE)


##########################################
##### 2) Fragmentation analysis #########
##########################################

#calculate fragementation

#do the connected component labelling
ccl.v = ConnCompLabel(LC3V)
ccl.3 = ConnCompLabel(LC3)

#calculate the patch statistics
ps.dataV = PatchStat(ccl.v)
ps.data3 = PatchStat(ccl.3)


#calculate the class statistics
cl.dataV = ClassStat(ccl.3)
cl.data3 = ClassStat(ccl.v)

#ccl.mat
#image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))

#identify background data is 0
#clb.data = ClassStat(m,bkgd=0), dependign on above result

#########################################################################
#Protected areas
pa <- raster(paste(datwd,"Rasters/PA_npaes_test_web.tif", sep=""))
proj4string(pa) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

#Vegetation map
vm <- raster(paste(datwd,"Rasters/VEG12test_web.tif", sep=""))
proj4string(vm) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

#Stack all rasters
dat <- stack(lc, pa, vm)
nex <- extent(2020000, 2140000, -4100000, -3950000)
x <- crop(dat, nex)