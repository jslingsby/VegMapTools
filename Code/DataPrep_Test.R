##########################################
######## R code for setting up vegmap, transformation 
######## and protected area data for VegMapTools analyses
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 12 April 2016
##########################################

##########################################
###1) Get libraries and setwd
##########################################
library(raster)
library(gdalUtils)
library(rgdal)
library(dplyr)

if(Sys.getenv("USER")=="jasper") {datwd <- "/Users/jasper/Documents/GIS/VegToolsRaw/"}
if(Sys.getenv("USERNAME")=="MatlalaM") {datwd <- "Your data directory"}

##########################################
###2) Get and process data
##########################################

###Set desired extent
nex <- extent(-320000, -220000, 6150000, 6250000)

###Land cover - get, crop and write out, read in
lc <- raster(paste(datwd, "Landcover/LCsmiso_trans1.tif", sep=""))
lc
crop(lc, nex, filename = "Data/CTtest.grd", overwrite=T)
lc <- raster("Data/CTtest.grd")

lcC <- lc #Set up a landcover with 1 for natural and NA for everything else...
lcC[which(getValues(lc)==2)] <- NA

###VegMap 2006
vm2006 <- readOGR(paste(datwd, "VM2006", sep=""), layer = "vegmap2006_geo") #Get data
vm2006 <- spTransform(vm2006, CRS(proj4string(lc))) #reproject to UTM35S
vm2006 <- crop(vm2006, nex) #crop

#fyn2006 <- vm2006[which(vm2006$BIOME=="Fynbos Biome"),] #subset by biome etc
#?intersect
#?extract
#?gdal_rasterize

rasterize(vm2006, lc, filename="Data/vegtype.grd", overwrite=T) #Rasterize  #, field="NAME"
vt <- raster("Data/vegtype.grd") #Read in rasterized veg types
vtnames <- levels(vt)[[1]]
#vtnames <- levels(vm2006$NAME)[levels(vt)[[1]]$ID]
#levels(vt) <- data.frame(ID = levels(vt)[[1]]$ID, NAMES = vtnames)
rm(vm2006) #Remove any large objects that aren't being used from memory...

###Identify unique fragments
#?gdal_cmd_builder
#gdal_sieve.py - http://www.gdal.org/gdal_sieve.html
#gdal_cmd_builder("gdal_sieve.py")

x <- !is.na(vt) * lcC #Trim off "Natural" ocean...
x[which(getValues(x)==0)] <- NA #Make zeros NA

if(!file.exists(fn <- "Data/clumps.grd")) { #Calculate clump numbers
  clumps <- clump(x, directions=8, filename=fn, overwrite=T)
} else {
  clumps <- raster(fn)
}

#Visualize clumps?
x <- clumps
x[which(getValues(x)>1)] <- NA
plot(x, col="red")
plot(lcC, add=T, col= "blue", alpha=0.5)

###Protected Areas
pas <- readOGR(paste(datwd, "PA", sep=""), "PA14_terUTM34s")
pas <- spTransform(pas, CRS(proj4string(lc))) #reproject to UTM35S
pas <- crop(pas, nex) #crop
rasterize(pas, lc, filename="Data/protected.grd", overwrite=T) #rasterize
pas <- raster("Data/protected.grd") #read in raster

###Make a data table

#One way to make a table - slower, but this way checks that the extent, coordinate reference system and resolution are the same
trainingbrick <- brick(vt, lc, clumps, pas)
names(trainingbrick) <- c("VegType", "LandCover", "Fragment", "Protected")
tb <- data.frame(getValues(trainingbrick))

#Another way, but this just checks that they have the same number of cells (!!!NB!!! It does not check extent, resolution or CRS)
dat <- data.frame(VegType = getValues(vt), LandCover = getValues(lc), Fragment = getValues(clumps), Protected = getValues(pas))

#Check that they are the same
identical(tb, dat) #Not the same!!! But why? - Let's look at the objects more closely

class(tb) #Are they the same class?
class(dat)
sapply(dat, class) #Are the columns in each data object the same class?
sapply(tb, class) #NO!? 

dat$LandCover <- as.numeric(dat$LandCover) #Let's make those integer columns numeric
dat$Fragment <- as.numeric(dat$Fragment)

identical(tb, dat) #Yay!!!

rm(tb) #Remove one of the tables from memory - to help R think

dat <- dat[!is.na(dat$VegType),] #Remove any pixels that dont have a veg type...
dat$LandCover[which(dat$LandCover==2)] <- 0 #set landcover data so that only "Natural" has 1 and all else is NA or 0

dat$VegType <- vtnames$NAME[dat$VegType]
levels(dat$VegType)

#Make dat a special table that library(dplyr) works with
dtbl <- tbl_df(dat)

###Calculate veg type extents - using dplyr queries
original <- summarise(group_by(dtbl, VegType), original = (length(LandCover)*900)/10000)
remaining <- summarise(group_by(dtbl, VegType), remaining = (sum(LandCover)*900)/10000)
protected <- summarise(group_by(filter(dtbl, Protected>0) , VegType), protected = (sum(LandCover)*900)/10000)


#fragments? - max size, # clumps, etc?

###Join extents and calculate threat status and protection level
d <- left_join(original, remaining, by="VegType")
d <- left_join(d, protected, by="VegType")
d$protected[is.na(d$protected)] <-0 #Make the veg types that are NA (i.e. have no protected cells) 0
d$perc_remaining <- 100*d$remaining/d$original
d$perc_protected <- 100*d$protected/d$original
d$threat_status <- cut(d$perc_remaining, breaks=c(0,20,35,60,100), labels = c("CR", "EN", "VU", "LC"))
d$protection_level <- cut(d$perc_protected, c(-1,4,10,20,100), labels = c("Hardly", "Poorly", "Moderately", "Well"))

########END########

###Elevation
# Mosaic the 90m SRTM digital elevation model for South Africa
system.time( #Time the process
if(!file.exists("Data/dem_SRTM90.tif")) {
mosaic_rasters(list.files("/Users/jasper/Documents/GIS/SRTM/All", full.names=T, pattern = ".tif"), "Data/dem_SRTM90.tif", output.vrt = NULL, output_Raster = TRUE, trim_margins = NULL, verbose = FALSE)
} else {dem <- raster("Data/dem_SRTM90.tif")}
)

###Climate
# Get "current" climate data from Schulze et al 2007 - vars include map, mmp01, pptconc, tmax01, tmin07
if(!file.exists("Data/currentclimate.grd")) {

grids <- c("/Users/jasper/Documents/GIS/Schultze (2007)/grids/gmap", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/gmednrfl1", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/tmaxave01c", "/Users/jasper/Documents/GIS/Schultze (2007)/grids/tminave07c") #The paths to the files we need
gridsn <- sapply(grids, FUN = function(x){strsplit(x, "/")[[1]][8]}) #Extract the file names
  
  #Align all rasters to the same extent, grid, projection...
  if(!file.exists("Data/gmap.tif")) {
  for (i in 1:length(grids))
    {
      align_rasters(grids[i], grids[1], paste("Data/", gridsn[i], ".tif", sep=""), output_Raster = TRUE, nThreads = 2, verbose = FALSE)
    }
  } else {clim <- stack(paste("Data/", gridsn, ".tif", sep=""))}

  #Get and rasterize pptcon to the same grid
  if(!file.exists("Data/pptcon.grd")) {
  pptcon <- readOGR(dsn = "/Users/jasper/Documents/GIS/Schultze (2007)/shape_files/rfl_seasconc.shp", layer = "rfl_seasconc")
  rasterize(pptcon, clim, field = "RFL_CONC", filename = "Data/pptcon.grd")} else {
  pptcon <- raster("Data/pptcon.grd")
  }

  #Bind the aligned rasters together, rename and write to file
  clim <- stack(clim, pptcon)
  names(clim) <- c("map", "mmp01", "tmax01", "tmin07", "pptconc") #Use names that match the futures names
  writeRaster(clim, "Data/currentclimate.grd")
} else {
clim <- brick("Data/currentclimate.grd") #Otherwise, if the file does exist, just read it in as a RasterBrick
}
#rm(clim, grids, gridsn, pptcon)

# Get "future" climate data (including projections from multiple models) from Wilson et al. 2015
if(!file.exists("Data/futureclimate.grd")) {fclim <- stack(list.files("/Users/jasper/Documents/Papers/PhD/Fynbos literature/Wilson 2015 PNAS tifs", full.names=T, pattern = ".tif"))
projectRaster(fclim, clim)
fclim_SD <- subset(fclim, seq(2,440,2))
fclim <- subset(fclim, seq(1,439,2))
writeRaster(fclim, "Data/futureclimate.grd")
writeRaster(fclim_SD, "Data/futureclimate_SD.grd")} else {
  fclim <- brick("Data/futureclimate.grd")
}
#object.size(fclim)
#rm(fclim, fclim_SD)

#Get and format metadata about the futures
futureInfo <- as.data.frame(matrix(unlist(strsplit(names(fclim), "_")), nlayers(fclim), 4, byrow=T, dimnames=list(1:nlayers(fclim), c("Scenario", "Model", "Period", "Variable"))), stringsAsFactors = F)
futureInfo$Variable <- sapply(futureInfo$Variable, function(x) {strsplit(x, ".", fixed=T)[[1]][1]})

for (i in 1:nlayers(clim))
{
fclim[[which(futureInfo$Variable==names(clim)[i])]] <- clim[[i]] + fclim[[which(futureInfo$Variable==names(clim)[i])]]
}

# Get veg age/return interval data (including projections from multiple models)
if(!file.exists("Data/futureages.grd")) {fage <- stack(list.files("/Users/jasper/Documents/Papers/PhD/Fynbos literature/Wilson 2015 PNAS tifs/ages", full.names=T, pattern = ".tif"))
writeRaster(fage, "Data/futureages.grd")} else {fage <- brick("Data/futureages.grd")}

