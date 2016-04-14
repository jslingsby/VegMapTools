##########################################
######## R code for setting up climate and 
######## elevation data for VegMapTools analyses
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 13 April 2016
##########################################

##########################################
###1) Get libraries, setwd and get data
##########################################
library(raster)
library(gdalUtils)
library(rgdal)
library(dplyr)

##########################################
###2) Get and process data
##########################################

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

write.csv(futureInfo, "Data/futureclimate_Info.csv")
writeRaster(fclim, "Data/futureclimate_plus_Current.grd")

# Get veg age/return interval data (including projections from multiple models)
if(!file.exists("Data/futureages.grd")) {fage <- stack(list.files("/Users/jasper/Documents/Papers/PhD/Fynbos literature/Wilson 2015 PNAS tifs/ages", full.names=T, pattern = ".tif"))
writeRaster(fage, "Data/futureages.grd")} else {fage <- brick("Data/futureages.grd")}

