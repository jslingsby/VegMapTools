##########################################
######## R code for setting up vegmap, transformation 
######## and protected area data for VegMapTools analyses
##########################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 30 November 2016
##########################################

##########################################
###1) Get libraries and setwd
##########################################
###Install libraries
install.packages("doSNOW", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)
install.packages("foreach", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)
install.packages("rgdal", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)
install.packages("raster", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)
install.packages("dplyr", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)
install.packages("rgeos", lib="/home/slingsby/Rlib", repos="http://cran.cnr.Berkeley.edu/", dependencies=TRUE)

###Get libraries
library(dplyr, lib.loc="/home/slingsby/Rlib")
library(doSNOW, lib.loc="/home/slingsby/Rlib")
library(foreach, lib.loc="/home/slingsby/Rlib")
library(raster, lib.loc="/home/slingsby/Rlib")
library(rgdal, lib.loc="/home/slingsby/Rlib")
library(rgeos, lib.loc="/home/slingsby/Rlib")


##########################################
###2) Get data
##########################################

setwd("/home/slingsby")

#2012 National veg map rasterized to 30m
vegA <- raster("Rasters/VEG12test_web.tif")
proj4string(vegA) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #EPSG3857 - Web Mercator

#Land cover
lcA <- raster("Rasters/LC13test_web.tif")
proj4string(lcA) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs") #EPSG3857 - Web Mercator

#Total Ecosystem Organic Carbon
teoc <- raster("Rasters/TotalEcosystemOrganicCarbon_g_C_sq.m_DEA_CSIR_1.1.9-2015-10-06.tif")
teoc <- projectRaster(teoc, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))

#lc1990 <- raster(paste(datwd,"Landcover/sa_lcov_1990_gti_utm35n_vs18.tif", sep=""))
#lc1990 <- projectRaster(lc1990, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))

#lc2013 <- raster(paste(datwd,"Landcover/sa_lcov_2013-14_gti_utm35n_vs22b.tif", sep=""))
#lc2013 <- projectRaster(lc2013, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))

reA <- readOGR("Rasters/REEA_OR_2016_Q2.shp", layer = "REEA_OR_2016_Q2")
reA <- spTransform(reA, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
reA <- reA[-which(reA@data$PROJ_STATU == "Withdrawn/Lapsed"),] #Remove "Withdrawn/Lapsed" development proposals
reA <- gBuffer(reA, byid=TRUE, width=0)

#Subset key info
lctab <-lcA@data@attributes[[1]]
vegtab <-vegA@data@attributes[[1]]
retab <- reA@data


##########################################
###3) Set up list of extents
##########################################

#extent(1774750, 3730450, -4152956, -2506556)  #full extent

y <- cbind(seq(-2547716, -4152956, -41160), seq(-2506556, -4111796, -41160)) #Create sequences for 40 rows of tiles
x <- cbind(seq(1774750, 3665260, 65190), seq(1839940, 3730450, 65190))  #Create sequences for 30 columns of tiles

sets <- expand.grid(1:30, 1:40) #Set up indices
nextent <- list() #Set up list for storing extents

for(j in 1:nrow(sets)) #Loop through indices to make extents
{
nextent[[j]] <- c(x[sets[j,1],], y[sets[j,2],])
}

warnings() #catch weird warning?

##########################################
###4) Set up cluster
##########################################
packageVec <- c("raster", "rgdal", "rgeos", "dplyr")

ntasks <- length(nextent)

cl <- makeCluster(ntasks/20, type = "SOCK", outfile="REout.out")
registerDoSNOW(cl)

clusterEvalQ(cl, Sys.setenv(TMPDIR = "/home/slingsby/tmp/"))
clusterEvalQ(cl, Sys.setenv(TMP = "/home/slingsby/tmp"))
clusterEvalQ(cl, Sys.setenv(TEMP = "/home/slingsby/tmp"))
#clusterEvalQ(cl, library(raster)) #, lib.loc="/home/slingsby/Rlib")) #Load necessary libraries on all nodes
#clusterEvalQ(cl, library(rgdal)) #, lib.loc="/home/slingsby/Rlib"))
#clusterEvalQ(cl, library(rgeos)) #, lib.loc="/home/slingsby/Rlib"))
#clusterEvalQ(cl, library(dplyr)) #, lib.loc="/home/slingsby/Rlib"))

###Run loop
out <- foreach(i = 1:ntasks, .packages=packageVec) %dopar% { 

cat(paste("Running task", i, sep=" "))  
    
##########################################
###5) Start loop, set extent, crop, simplify, join and table
##########################################
#Set extent for loop

nex <- extent(nextent[[i]])

#Crop rasters
lc <- crop(lcA, nex)
veg <- crop(vegA, nex)
re <- crop(reA, nex)
tc <- projectRaster(teoc, lc)

#Simplify datasets to key info
lc <- deratify(lc, att="LC3", layer=1, complete=TRUE, drop=F)
lc[which(getValues(lc) %in% c(0, 128))] <- NA #Ocean
lc[which(getValues(lc) %in% c(1:3, 6, 10))] <- 1 #Natural or Near-natural
lc[which(getValues(lc) %in% c(4, 5, 7:9, 11:16))] <- 0 #Transformed

veg <- deratify(veg, att="Value", layer=1, complete=TRUE, drop=F)

#Rasterize renewables layer and cut from landcover if necessary
if(!is.null(re)) {
  cat("There are RE impacts in this extent")
  re <- gBuffer(re, byid=TRUE, width=0)
  re@data <- data.frame(Val = rep(1, length = nrow(re@data)))
  rer <- rasterize(re,lc)
  lcre <- lc
  lcre[getValues(rer)>0] <- 0
  } else {lcre <- lc}

#Stack all rasters, extract to data frame and summarise data by veg type
names(lc) <- "LandCover"
names(veg) <- "VegType"
names(lcre) <- "RE_LandCover"
names(tc) <- "Total_Carbon"

dat <- stack(lc, veg, lcre, tc)
df <- as.data.frame(dat)
cat("Summarizing")
vsum <- summarise(group_by(df, VegType), LandCover=sum(as.numeric(LandCover_LC3)), RE_LandCover=sum(as.numeric(RE_LandCover_LC3)), Total_Carbon=sum(as.numeric(Total_Carbon)))
cat("It worked")
vsum$OriginalExtent <- summary(as.factor(getValues(veg)))
i
return(vsum)
}

#Kill children...  
stopCluster(cl)

##############################################################################
###6) Summarize, save and exit
##############################################################################

cat("Summarizing all results")

dfsum <- do.call(rbind, out)
REsum <- summarise(group_by(dfsum, VegType), LandCover=sum(LandCover), RE_LandCover=sum(RE_LandCover), Total_Carbon=sum(as.numeric(Total_Carbon)), OriginalExtent=sum(OriginalExtent))

save(i, REsum, vegtab, file="RE.Rdata")
quit(save="no")


