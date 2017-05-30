##############################################################################
######## Exploring the SANBI herbarium specimen data
##############################################################################
######## Compiled by Jasper Slingsby 2017
######## Last edited: 29 May 2017
##############################################################################

### Get libraries
library(gdata)
library(ggmap)
library(raster)
library(rgdal)
library(dplyr)
#library(reshape2)
#library(ggplot2)
#library(scales)
#library(animation)

##############################################################################
### Set up working directories and get data
##############################################################################

if(Sys.getenv("USER")=="jasper"){datwd <- "/Users/jasper/Documents/Databases/POSAoffline/"}
if(Sys.getenv("USERNAME")=="nicole"){datwd <- "/Users/jasper/Documents/Databases/NVD/NVD_16_09_2016/"}

dat <- read.csv(paste0(datwd,"NewPOSA_9May2017.csv"), stringsAsFactors = F)
dat <- trim(dat)

##############################################################################
### Subset to the data we want
##############################################################################

#sort(unique(dat[which(dat$Family == "Proteaceae"),]$Taxon))
pdat <- dat[which(dat$Taxon=="Protea repens (L.) L."),] #Select only rows for focal species

#gdat <- dat[which(dat$Family=="Poaceae"),] #Get one family (grasses in this case)

##############################################################################
### Explore the data
##############################################################################

sort(summary(as.factor(pdat$QDS))) #Look at number of collections per QDS

qdat <- pdat[which(pdat$QDS %in% names(which(summary(as.factor(pdat$QDS))>4))),] #Get all QDS with >5 collections

qdat <- qdat[order(qdat$QDS, qdat$CollectionYear),]

write.csv(qdat, paste0(datwd, "prepens.csv"), row.names = F)

#sort(pdat[which(pdat$QDS == "3318DD"),]$CollectionYear) # get individual QDS

