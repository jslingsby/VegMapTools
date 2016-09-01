##############################################################################
######## Script to fiddle with Ecosystem protection levels
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 30 May 2016
##############################################################################
######## Notes: Code/data need fixing!!! Protection level and remaining extent underestimated or NA because of extra conditions in data table - e.g. "(+0.6%)" or "see text"
##############################################################################

library(rgdal)
library(dplyr)

#Get 2006 vegmap shapefile
veg <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/VM2006/vegmap2006_geo.shp", layer = "vegmap2006_geo")

#Get data from attribute table
vdat <- veg@data

#Get hectares protected of each polygon (excluding stuff in brackets etc)
vdat$PL <- (vdat$POLYSQKM*as.numeric(sapply(as.character(vdat$PROTCTD), FUN = function(x) {strsplit(x, split="\\%")[[1]][1]})))/vdat$VTYPESQKM

#Get remaining extent in hectares of each polygon (excluding stuff in brackets etc)
vdat$RE <- (vdat$POLYSQKM*as.numeric(gsub("\\%", "", as.character(vdat$REMAINING))))/vdat$VTYPESQKM

#Summarise data by veg type
#vsum <- summarise(group_by(vdat, NAME), PL=sum(PL, na.rm=T), RE=sum(RE, na.rm=T))
vsum <- summarise(group_by(vdat, NAME), PL=sum(PL), RE=sum(RE))

#Get ancillary info and add to vegtype summary
adat <- unique(vdat[,which(colnames(vdat)%in%c("NAME", "CONSTRGT", "BIOME", "CNSRVTNSTT", "VTYPESQKM"))])

dat <- merge(vsum, adat) #Merge data

dat$CONSTRGT <- as.numeric(gsub("\\%", "", as.character(dat$CONSTRGT))) #Make conservation target numeric

dat <- na.omit(dat)

#How much is left to lose until we are below target
dat$ToLoseCT <- dat$RE - dat$CONSTRGT
dat$ToLose20 <- dat$RE - 20

#How much is left to lose until we are below target
dat$ToProtectCT <- dat$CONSTRGT - dat$PL
dat$ToProtect20 <- 20 - dat$PL

dat$Ratio <- dat$ToProtectCT/dat$ToLoseCT
pairs(dat[,which(colnames(dat)%in%c("Ratio","PL","RE"))])

#Subset
datP <- dat[which(dat$PL>dat$CONSTRGT),] #Reached target
datF <- dat[which(dat$PL<dat$CONSTRGT & dat$RE<dat$CONSTRGT),]  #can't reach target
datG  <- dat[which(dat$PL<dat$CONSTRGT & dat$RE>dat$CONSTRGT),]  #can reach target

pairs(datG[,which(colnames(datG)%in%c("Ratio","PL","RE"))])

#Calculate slopes
dat$PL ~ slope*dat$RE + 20

#slope = dat$PL/dat$RE - 20

#Plotting
ggplot(dat, aes(x = RE, y = PL, size = VTYPESQKM, colour = CNSRVTNSTT)) + geom_point() + facet_wrap(~BIOME)

ggplot(dat, aes(x = ToLoseCT, y = ToProtectCT, size = VTYPESQKM, colour = CNSRVTNSTT)) + geom_point() + facet_wrap(~BIOME)


#as.data.frame(vsum[which(vsum$RE<20),])
