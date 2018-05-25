rm(list=ls())
library(vmstools)
library(rgdal)
library(data.table)
library(sp)
library(sf)
library(raster)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"C:/Users/jepol/Desktop/N2000/Germany_2018/"
resPath     <- paste0(sysPath,"Results/")
SpatialPath <- paste0(sysPath, "Spatial")
intermediatePath <- paste0(sysPath,"int/")

intermediatePath_historic <- paste0(sysPath,"int/historic/")


dir.create(SpatialPath, showWarnings = FALSE)

#Define gears used in workflow
Dredge <- c("DRB", "DRO", "DRC", "BMS")
BottomTrwl <- c("OTB", "OTT", "PTB", "TBN", "TBS")
BeamTrwl <- "TBB"
PelagicTrwl <- c("OTM", "PTM")
Lines <- c("LH", "LHP", "LL", "LLD", "LLS", "LX")
Traps <- c("FPO", "FYK", "FPN")
Nets <- c("GTR", "GNS", "GND", "GN")
AnchoredSeine <- "SDN"
FlyShootingSeine <- "SSC"
PurseSeine <- "PS"
BeamTrwl_TBC <- "TBC"



### 2012-2017

dat <- readRDS(paste0(intermediatePath, "Measuredata.rds"))

dat[Gear%in% c(Dredge, BottomTrwl,BeamTrwl,  AnchoredSeine, FlyShootingSeine), GearGroup:="Bundslæbende"]
dat[Gear %in% c(Nets), GearGroup:="Garn"]
dat[Gear %in% c(BeamTrwl_TBC), GearGroup:="Crangon"]
dat[is.na(GearGroup), GearGroup:="Andre redskaber"]

## Make summary table 2012-2017

m <- dat[,.("Fartøjer"= sum(NoVessels), "Effort (timer)"= sum(Effort_hrs, na.rm=T),  "Landinger (kg)"=sum(LE_KG_TOT, na.rm = T), "Landinger (dkk)"=sum(LE_EURO_TOT, na.rm = T)*7.45, "Landinger (euro)"=sum(LE_EURO_TOT, na.rm = T)), by=.(År=Year, Område=Scenario, GearGroup)]

m <- melt(m, id.vars=c("År", "Område", "GearGroup"))

out <- dcast(m, Område+variable+GearGroup~År, value.var = "value")
setorder(out, Område, GearGroup)

fwrite(out, paste0(resPath, "Measures.csv"))

### 2005-2017

dat <- readRDS(paste0(intermediatePath_historic, "Measuredata.rds"))

dat[Gear%in% c(Dredge, BottomTrwl,BeamTrwl,  AnchoredSeine, FlyShootingSeine), GearGroup:="Bundslæbende"]
dat[Gear %in% c(Nets), GearGroup:="Garn"]
dat[Gear %in% c(BeamTrwl_TBC), GearGroup:="Crangon"]
dat[is.na(GearGroup), GearGroup:="Andre redskaber"]

## Make summary table 2005-2017

m <- dat[,.("Fartøjer"= sum(NoVessels), "Effort (timer)"= sum(Effort_hrs, na.rm=T),  "Landinger (kg)"=sum(LE_KG_TOT, na.rm = T), "Landinger (dkk)"=sum(LE_EURO_TOT, na.rm = T)*7.45, "Landinger (euro)"=sum(LE_EURO_TOT, na.rm = T)), by=.(År=Year, Område=Scenario)]

m <- melt(m, id.vars=c("År", "Område"))

out <- dcast(m, Område+variable~År, value.var = "value")
setorder(out, Område)

fwrite(out, paste0(resPath, "Measures_historic.csv"))


m5 <- dat[Scenario=="Measure 5"]

#Find top 10 most valuable species
rate <-  melt(m5, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
              measure.vars = c(grep("LE_EURO", colnames(m5), value = T)))
rate <- rate[, sum(value), by=variable]
setorder(rate, -V1)
keep <- gsub("LE_EURO_", "", as.character(rate$variable[2:6]))
keep <- as.character(rate$variable[2:6])

op <- melt(m5, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
           measure.vars = grep(paste0(keep, collapse="|"), colnames(m5), value = T))


n <- op[,.(EURO=sum(value)), by=.(År=Year, Område=Scenario, Species=gsub("LE_EURO_", "", as.character(variable)))]

out2 <- dcast(n, Område+Species~År, value.var = "EURO")
fwrite(out2, paste0(resPath, "Species_M5.csv"))


###### Make Shapefiles from points data #########

#Define projections
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
utm32 <- CRS("+proj=utm +zone=32+datum=WGS84")

p <- readRDS(paste0(intermediatePath, "Points.rds"))

p[LE_GEAR%in% c(Dredge, BottomTrwl,BeamTrwl,  AnchoredSeine, FlyShootingSeine), GearGroup:="Bundslæbende"]
p[LE_GEAR %in% c(Nets), GearGroup:="Garn"]
p[is.na(GearGroup), GearGroup:="Andre redskaber"]

p$LE_KR_TOT <- p$LE_EURO_TOT*7.45

b <-    p[GearGroup=="Bundslæbende"]
g <-    p[GearGroup=="Garn"]
a <-    p[GearGroup=="Andre redskaber"]
tbc <-  p[LE_GEAR=="TBC"]

coordinates(p) <- ~SI_LONG+SI_LATI
proj4string(p) <-  latlong
writeOGR(p, dsn=SpatialPath, layer="Entire", driver = "ESRI Shapefile")
###
coordinates(b) <- ~SI_LONG+SI_LATI
proj4string(b) <-  latlong
writeOGR(b, dsn=SpatialPath, layer="Bund", driver = "ESRI Shapefile")
###
coordinates(g) <- ~SI_LONG+SI_LATI
proj4string(g) <-  latlong
writeOGR(g, dsn=SpatialPath, layer="Garn", driver = "ESRI Shapefile")
###
coordinates(a) <- ~SI_LONG+SI_LATI
proj4string(a) <-  latlong
writeOGR(a, dsn=SpatialPath, layer="Andre", driver = "ESRI Shapefile")

###
coordinates(tbc) <- ~SI_LONG+SI_LATI
proj4string(tbc) <-  latlong
writeOGR(tbc, dsn=SpatialPath, layer="TBC", driver = "ESRI Shapefile")

##### Make raster files that sums the landings in a 1x1 km grid

##### Garn
pts <- spTransform(g, utm32)
rast <- raster()
extent(rast) <- extent(pts) # this might be unnecessary
proj4string(rast) <- utm32
res(rast) <- c(1000,1000)
rast2 <- rasterize(pts, rast, pts$LE_KR_TOT, fun=sum) 

writeRaster(rast2, paste0(SpatialPath, "/GarnGRID"), format = "GTiff")

##### Andre
pts <- spTransform(a, utm32)
rast <- raster()
extent(rast) <- extent(pts)
proj4string(rast) <- utm32
res(rast) <- c(1000,1000)
rast2 <- rasterize(pts, rast, pts$LE_KR_TOT, fun=sum) 

writeRaster(rast2, paste0(SpatialPath, "/AndreGRID"), format = "GTiff")


##### Bund
pts <- spTransform(b, utm32)
rast <- raster()
extent(rast) <- extent(pts) 
proj4string(rast) <- utm32
res(rast) <- c(1000,1000)
rast2 <- rasterize(pts, rast, pts$LE_KR_TOT, fun=sum) 

writeRaster(rast2, paste0(SpatialPath, "/BundGRID"), format = "GTiff")

##### TBC
pts <- spTransform(tbc, utm32)

rast <- raster()
extent(rast) <- extent(pts)
proj4string(rast) <- utm32
res(rast) <- c(1000,1000)
rast2 <- rasterize(pts, rast, pts$LE_KR_TOT, fun=sum) 

writeRaster(rast2, paste0(SpatialPath, "/tbcGRID"), format = "GTiff", overwrite=T)


##### Alle
pts <- spTransform(p, utm32)

rast <- raster()
extent(rast) <- extent(pts)
proj4string(rast) <- utm32
res(rast) <- c(1000,1000)
rast2 <- rasterize(pts, rast, pts$LE_KR_TOT, fun=sum) 

writeRaster(rast2, paste0(SpatialPath, "/EntireGRID"), format = "GTiff")
