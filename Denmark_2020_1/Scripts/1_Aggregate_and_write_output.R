### Use combined Logbook and VMS files, either from the supllied code "0_CombineTacsatAndEflalo" or your countrys own method for combining. 

### The output consist of 8 csv files, which will be generated in the Results folder:
### - Two files with landings, no. of vessels and effort hours per defined areas 1 and 2, year, month and gear  
### - One file with landings, no. of vessels and effort hours per ICES square , year, month and gear  
### - Two files with no. of vessels per defined areas 1 and 2 and gear catagory (mobile bottom contacting gears / other gears)
### - One file with no. of vessels per ICES square and gear catagory (mobile bottom contacting gears / other gears)
### - Two files with all pings, where fishing is assumed, within the defined areas 1 and 2. 
###   This file holds information on position, year, month, gear, speed and area

### Please respond to the datacall with a zipped file attached/downloadable consisting these eight csv files. 

rm(list=ls())
library(data.table)
library(sf)
library(mapview)
library(vmstools)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"Q:/dfad/users/jepol/home/GitHub/datacallsVMSLogbook/Denmark_2020_1/" # Please change accordingly
RdataPath   <- paste0(sysPath,"Rdata/") ## Put cleaned tacsat and eflalo files in this folder
dataPath    <- paste0(sysPath,"Data/")  ## Put file speedarr.csv in this folder
resPath     <- paste0(sysPath,"Results/")  ## Output will generated here

dir.create(resPath, showWarnings = FALSE)

FlagCountry <- "DK" # Change to your own country code

YearsToSubmit <- 2015:2019 # If your country havent processed 2019 yet, please change to 2014:2018

################## Load all pings inside the defined areas for the whole period ###################
dat <- rbindlist(lapply(paste0(RdataPath, "TacsatEflalo_", YearsToSubmit, ".rds"), readRDS), fill = T)
dat[, LE_YEAR := year(SI_DATIM)] #Add year
dat[, FlagCountry := FlagCountry] #Add Your country code
### Load and inspect polygons
shp1 <- readRDS(paste0(RdataPath, "shp1.rds"))
mapview(shp1)

shp2 <- readRDS(paste0(RdataPath, "shp2.rds"))
mapview(shp2)

#Remove species columns with sum 0
zeroes <- names(colSums(Filter(is.numeric, dat))
                [colSums(Filter(is.numeric, dat))==0])
try(dat[,(zeroes):=NULL], silent = T)

#Add ices square to data
dat$Square <- ICESrectangle(data.frame(SI_LONG = dat$SI_LONG,
                                      SI_LATI = dat$SI_LATI))

ns_squares <- c("39F4", "39F5", "39F6", "39F7", "39F8", "40F3", "40F4", "40F5", "40F6", "40F7", 
                "40F8", "41F3", "41F4", "41F5", "41F6", "41F7", "41F8", "42F5", "42F6", "42F7",
                "42F8", "43F6", "43F7", "43F8", "43F9", "44F8", "44F9", "44G0", "45F9", "45G0")

bs_squares <- c("38G3", "38G4", "38G5", "39G4", "39G5", "39G6", "40G4", "40G5")

#Only keep pings within these Ices Squares
dat <- dat[Square %in% c(ns_squares, bs_squares)]


################### Process shp1 ################### 

dat$lon <- dat$SI_LONG
dat$lat <- dat$SI_LATI

#Add Name and Region to TacsatEflalo
pts1 <- dat %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)

joined1 <- st_join(pts1, shp1, join = st_intersects)

dat1 <- data.table(joined1)

dat1[, geometry:=NULL]

# Make a subset with only data from within the defined ICES squares
dat1a <- dat1[!is.na(Name)]

#######Aggregate data and write to results folder################

#Per ICES square
out1 <- dat1[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
             by=.(LE_YEAR, Square, LE_GEAR, FlagCountry),
             .SDcols=grepl("LE_KG|LE_EURO", names(dat1))]

fwrite(out1, paste0(resPath, "landings_Square_", FlagCountry, ".csv"))

#Per defined area
out1a <- dat1a[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
             by=.(LE_YEAR, Region, Name, LE_GEAR, FlagCountry),
             .SDcols=grepl("LE_KG|LE_EURO", names(dat1))]

fwrite(out1a, paste0(resPath, "landings_RegionName1_", FlagCountry, ".csv"))


# Check if all your mobile bottom contacting gears are withing this list. 
# If not, please add the missing ones:
Bottom_Trawls <- c("TBB", "OTB", "PTB", "OTT", "TB", "TBN", "TBS", "SB", 
                   "SDN", "SSC", "SPR", "SX", "SV", "DRB", "DRH", "HMD")
sort(unique(dat1$LE_GEAR))

#Add column GearGroup to data
dat1[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]
dat1a[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]

#Find unique number of vessels, per year for mobile bottom gears 
#and other gears and write it to result folder
nov_square1 <- dat1[, .(NoVessels=uniqueN(VE_REF)),
                   by=.(LE_YEAR, Square, GearGroup, FlagCountry)]

nov_RegionName1 <- dat1a[, .(NoVessels=uniqueN(VE_REF)),
                       by=.(LE_YEAR, Region, Name, GearGroup, FlagCountry)]

fwrite(nov_square1, paste0(resPath, "NumberOfVessels_Square_", FlagCountry,  ".csv"))
fwrite(nov_RegionName1, paste0(resPath, "NumberOfVessels_RegionName1_", FlagCountry,  ".csv"))

# Save pings within the defined areas with data on gear, year, position, speed and area 
pings1 <- dat1a[, c("LE_YEAR", "LE_GEAR", "SI_LONG", "SI_LATI", "SI_SP", "Square", "Name", "Region", "FlagCountry")]
fwrite(pings1, paste0(resPath, "pings1_", FlagCountry, ".csv"))


################### Process shp2 ################### 

## Note that some polygons are overlapping, so there will be pings that are occuring more than one time ##

#Add Name and Region to TacsatEflalo
pts2 <- dat %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326)

joined2 <- st_join(pts2, shp2, join = st_intersects)

dat2 <- data.table(joined2)

dat2[, geometry:=NULL]

# Make a subset with only data from within the defined areas
dat2a <- dat2[!is.na(Name)]

#######Aggregate data and write to results folder################

#Per defined area
out2a <- dat2a[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
               by=.(LE_YEAR, Region, Name, LE_GEAR, FlagCountry),
               .SDcols=grepl("LE_KG|LE_EURO", names(dat2))]

fwrite(out2a, paste0(resPath, "landings_RegionName2_", FlagCountry, ".csv"))


# Check if all your mobile bottom contacting gears are withing this list. 
# If not, please add the missing ones:
Bottom_Trawls <- c("TBB", "OTB", "PTB", "OTT", "TB", "TBN", "TBS", "SB", 
                   "SDN", "SSC", "SPR", "SX", "SV", "DRB", "DRH", "HMD")
sort(unique(dat2$LE_GEAR))

#Add column GearGroup to data
dat2[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]
dat2a[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]

#Find unique number of vessels, per year for mobile bottom gears 
#and other gears and write it to result folder

nov_RegionName2 <- dat2a[, .(NoVessels=uniqueN(VE_REF)),
                         by=.(LE_YEAR, Region, Name, GearGroup, FlagCountry)]

fwrite(nov_RegionName2, paste0(resPath, "NumberOfVessels_RegionName2_", FlagCountry,  ".csv"))

# Save pings within the defined areas with data on gear, year, position, speed and area 
pings2 <- dat2a[, c("LE_YEAR", "LE_GEAR", "SI_LONG", "SI_LATI", "SI_SP", "Square", "Name", "Region", "FlagCountry")]
fwrite(pings2, paste0(resPath, "pings2_", FlagCountry, ".csv"))



############################################################################################################
############################### Please take a moment to review your results.################################ 
#For example compare the cod landings in the selected ices squares from VMS and from logbooks#################
############################################################################################################


################## Load logbook data for the whole period ###################
m_list3 <- list.files(path = RdataPath, pattern= "cleanEflalo_", full.names = T)
dat3 <- data.table()
for(y in YearsToSubmit){
  eflalo <- readRDS(paste0(RdataPath, "cleanEflalo_", y, ".rds"))
  eflalo$LE_YEAR <- y
  dat3 <- rbindlist(list(dat3, eflalo), fill = T)
}

#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
for (i in seq_along(dat3)) set(dat3, i=which(is.na(dat3[[i]])), j=i, value=0)

#Compare data from combined VMS and logbooks with the logbooks alone.
#If there is big differences, something has gone wrong.
vmscod <- dat1[,.(LE_KG_COD_vms = sum(LE_KG_COD, na.rm = T)), by=.(LE_RECT = Square, LE_YEAR)]
logcod <- dat3[,.(LE_KG_COD_log = sum(LE_KG_COD, na.rm = T)), by=.(LE_RECT, LE_YEAR)]


codcompare <- merge(vmscod, logcod)

options("scipen"=50, "digits"=0)

View(codcompare)



