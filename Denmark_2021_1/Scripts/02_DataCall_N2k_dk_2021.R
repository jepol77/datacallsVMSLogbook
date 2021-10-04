### Extract data from all Scenarios according to the lookup table ###

# Before running this code the following needs to be done:
### - Cleaned tacsatEflalo files for all the years should be in the RdataPath. Each file 
###   should be saved as tacsatEflalo_"Year".rds (eg. tacsatEflalo_2019.rds)
###   These files  should be in the standard tacsatEflalo format and contain merged kg and EURO for all
###   species. This can be obtained using the contrys own procesdure or using 
###   the 0_CreateTacsatEflalo.R script. 
### - Furthermore the Cleaned tacsatEflalo should contain these columns:
###   "VE_REF", "SI_LATI", "SI_LONG", "SI_DATE", "SI_TIME", "SI_SP", "SI_HE", "SI_HARB", "SI_STATE", "SI_FT", "SI_DATIM", "INTV", "FT_REF"
### - Cleaned eflalo files should be in the RdataPath, saved as cleanEflalo_"Year".rds (eg. cleanEflalo_2019.rds)

rm(list=ls())
library(vmstools)
library(data.table)
library(beepr)
library(sf)
library(mapview)
library(tibble)

#Add you country code:

Country <- "DK"

#- Set the working directory to the folder where you keep your code and data
sysPath       <- "Q:/dfad/users/jepol/home/N2000/Projects/Denmark/"
RdataPath     <- "Q:/dfad/users/jepol/home/N2000/Rdata/"
dataPath      <- paste0(sysPath,"Data/")
polygonPath   <- paste0(sysPath,"shapes/")
intermediatePath <- paste0(sysPath,"int/")
resPath       <- paste0(sysPath,"Results/")

# If directories not exist, create them
dir.create(sysPath, showWarnings = FALSE)
dir.create(dataPath, showWarnings = FALSE)
dir.create(polygonPath, showWarnings = FALSE)
dir.create(intermediatePath, showWarnings = FALSE)
dir.create(resPath, showWarnings = FALSE)

#Load and inspect N2000 areas
shp <- st_read(paste0(polygonPath, "shp.shp"))
mapview(shp)

# Delete files in intermediatePath (if you run the script after editing)
lst <- list.files(intermediatePath, full.names = T)
file.remove(lst)

#Extract data if 2018 is not yet processed, this should be changed to 2017.
for (Year in c(2019:2020)) {
  print(paste(Sys.time(), Year))
  
  tacsatEflalo <- readRDS(paste0(RdataPath,"tacsatEflalo_",Year,".rds"))
  eflalo <- readRDS(paste0(RdataPath,"cleanEflalo_",Year,".rds"))
  
  #If data is saved as .RData, use this instead
  # load(file=file.path(paste(RdataPath,"tacsatEflalo_",Year,".RData",sep="")))
  # load(file.path(paste(RdataPath,"cleanEflalo_",Year,".RData",sep="")))
  
  # # Add relevant information to tacsatEflalo
  tacsatEflalo$LE_RECT  <- ICESrectangle(tacsatEflalo)
  tacsatEflalo$LE_GEAR  <- eflalo$LE_GEAR[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_MONTH <- as.integer(month(tacsatEflalo$SI_DATIM))
  
  # Make data.frames into data.tables for faster processing
  te <- data.table(tacsatEflalo); gc()
  e <- data.table(eflalo)
  te$LE_YEAR <- Year
  e$LE_YEAR <- Year
 
  # Calculate total kg and EURO if not already present in data as LE_KG_TOT and LE_EURO_TOT
  if(!"LE_KG_TOT" %in% colnames(te))
    te[,LE_KG_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("KG", names(te))]
  if(!"LE_EURO_TOT" %in% colnames(te))
    te[,LE_EURO_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("EURO", names(te))]
  
  if(!"LE_KG_TOT" %in% colnames(e))
    e[,LE_KG_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("KG", names(e))]
  if(!"LE_EURO_TOT" %in% colnames(e))
    e[,LE_EURO_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("EURO", names(e))]
  
  #Only preserve relevant squares
  sq <- c("42F6", "42F7", "43F6", "43F7", "43F8", "43F9","44F8", "44F9")
  
  #Write squares of interest to intermediate files from both eflalo and tacsateflalo
  saveRDS(e[e$LE_RECT %in% sq,], paste0(intermediatePath, "eflalo", Year, ".rds"))
  saveRDS(te[te$LE_RECT %in% sq,], paste0(intermediatePath, "te", Year, ".rds"))
  
  #Make tacsateflalo into spatial points using sf
  p_te <- te %>% 
    sf::st_as_sf(coords = c("SI_LONG","SI_LATI")) %>% 
    sf::st_set_crs(4326)
  
  #Add N2k area to file and save only points inside those areas
  sub <- st_join(p_te, shp, join = st_intersects)
  sub <- sub[!is.na(sub$N2k),]
  saveRDS(sub, paste0(intermediatePath, "N2k", Year, ".rds"))
}

################## Load all pings inside the N2k areas for the whole period ###################
m_list1 <- list.files(path = intermediatePath, pattern= "N2k", full.names = T)
dat1 <- rbindlist(lapply(m_list1, readRDS), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
dat1[is.na(dat1)] = 0

#Remove species columns with sum 0
zeroes <- names(colSums(Filter(is.numeric, dat1))
                [colSums(Filter(is.numeric, dat1))==0])
try(dat1[,(zeroes):=NULL], silent = T)

dat1$LE_MONTH <- month(dat1$SI_DATIM)

#Aggregate data and write to results folder
out1 <- dat1[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
           by=.(LE_YEAR, LE_MONTH, N2k, LE_GEAR),
           .SDcols=grepl("LE_KG|LE_EURO", names(dat1))]

setorder(out1, LE_YEAR, LE_MONTH, N2k, LE_GEAR)

fwrite(out1, paste0(resPath, "landings_N2k_", Country, ".csv"))

# Check if all your mobile bottom contacting gears are withing this list. 
# If not, please add the missing ones:
Bottom_Trawls <- c("TBB", "OTB", "PTB", "OTT", "TB", "TBN", "TBS", "SB", 
                   "SDN", "SSC", "SPR", "SX", "SV", "DRB", "DRH", "HMD")
sort(unique(dat1$LE_GEAR))

dat1$GearGroup <- ifelse(dat1$LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")

#Find unique number of vessels, both per year and per month for mobile bottom gears 
#and other gears and write it to result folder
no_y <- dat1[, .(NoVessels=uniqueN(VE_REF)),
            by=.(LE_YEAR, N2k, GearGroup)]
no_y$LE_MONTH <- "All"
no_m <- dat1[, .(NoVessels=uniqueN(VE_REF)),
           by=.(LE_YEAR, LE_MONTH, N2k, GearGroup)]

nov <- rbind(no_m, no_y)
setorder(nov, LE_YEAR, N2k, LE_MONTH, GearGroup)
fwrite(nov, paste0(resPath, "NoVessels_", Country, ".csv"))

# Save pings within the N2k areas with data on month, year and gear
pings <- data.table(LE_YEAR=dat1$LE_YEAR, LE_MONTH=dat1$LE_MONTH, LE_GEAR=dat1$LE_GEAR,  
                    st_coordinates(dat1$geometry))
fwrite(pings, paste0(resPath, "pings_N2k_", Country, ".csv"))


################## Load all pings inside the selected ices square areas for the whole period ###################
m_list2 <- list.files(path = intermediatePath, pattern= "te", full.names = T)
dat2 <- rbindlist(lapply(m_list2, readRDS), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
dat2[is.na(dat2)] = 0

#Remove species columns with sum zero
zeroes <- names(colSums(Filter(is.numeric, dat2))
                [colSums(Filter(is.numeric, dat2))==0])
try(dat2[,(zeroes):=NULL], silent = T)

#Aggregate data 
out2 <- dat2[, .(NoVessels_VMS=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), 
               LE_KG_TOT_VMS=sum(LE_KG_TOT, na.rm=T), LE_EURO_TOT_VMS=sum(LE_EURO_TOT, na.rm=T)),
           by=.(LE_YEAR, LE_RECT, LE_GEAR)]

################## Load logbook data from the selected ices square for the whole period ###################

m_list3 <- list.files(path = intermediatePath, pattern= "eflalo", full.names = T)
dat3 <- rbindlist(lapply(m_list3, readRDS), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
for (i in seq_along(dat3)) set(dat3, i=which(is.na(dat3[[i]])), j=i, value=0)

#Remove species columns with sum zero
zeroes <- names(colSums(Filter(is.numeric, dat3))
                [colSums(Filter(is.numeric, dat3))==0])
try(dat3[,(zeroes):=NULL], silent = T)

#Aggregate data
out3 <- dat3[, .(NoVessels_LOG=uniqueN(VE_REF),
               LE_KG_TOT_LOG=sum(LE_KG_TOT, na.rm=T), LE_EURO_TOT_LOG=sum(LE_EURO_TOT, na.rm=T)),
           by=.(LE_YEAR, LE_RECT, LE_GEAR)]


# Merge vms pings data and logbook data.
out4 <- merge(out2, out3, by=c("LE_YEAR", "LE_RECT", "LE_GEAR"), all = T)

#Write aggregated landings and no. of vessels from logbooks and merged logbooks and vms to result folder
#You can choose to only report from one of the sources if you wish (replace out4 with out2 or out3)
fwrite(out4, paste0(resPath, "landings_rect_", Country, ".csv"))


############################################################################################################
############################### Please take a moment to review your results.################################ 
#For example compare the total landings in the selected ices squares to the total landings in the N2k areas#
############################################################################################################

out5 <- out4[,.(LE_KG_TOT_RECT=sum(LE_KG_TOT_VMS, na.rm = T), LE_EURO_TOT_RECT=sum(LE_EURO_TOT_VMS, na.rm = T)),
             by="LE_YEAR"]
out6 <- out1[,.(LE_KG_TOT_N2k=sum(LE_KG_TOT, na.rm = T), LE_EURO_TOT_N2k=sum(LE_EURO_TOT, na.rm = T)),
            by="LE_YEAR"]
out7 <- merge(out5,out6)

out7

fwrite(out7, paste0(resPath, "comp_", Country, ".csv"))
# Also, inspect the differences between landings directly from logbooks and from merged vms and logbooks
# These should be quite similar, but differences will occur
# since fishing is reported in the most fished ices square.
options("scipen"=100, "digits"=4)
View(out4)

#inspect specific tacsateflalo data in N2k areas. Here only for 
#gilnets in 2019 (since mapview cant handle very large datasets)               
st_geometry(dat1) <- dat1$geometry
mapview(dat1[dat1$LE_YEAR==2019 & dat1$LE_GEAR=="GN",], zcol="LE_KG_TOT")
