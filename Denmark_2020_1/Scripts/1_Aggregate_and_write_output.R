rm(list=ls())
library(vmstools)
library(data.table)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"Q:/dfad/users/jepol/home/20-02-25_Potentielle_MPA/" # Please change accordingly
RdataPath   <- paste0(sysPath,"Rdata/") ## Put cleaned tacsat and eflalo files in this folder
dataPath    <- paste0(sysPath,"Data/")  ## Put file speedarr.csv in this folder
resPath     <- paste0(sysPath,"Results/")  ## Output will generated here

dir.create(resPath, showWarnings = FALSE)

FlagCountry <- "DK" # Change to your own country code

################## Load all pings inside the defined areas for the whole period ###################
dat1 <- rbindlist(lapply(paste0(RdataPath, "TacsatEflalo_", 2013:2019, ".rds"), readRDS), fill = T)
dat1[, LE_YEAR := year(SI_DATIM)] #Add year

#Remove species columns with sum 0
zeroes <- names(colSums(Filter(is.numeric, dat1))
                [colSums(Filter(is.numeric, dat1))==0])
try(dat1[,(zeroes):=NULL], silent = T)


# Make a subset with only data from within the defined areas
dat2 <- dat1[!is.na(Name)]


#######Aggregate data and write to results folder################

#Per ICES square
out1 <- dat1[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
             by=.(LE_YEAR, Square, LE_GEAR, FlagCountry),
             .SDcols=grepl("LE_KG|LE_EURO", names(dat1))]

fwrite(out1, paste0(resPath, "landings_Square.csv"))

#Per defined area
out2 <- dat2[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
             by=.(LE_YEAR, Region, Name, LE_GEAR, FlagCountry),
             .SDcols=grepl("LE_KG|LE_EURO", names(dat1))]

fwrite(out2, paste0(resPath, "landings_RegionName.csv"))


# Check if all your mobile bottom contacting gears are withing this list. 
# If not, please add the missing ones:
Bottom_Trawls <- c("TBB", "OTB", "PTB", "OTT", "TB", "TBN", "TBS", "SB", 
                   "SDN", "SSC", "SPR", "SX", "SV", "DRB", "DRH", "HMD")
sort(unique(dat1$LE_GEAR))

#Add column GearGroup to data
dat1[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]
dat2[, GearGroup := ifelse(LE_GEAR %in% Bottom_Trawls, "MBCG", "Other")]

#Find unique number of vessels, per year for mobile bottom gears 
#and other gears and write it to result folder
nov_square <- dat1[, .(NoVessels=uniqueN(VE_REF)),
                   by=.(LE_YEAR, Square, GearGroup, FlagCountry)]

nov_RegionName <- dat2[, .(NoVessels=uniqueN(VE_REF)),
                   by=.(LE_YEAR, Region, Name, GearGroup, FlagCountry)]


# Save pings within the defined areas with data on gear, year, position, speed and area 
pings <- dat1[, c("LE_YEAR", "LE_GEAR", "SI_LONG", "SI_LATI", "SI_SP", "Square", "Name", "Region", "FlagCountry")]
fwrite(pings, paste0(resPath, "pings_N2k.csv"))


############################################################################################################
############################### Please take a moment to review your results.################################ 
#For example compare the cod landings in the selected ices squares to from VMS and logbooks#################
############################################################################################################


################## Load logbook data for the whole period ###################
m_list3 <- list.files(path = RdataPath, pattern= "cleanEflalo_", full.names = T)
dat3 <- data.table()
for(y in 2013:2019){
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

options("scipen"=100, "digits"=4)

codcompare <- merge(vmscod, logcod)

View(codcompare)



