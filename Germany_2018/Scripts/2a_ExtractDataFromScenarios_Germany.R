### Extract data from all Scenarios according to the lookup table ###

# Before running this code the following needs to be done:
### - The file lookup.csv should be placed in folder dataPath
### - Cleaned tacsatEflalo files for all the years should be in the RdataPath. Each file 
###   should be saved using saveRDS() as tacsatEflalo_"Year".rds (eg. tacsatEflalo_2016.rds)
###   These files  should be in the standard tacsatEflalo format and contain merged kg and EURO for all
###   species. This can be obtained using the countrys own procesdure or using 
###   the 0_CreateTacsatEflalo.R script. 
###   Furthermore the Cleaned tacsatEflalo should contain these columns:
###   "VE_REF", "SI_LATI", "SI_LONG", "SI_DATE", "SI_TIME", "SI_SP", "SI_HE", "SI_HARB", "SI_STATE", "SI_FT", "SI_DATIM", "INTV", "FT_REF"
### - Cleaned eflalo files should be in the RdataPath, saved as cleanEflalo_"Year".rds (eg. cleanEflalo_2016.rds)

rm(list=ls())
library(vmstools)
library(rgdal)
library(data.table)
library(sp)
library(beepr)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"C:/Users/jepol/Desktop/N2000/Germany_2018/"
RdataPath   <- "Q:/dfad/users/jepol/home/N2000/Rdata/"
dataPath    <- paste0(sysPath,"Data/")
polygonPath <- paste0(sysPath,"shapes")
intermediatePath <- paste0(sysPath,"int/")
resPath     <- paste0(sysPath,"Results/")

# If directories not exist, create them
dir.create(sysPath, showWarnings = FALSE)
dir.create(RdataPath, showWarnings = FALSE)
dir.create(dataPath, showWarnings = FALSE)
dir.create(polygonPath, showWarnings = FALSE)
dir.create(intermediatePath, showWarnings = FALSE)
dir.create(resPath, showWarnings = FALSE)

# Read in lookup table
lookup <- fread(paste0(dataPath, "Lookup.csv"), blank.lines.skip=T)
lookup[lookup==""] <- NA

measures <- gsub(".shp", "", list.files(path = polygonPath, pattern = "shp"))

#Load  measures shapefiles and create a single shapefile for all the measures (ms)
ms <- readOGR(dsn = polygonPath, layer = measures[1])
M1 <- readOGR(dsn = polygonPath, layer = measures[1])

for(i in measures[2:length(measures)]){
x <- readOGR(dsn = polygonPath, layer = i)
ms <- rbind(ms,x)
assign(i, x)
}

# Make a AOI polygon for vms pings near the measures.
# 20% larger than the extent of the entire measures (ms)
sizeup <- 1.20

a <- bbox(ms)[1] - (((bbox(ms)[3]-bbox(ms)[1])*sizeup) - (bbox(ms)[3]-bbox(ms)[1]))
b <- bbox(ms)[3] + (((bbox(ms)[3]-bbox(ms)[1])*sizeup) - (bbox(ms)[3]-bbox(ms)[1]))
c <- bbox(ms)[2] - (((bbox(ms)[4]-bbox(ms)[2])*sizeup) - (bbox(ms)[4]-bbox(ms)[2]))
d <- bbox(ms)[4] + (((bbox(ms)[4]-bbox(ms)[2])*sizeup) - (bbox(ms)[4]-bbox(ms)[2]))

AOI <- as(raster::extent(a,b,c,d), "SpatialPolygons")
proj4string(AOI) <- ms@proj4string
# plot(AOI)
# 
# plot(ms,add=T)

for (Year in c(2012:2017)) {
  print(Year)
  tacsatEflalo <- readRDS(paste0(RdataPath,"tacsatEflalo_",Year,".rds"))
  eflalo <- readRDS(paste0(RdataPath,"cleanEflalo_",Year,".rds"))

  ## Add icessquares and Csquares to tacsatEflalo
  tacsatEflalo$REF_ICES <- ICESrectangle(tacsatEflalo)
  tacsatEflalo$CSquare  <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, 0.05)
  
  # Add relevant information to tacsatEflalo from eflalo
  tacsatEflalo$LE_MSZ   <- eflalo$LE_MSZ[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_MET   <- eflalo$LE_MET[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$VE_LEN   <- eflalo$VE_LEN[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_GEAR  <- eflalo$LE_GEAR[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_MONTH <- as.integer(month(tacsatEflalo$SI_DATIM))
  
    # Make tacsatEflalo into a data.table for faster processing
  te <- data.table(tacsatEflalo); gc()
  # Calculate total kg and EURO
  if(!"LE_KG_TOT" %in% colnames(te))
    te[,LE_KG_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("KG", names(te))]
  if(!"LE_EURO_TOT" %in% colnames(te))
    te[,LE_EURO_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("EURO", names(te))]
  
  #to indicate vessels targeting brown shrimp (Crangon crangon), the metier level 6 equal to ’TBB_CRU_16-31_0_0‘ is indicated as ‘TBC’ (‘Trawl Beam Crangon’) on gear level
  te[LE_MET=="TBB_CRU_16-31_0_0", LE_GEAR:="TBC"]
 
  # make spatial points for each occurence in the tacsatEflalo file
  pings  <- SpatialPoints(te[,.(SI_LONG, SI_LATI)],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #Define relevant geargroups
  Total_Gear <- unique(te$LE_GEAR)
  
  #Also include TBC in bottom trawls for reference - can be filtered out later. 
  Bottom_Trawls <- c("TBB", "OTB", "PTB", "OTT", "TB", "TBN", "TBS", "SB", "SDN", "SSC", "SPR", "SX", "SV", "DRB", "DRH", "HMD", "TBC")
  Gillnets_Entangling_nets <- c("GN", "GNS", "GND", "GNC", "GTR", "GTN")
 
  #Save all points with month, gear landings and position in the area of interest (AOI)
  p <- te[!is.na(over(pings, as(AOI,"SpatialPolygons")))]
  points <- p[,c("LE_MONTH", "LE_GEAR", "LE_KG_TOT", "LE_EURO_TOT", "SI_LATI", "SI_LONG")]
  points <- data.table(Year, points)
  saveRDS(points, file=paste0(intermediatePath, "Points_", Year, ".rds"))
  
  #Calculate output data according to each row in the lookup table and save it in the RdataPath
  for (i in 1:nrow(lookup)) {

    #Get data inside shape
    if(!is.na(lookup$Shape[i])){
      shape <- get(lookup$Shape[i])
      #Get the rigth shape and ensure that it is in the right projection
      shape <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      #Get all data within shape
      sub <- te[!is.na(over(pings, as(shape,"SpatialPolygons")))]
    }
    
      #Get data within timeframe
      if(!is.na(lookup$Time_Start[i])){
        sub <- sub[strptime(sub$SI_DATE, "%d/%m/%Y") >= strptime(paste(lookup$Time_Start[i], Year, sep = "-"),  "%d-%m-%Y") &
                     strptime(sub$SI_DATE, "%d/%m/%Y") <= strptime(paste(lookup$Time_End[i], Year, sep = "-"),  "%d-%m-%Y") ]
      }
      
      # Get relevant gears - remember to define them first
      if(!is.na(lookup$Gears[i])){
        sub <- sub[LE_GEAR %in% get(lookup$Gears[i])]
      }
      
      #Remove species columns where the sum is zero
      zeroes <- names(colSums(Filter(is.numeric, sub))
                      [colSums(Filter(is.numeric, sub))==0])
      try(sub[,(zeroes):=NULL], silent = T)
      
      if(nrow(sub)!=0){
      #Aggregate data on gear level
      out <- sub[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60, na.rm=T), lapply(.SD, sum, na.rm=T)),
                 by=.(Gear=LE_GEAR),
                 .SDcols=grepl("LE_KG|LE_EURO", names(sub))]
      
      #Add Scenario name and year to data table
      out <- data.table(Scenario=lookup$Measure[i], Year=Year, out)
      #Write to rds file
      saveRDS(out, file=paste0(intermediatePath, "MeasureData_", lookup$Measure[i], "_", Year, ".rds"))
    }
  }
}


# Get data from all measures / years into one data table.
m_list1 <- list.files(path = intermediatePath, pattern= "MeasureData_", full.names = T)
dat <- rbindlist(lapply(m_list1, readRDS), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
dat[is.na(dat)] = 0
#setcolorder(measures, c("Year", "Scenario", "GEAR", "LE_KG_TOT", "LE_EURO_TOT"))
saveRDS(dat, paste0(intermediatePath, "Measuredata.rds"))

# Get point data from all years in the AOI into one data table.
pt_lst <- list.files(path = intermediatePath, pattern= "Points", full.names = T)
points <- rbindlist(lapply(pt_lst, readRDS), fill=T)
saveRDS(points, paste0(intermediatePath, "Points.rds"))

# Wake you up when finished
beep()
