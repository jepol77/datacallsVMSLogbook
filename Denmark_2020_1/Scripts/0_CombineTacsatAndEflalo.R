### Combine Tacsat And Eflalo - proposal to be adapted to your specific needs ###
### Code (Vers 3 27.02.2020) by Jeppe Olsen (jepol@aqua.dtu.dk) 
### The code is developed with input from Christian von  Dorrien (christian.dorrien@thuenen.de) 

# Before running this code the following needs to be done:
### - All vms points needs to be cleaned, so that all points are:
###     - Not inside Harbor, on earth, not on land, not duplicate, not pseudoDupliacate, no foreign vessels
###   cleaned vms files named tacsat should be saved as cleanTacsat_"Year".rds (eg. cleanTacsat_2016.rds) in RdataPath.
### - All logbook data need to be cleaned, so all records are: 
###     - Not duplicated, not NoCatch, no landings before departures, no overlapping trips, no dates before 1st of January, 
###       not wrong mesh sizes, not wrong vessel lengths
###   Cleaned logbook data named eflalo should be saved as cleanEflalo_"Year".rds (eg. cleanEflalo_2016.rds) in RdataPath.
### - Make a file called speedarr.csv, which should contain speed thresholds for all gears in your fleet. Place it in dataPath

### See https://github.com/nielshintzen/vmstools/wiki for details.

### This code uses static speed thresholds, if automatic thresholds are preferred, please change code accordingly.
### The output is a combined VMS and Logbook file based on catch date and trip id, and is filtered to only the requested ICES squares.

### If your country combines VMS and Logbooks in a different way more suitable for your country, please use that method instead, and  
### mention it when you answer the call. 

rm(list=ls())
library(vmstools)
library(data.table)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"Q:/dfad/users/jepol/home/20-02-25_Potentielle_MPA/"
RdataPath   <- paste0(sysPath,"Rdata/") ## Put cleaned tacsat and eflalo files in this folder
dataPath    <- paste0(sysPath,"Data/")  ## Put file speedarr.csv in this folder

FlagCountry <- "DK" # Change to your own country code

YearsToSubmit <- 2015:2019 # If your country havent processed 2019 yet, please change to 2014:2018

# If directories not exist, create them
dir.create(sysPath, showWarnings = FALSE)
dir.create(RdataPath, showWarnings = FALSE)
dir.create(dataPath, showWarnings = FALSE)
# 
setwd(sysPath)

### Load and inspect polygons
shp <- readRDS(paste0(RdataPath, "shp.rds"))
mapview(shp)


#- Set the country abbreviation to e.g. deu, gbr, dnk
country     <- "dnk"
interval    <- 60 #set interval time applicable for your country VMS

for (Year in YearsToSubmit) {
  print(Year)  
  
  tacsat <- readRDS(paste0(RdataPath, "cleanTacsat_", Year, ".rds"))
  eflalo <- readRDS(paste0(RdataPath, "cleanEflalo_", Year, ".rds"))
  
  tacsat <- formatTacsat(tacsat) # format each of the columns to the specified class
  eflalo <- formatEflalo(eflalo) # format each of the columns to the specified class
  
  #Create as.POSIXct columns for date and time
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
 
  #- Merge eflalo and tacsat together
  tacsatp               <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  #Report VMS points that could not be coupled with any logbook
  x              <- subset(tacsatp,FT_REF == 0)
  x$SI_STATE <- 0
  print(paste("Percentage VMS points that could not be combined with any logbooks using vmstools::mergeEflalo2Tacsat (most of these should be sailing not related to fishing or days in harbour where VMS is not turned off):", round(nrow(x)/nrow(tacsat)*100, digits = 2), "%"))
  
  #Only use vms points that can be merged with logbooks
  tacsatp <- subset(tacsatp,FT_REF != 0)
  
  # Assign Gears and mesh size to tacsatp in accordance with the logbook
  tacsatp$LE_GEAR        <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$LE_MSZ        <- eflalo$LE_MSZ[match(tacsatp$FT_REF,eflalo$FT_REF)]
  
  # If there is no duration on a ping, 
  # set it to the default interval of the country
  tacsatp$INTV[is.na(tacsatp$INTV)] <- interval
  
  # If the length of a ping duration is more than twice of the normal interval (e.g. becuase 
  # there has been no signal for 6 hours, or the ping is at the start of a trip), set it 
  # to the deafult interval of the country
  tacsatp$INTV[tacsatp$INTV>(interval*2)]    <- interval
  
  #- Read in fixed speed bounds for different gears - Place speedarr.csv in dataPath
  # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
  
  speedarr              <- read.csv(file=paste(dataPath,"/speedarr.csv",sep=""))
  colnames(speedarr)    <- c("LE_GEAR","min","max", "net")
  speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
  speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp$LE_GEAR),]
  
  # This code is similar to vmstools:splitAmongPings, but with a few differences:
  # - This code only combine logbook data with vms data on a day level. If there is no pings
  #   on a day where there is logbook data, that logbook data is not used. That differs from the 
  #   splitAmongPings, which can combine also at trip level, meaning that if there are no pings on
  #   a certain day, the kg and euro will be split out on the rest of the trip. 
  # - This code does not combine using C-Squares, since danish fishermen will only report ICES-squares
  #   where most of the fishing takes place. splitAmongPings will take out pings not in the ICES-square 
  #   reported and only distribute kg and euro to pings inside the reported ICES-square. 
  # 
  # Furthermore, this script can handle all data, so no need to subset on area and "most important 
  # species". 
  
  # Assign activity to the column SI_STATE. If speed is within threshold, set it to 1, else set it to 0
  t <- data.table(tacsatp) # data.table for faster processing
  class(t$SI_STATE) <- "numeric"
  t$SI_STATE<-0
  gears <- unique(tacsatp$LE_GEAR[!is.na(tacsatp$LE_GEAR)])
  
  for(mm in gears){
    t[LE_GEAR==mm & SI_SP >= speedarr[speedarr$LE_GEAR==mm,"min"] & SI_SP <= speedarr[speedarr$LE_GEAR==mm,"max"], SI_STATE:=1]
  }
  
  
  ####### Danish vessels using TBB have different speed thresholds depending on mesh size. This is a way to deal with that. Is commented out as per default. ##################################
  #t[t$LE_GEAR=="TBB",]$SI_STATE <- 0
  #
  #t[t$LE_GEAR=="TBB" & t$SI_SP >= 2 & t$SI_SP <= 4 
  #        & t$LE_MSZ <= 40,]$SI_STATE <- 1
  #
  #t[t$LE_GEAR=="TBB" & t$SI_SP >= 5 & t$SI_SP <= 7 
  #        & t$LE_MSZ > 40,]$SI_STATE <- 1
  #############################################################################################################################################################################################
  
  
  #save all pings for later processing
  t_all <- t
  
  #Use only vessels which are fishing for subsequent processing.
  t <- t[SI_STATE==1]
  
  
  e <- data.table(eflalo) # data.table for faster processing
  
  #Add a SI_DATE to merged eflalo, for easier merging
  e[,SI_DATE:=LE_CDAT]
  
  #Add id for merging
  t[,id:=paste0(FT_REF, SI_DATE)]
  e[,id:=paste0(FT_REF, SI_DATE)]
  
  #find all column names with KG or EURO in them
  kg_euro <- grep("KG|EURO", colnames(e), value = T)
  
  #Sum all eflalo records with the same day and id
  n <- e[,lapply(.SD,sum),by=.(id),
         .SDcols=kg_euro]
  
  #Setkey for merging
  setkey(t, id)
  setkey(n, id)
  
  #find all vms points that cannot be merged
  tx <- t[!n, on=c("id")]
  print(paste("Percentage cleaned VMS points not able to be merged with Logbooks, using date and FT_REF:", round(nrow(tx)/nrow(t)*100,2), "%"))
  
  #Add kg and euro to vms points
  ts <- merge(t, n)
  
  #set key for merging
  setkey(ts, id)
  
  #Set a weight on each point for later multiply
  ts[,Weight:=INTV/sum(INTV), by=.(id)]
  #Multiply the weight to each KG and EURO column 
  ts[,(kg_euro):= lapply(.SD, function(x) x * Weight), .SDcols=kg_euro]
  
  #Find eflalo records with no matching vms points
  `%!in%` <- function(a,b) ! a %in% b
  ex <- e[id%!in%unique(ts$id)]
  
  print (paste("Percentage cleaned logbook entries not able to be merged with VMS data, using date and FT_REF (eg, vessels <12 m):", round(nrow(ex)/nrow(e)*100,2), "%"))

  
  ## Here we could distribute eflalo records that matches trip, but not day ###
  ## not desired by Denmark, but useful for others?                         ###
  ##############################################################################

  ind <-   which(sapply(ts, is.numeric))
  for(j in ind){
    set(ts, i = which(is.na(ts[[j]])), j = j, value = 0)
  }
  
  #Remove columns with sum zero
  zeroes <- names(colSums(Filter(is.numeric, ts))
                   [colSums(Filter(is.numeric, ts))==0])
  ts[,(zeroes):=NULL]
  
  #Add ices square to data
  ts$Square <- ICESrectangle(data.frame(SI_LONG = ts$SI_LONG,
                                                SI_LATI = ts$SI_LATI))
  
  ns_squares <- c("39F4", "39F5", "39F6", "39F7", "39F8", "40F3", "40F4", "40F5", "40F6", "40F7", 
                  "40F8", "41F3", "41F4", "41F5", "41F6", "41F7", "41F8", "42F5", "42F6", "42F7",
                  "42F8", "43F6", "43F7", "43F8", "44F8", "44F9", "44G0", "45F9", "45G0")
  
  bs_squares <- c("38G3", "38G4", "38G5", "39G4", "39G5", "39G6", "40G4", "40G5")
  
  #Only keep pings within these Ices Squares
  ts <- ts[Square %in% c(ns_squares, bs_squares)]
 
  #Add Name and Region to TacsatEflalo
  pts <- ts %>% 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>% 
    sf::st_set_crs(4326)
  
  out <- st_join(pts, shp, join = st_intersects)
  
  ts$Name <- out$Name
  ts$Region <- out$Region
  
  ts$FlagCountry <- FlagCountry
  
  #Save the combined VMS and Logbook file where fishing is assumed.
  saveRDS(ts, (paste0(RdataPath,"tacsatEflalo_",Year,".rds",sep="")))
}

