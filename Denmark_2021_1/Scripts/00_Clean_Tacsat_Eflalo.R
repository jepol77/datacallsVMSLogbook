#This is a script, inspired very much from the work of Niels Hintzen. For further details see
#https://github.com/nielshintzen/vmstools/wiki

rm(list=ls())
library(haven)
library(data.table)
library(vmstools)

sysPath       <-"Q:/dfad/users/jepol/home/N2000/"
RdataPath  <-    paste(sysPath,"Rdata/",sep="")   
inputpath     <-  "Q:/dfad/data/Data/eflalo/"

dir.create(sysPath, showWarnings = FALSE)
dir.create(RdataPath, showWarnings = FALSE)

interval <- 60
spThres <- 25

yearsToSubmit <- sort(2019:2020)

for (year in yearsToSubmit) {
  tacsat <- readRDS(paste0(inputpath, "tacsat2_", year, ".rds"))
  
  tacsat <- data.frame(tacsat)
  tacsat <- formatTacsat(tacsat) # format each of the columns to the specified 
 
   ######## Clean Tacsat ########
  
  #Add datetime
  tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, 
                                      sep = " "), tz = "Europe/Paris", format = "%d/%m/%Y  %H:%M")
  
  ## REMOVE UNWANTED POINTS ##
  #points not on the globe
  idx <- which(abs(tacsat$SI_LATI) > 90 | abs(tacsat$SI_LONG) > 180) 
  if(length(idx)>0) tacsat          <- tacsat[-idx,]
  
  #Points outside compass range
  idx <- unique(c(idx,which(tacsat$SI_HE < 0 | tacsat$SI_HE >  360))) 
  if(length(idx)>0) tacsat          <- tacsat[-idx,]
  
  #Duplicate points
  uniqueTacsat    <- paste(tacsat$VE_REF,tacsat$SI_LATI,tacsat$SI_LONG,tacsat$SI_DATIM) 
  tacsat          <- tacsat[!duplicated(uniqueTacsat),] #get rid of the duplicates
  
  #Points with more speed than 25 knots
  # idx <- which(tacsat$SI_SP > spThres)
  # tacsat <- tacsat[-idx,]
  
  ### Add Intervals between pings ###
  tacsat <- intervalTacsat(tacsat,level="vessel")
  tacsat$INTV[is.na(tacsat$INTV)] <- interval
  tacsat$INTV[tacsat$INTV>120]    <- interval  ### If there is no pings for more than two hours, set the interval value to one hour
  tacsat <- tacsat[tacsat$INTV!=0,] # Remove duplicates
  
  #Point in harbor
  #tacsat <- tacsat[tacsat$SI_HARB!=1,]
  
  saveRDS(tacsat,file=file.path(RdataPath, paste0('cleanTacsat_', year, '.rds')))
  
  ######## Clean Eflalo ########
  
  eflalo <- readRDS(paste0(inputpath, "eflalo4_", year, ".rds"))
  eflalo <- data.frame(eflalo)
  eflalo <- formatEflalo(eflalo) # format each of the columns to the specified class
  
  #Remove duplicates
  eflalo <- eflalo[!duplicated(eflalo$LE_ID),]
  
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                                  tz = "Europe/Paris", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                  tz = "Europe/Paris", format = "%d/%m/%Y  %H:%M")
  
  # #Remve trips with departures after arrival
  # idx               <- which(eflalo$FT_LDATIM >= eflalo$FT_DDATIM)
  # eflalo            <- eflalo[idx,]
  
  saveRDS(eflalo,paste0(RdataPath, 'cleanEflalo_', year, '.rds'))
}




