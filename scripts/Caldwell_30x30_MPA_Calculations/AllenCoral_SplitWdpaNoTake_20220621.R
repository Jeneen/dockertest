###### ALLEN CORAL ATLAS - Split WDPA data by no-take percentage ####
######      Author: Iain R. Caldwell
######    1) Open the tropical WDPA data
######    2) Split the WDPA data into three files and save
######        a) Completely no-take
######        b) Partial no-take
######        c) No no-take

rm(list = ls()) #remove past stored objects
options(scipen = 999) #turn off scientific notation
memory.limit(100000)

####  Load packages and libraries ####
library(tidyverse)
library(rgdal)
library(spdplyr)
library(raster)
library(rgeos)

####  Set parameters and directories ####
dateNum <- as.character(Sys.Date())
wdpaDir <- "C:/AllenCoralAtlas/WDPA/"

######    1) Open the tropical WDPA data ####
wdpaTropGeojson <- rgdal::readOGR(paste0(wdpaDir, "WDPA_Tropics_Jun2022.geojson")) #2777 elements

#Extract only those fields of most interest
wdpaTropGeojson <- wdpaTropGeojson %>% 
  dplyr::filter(MARINE != 0) %>% #remove those with no marine area
  dplyr::select(WDPAID, WDPA_PID, NAME, REP_M_AREA, NO_TAKE, NO_TK_AREA)

#Calculate the percentage no take within each MPA
wdpaTropGeojson$REP_M_AREA[wdpaTropGeojson$NO_TK_AREA > wdpaTropGeojson$REP_M_AREA] <- wdpaTropGeojson$NO_TK_AREA[wdpaTropGeojson$NO_TK_AREA > wdpaTropGeojson$REP_M_AREA] 
wdpaTropGeojson$NTZ_perc <- wdpaTropGeojson$NO_TK_AREA/wdpaTropGeojson$REP_M_AREA * 100
wdpaTropGeojson$NTZ_perc[wdpaTropGeojson$NO_TAKE == "All"] <- 100

######    2) Split the WDPA data into three files and save
######        a) Completely no-take
fullNtzWdpaTropGeojson <- wdpaTropGeojson %>% 
  dplyr::filter(NTZ_perc == 100) #355 elements

#Save file
writeOGR(obj = fullNtzWdpaTropGeojson, dsn = paste0(wdpaDir, "FullNTZ_WDPA_Tropics_Jun2022.geojson"), layer = "fullNtzWdpaTropGeojson", driver = "GeoJSON")

######        b) Partial no-take
partNtzWdpaTropGeojson <- wdpaTropGeojson %>% 
  dplyr::filter(!is.nan(NTZ_perc)) %>% 
  dplyr::filter(NTZ_perc < 100 & NTZ_perc > 0) #155

#Save file
writeOGR(obj = partNtzWdpaTropGeojson, dsn = paste0(wdpaDir, "PartNTZ_WDPA_Tropics_Jun2022.geojson"), layer = "partNtzWdpaTropGeojson", driver = "GeoJSON")

######        c) No no-take (or unknown)
noNtzWdpaTropGeojson <- wdpaTropGeojson %>% 
  dplyr::filter(NTZ_perc == 0 | is.nan(NTZ_perc)) #2264

#Save file
writeOGR(obj = noNtzWdpaTropGeojson, dsn = paste0(wdpaDir, "NoNTZ_WDPA_Tropics_Jun2022.geojson"), layer = "noNtzWdpaTropGeojson", driver = "GeoJSON")
