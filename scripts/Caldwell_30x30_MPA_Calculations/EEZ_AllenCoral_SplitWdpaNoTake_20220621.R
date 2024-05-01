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
library(terra)
library(sf)

####  Set parameters and directories ####
dateNum <- as.character(Sys.Date())
wdpaDir <- "Data/30x30/wdpa/"

######    1) Open the tropical WDPA data ####
wdpa1 <- readOGR(dsn = 
               "Data/30x30/wdpa/wdpa1/WDPA_WDOECM_Nov2022_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp", verbose = FALSE)
wdpa2 <- readOGR(dsn = 
                   "Data/30x30/wdpa/wdpa2/WDPA_WDOECM_Nov2022_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp", verbose = FALSE)
wdpa3 <- readOGR(dsn = 
                   "Data/30x30/wdpa/wdpa3/WDPA_WDOECM_Nov2022_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp", verbose = FALSE)

#filter and crop
wdpa1<- wdpa1 %>% 
  dplyr::filter(MARINE != 0) %>% #remove those with no marine area
  dplyr::select(WDPAID, WDPA_PID, NAME, REP_M_AREA, NO_TAKE, NO_TK_AREA)
wdpa2<- wdpa2 %>% 
  dplyr::filter(MARINE != 0) %>% #remove those with no marine area
  dplyr::select(WDPAID, WDPA_PID, NAME, REP_M_AREA, NO_TAKE, NO_TK_AREA)
wdpa3<- wdpa3 %>% 
  dplyr::filter(MARINE != 0) %>% #remove those with no marine area
  dplyr::select(WDPAID, WDPA_PID, NAME, REP_M_AREA, NO_TAKE, NO_TK_AREA)

tropicLat <- 23.43644
wdpa1TropShp <- crop(wdpa1, extent(-180, 180, -1*tropicLat, tropicLat))
wdpa2TropShp <- crop(wdpa2, extent(-180, 180, -1*tropicLat, tropicLat))
wdpa3TropShp <- crop(wdpa3, extent(-180, 180, -1*tropicLat, tropicLat))

#merge the files
wdpa_all <- union(wdpa1TropShp, wdpa2TropShp)
wdpa_all <- union(wdpa_all, wdpa3TropShp)

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
