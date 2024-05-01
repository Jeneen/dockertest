###### ALLEN CORAL ATLAS - Calculate the percentage of coral protected ####
######      Author: Iain R. Caldwell
######    1) Open the fully protected tropical WDPA data subset
######    2) Create an empty dataframe that can be filled with total coral area and total fully protected coral in 100% protected MPAs in each ecoregion
######    3) For each tropical ecoregion coral only file
######        a) open the coral only file and calculate the total area of corals
######        b) crop the fully protected data to the coral data (extent then actual)
######        c) flatten the remaining fully protected data, save, and calculate the area covered

rm(list = ls()) #remove past stored objects
options(scipen = 999) #turn off scientific notation
memory.limit(100000)

####  Load packages and libraries ####
library(tidyverse)
library(rgdal)
library(spdplyr)
library(raster)
library(rgeos)
library(sp)
library(maptools)

####  Set parameters and directories ####
rgeos::set_RGEOS_CheckValidity(2L)
dateNum <- as.character(Sys.Date())
rootDir <- "C:/AllenCoralAtlas/" #"E:/AllenCoralAtlas/" #"C:/AllenCoralAtlas/"
wdpaDir <- paste0(rootDir, "WDPA/")
coralOnlyDir <- paste0(rootDir, "CoralOnlyFiles/")
coralFullMpaDir <- paste0(rootDir, "CoralFullMpaFiles/")

######    1) Open the tropical WDPA data subsets for fully protected ####
fullNtzWdpaTropGeojson <- rgdal::readOGR(paste0(wdpaDir, "FullNTZ_WDPA_Tropics_Jun2022.geojson")) #355 elements

#Open the marine ecoregions, provinces, and realms
meowTBL <- read_csv(paste0(rootDir, "MarineEcoregionsProvincesRealms.csv"))

######    2) Create an empty dataframe that can be filled with total coral area, total coral in MPAs, and total coral in NTZs in each ecoregion ####
percCoralFullProtFilename <- paste0(rootDir, "PercCoralFullProtByEcoregion_20220621.csv") 
if(file.exists(percCoralFullProtFilename)) {
  percCoralFullProtTBL <- read_csv(file = percCoralFullProtFilename) %>%
    distinct()
} else {
  percCoralFullProtTBL <- tibble(Ecoregion = as.character(NA),
                                 TotalCoralArea = as.numeric(NA),
                                 CoralFullProtArea = as.numeric(NA),
                                 PercCoralFullProt = as.numeric(NA)) %>%
    filter(!is.na(Ecoregion))
  
  #Save a copy of this file to append to later
  write_csv(x = percCoralFullProtTBL, file = percCoralFullProtFilename)
}

# #### For Ningaloo reefs, remove those that Shaun Wilson indicated were not fully protected (WDPA_PID's of 63174_B and 555556894_A)
# fullNtzWdpaTropGeojson <- fullNtzWdpaTropGeojson %>% 
#   filter(!WDPA_PID %in% c("63174_B", "555556894_A"))

######    3) For each tropical ecoregion coral only file ####
#Get list of all coral only files extracted from Allen Coral Atlas
coralOnlyFiles <- list.files(coralOnlyDir, pattern = "_CoralOnly.geojson", full.names = F)

i = 54
for(i in 1:length(coralOnlyFiles)) {
  message("Started processing coral only file ", i, " of ", length(coralOnlyFiles), ": ", coralOnlyFiles[i])
  ecoregionName <- gsub("_.*$", "", coralOnlyFiles[i])
  coralFullMpaFilename <- paste0(coralFullMpaDir, ecoregionName, "_CoralFullMPA.geojson")
  
  if(ecoregionName %in% percCoralFullProtTBL$Ecoregion) {
    message("Coral percentage already added")
  } else {
    ######        a) open the coral only file ####
    coralGJ <- rgdal::readOGR(paste0(coralOnlyDir, coralOnlyFiles[i]))
    message("Coral only file opened")
    
    ######        b) crop the coral data to the fully protected data
    cropFullNtzCoralGJ <- raster::crop(fullNtzWdpaTropGeojson, extent(coralGJ))
    message("Corals cropped to MPA extent")
    
    if(is.null(cropFullNtzCoralGJ)) {
      message("No overlap between corals and full MPAs")
      cropCoralFullNtzGJ <- NULL
    } else {
      cropCoralFullNtzGJ <- raster::crop(coralGJ, cropFullNtzCoralGJ)
      message("Corals cropped to full MPAs")
    }
    
    
    if(is.null(cropCoralFullNtzGJ)) {
      message("No full MPAs within extent")
      
      #Add data to the table
      iterPercCoralProtTBL <- percCoralFullProtTBL[FALSE,]
      iterPercCoralProtTBL[1,] <- NA
      
      iterPercCoralProtTBL <- iterPercCoralProtTBL %>% 
        mutate(Ecoregion = ecoregionName,
               TotalCoralArea = sum(area(coralGJ) / 1000000),
               CoralFullProtArea = 0,
               PercCoralFullProt = 0)
      
      write_csv(x = iterPercCoralProtTBL, percCoralFullProtFilename, append = T)
      message("Added data to table")
      rm(iterPercCoralProtTBL, coralGJ, cropCoralFullNtzGJ)
    } else {
      message("Some overlap with full NTZ's")
      
      #Add data to the table
      iterPercCoralProtTBL <- percCoralFullProtTBL[FALSE,]
      iterPercCoralProtTBL[1,] <- NA
      
      iterPercCoralProtTBL <- iterPercCoralProtTBL %>% 
        mutate(Ecoregion = ecoregionName,
               TotalCoralArea = sum(area(coralGJ)) / 1000000,
               CoralFullProtArea = sum(area(cropCoralFullNtzGJ)) / 1000000,
               PercCoralFullProt = CoralFullProtArea/TotalCoralArea*100)
      
      write_csv(x = iterPercCoralProtTBL, percCoralFullProtFilename, append = T)
      message("Added data to table")
      
      #Save file
      writeOGR(obj = cropCoralFullNtzGJ, dsn = coralFullMpaFilename, layer = "cropCoralFullNtzGJ", driver = "GeoJSON")
      message("Saved file")
    }
  }
}

percCoralFullProtTBL <- percCoralFullProtTBL %>% 
  rename(Ecoregion_subregion = Ecoregion) %>% 
  mutate(Ecoregion = case_when(grepl(pattern = "GreaterAntilles", x = Ecoregion_subregion) ~ "GreaterAntilles",
                               grepl(pattern = "Bahamian", x = Ecoregion_subregion) ~ "Bahamian",
                               TRUE ~ Ecoregion_subregion)) 

percCoralFullProtSummTBL <- percCoralFullProtTBL %>% 
  filter(Ecoregion != "Total") %>% 
  dplyr::group_by(Ecoregion) %>%
  dplyr::summarise(TotalCoralArea = sum(TotalCoralArea),
                   CoralFullProtArea = sum(CoralFullProtArea)) %>% 
  ungroup() %>% 
  mutate(PercCoralFullProt = CoralFullProtArea/TotalCoralArea*100)

percCoralFullProtSummTBL[nrow(percCoralFullProtSummTBL),] <- list("Total",
                                                               sum(percCoralFullProtSummTBL$TotalCoralArea),
                                                               sum(percCoralFullProtSummTBL$CoralFullProtArea),
                                                               sum(percCoralFullProtSummTBL$CoralFullProtArea)/sum(percCoralFullProtSummTBL$TotalCoralArea)*100)

#Save summarized file
write_csv(x = percCoralFullProtSummTBL, file = paste0(rootDir, "PercCoralFullProtByEcoregionTotal_20220714.csv"))

###Summarize by province
meowTBL <- meowTBL %>% 
  rename(Ecoregion = ECOREGION) %>% 
  mutate(Ecoregion = gsub(pattern = " |/|-", replacement = "", x = Ecoregion))

setdiff(percCoralFullProtTBL$Ecoregion, meowTBL$Ecoregion)

percCoralProtByProvinceTBL <- percCoralFullProtTBL %>% 
  left_join(meowTBL) %>% 
  group_by(PROVINCE) %>% 
  summarise(TotalCoralArea = sum(TotalCoralArea),
            CoralFullProtArea = sum(CoralFullProtArea)) %>% 
  ungroup() %>% 
  mutate(PercCoralFullProt = CoralFullProtArea/TotalCoralArea*100)

percCoralProtByProvinceTBL[nrow(percCoralProtByProvinceTBL)+1,] <- NA

percCoralProtByProvinceTBL[nrow(percCoralProtByProvinceTBL),] <- list("Total",
                                                                  sum(percCoralProtByProvinceTBL$TotalCoralArea, na.rm = T),
                                                                  sum(percCoralProtByProvinceTBL$CoralFullProtArea, na.rm = T),
                                                                  sum(percCoralProtByProvinceTBL$CoralFullProtArea, na.rm = T)/sum(percCoralProtByProvinceTBL$TotalCoralArea, na.rm = T)*100)

#Save summarized file
write_csv(x = percCoralProtByProvinceTBL, file = paste0(rootDir, "PercCoralFullProtByProvinceTotal_20220714.csv"))

###Summarize by realm
percCoralProtByRealmTBL <- percCoralFullProtTBL %>% 
  left_join(meowTBL) %>% 
  group_by(REALM) %>% 
  summarise(TotalCoralArea = sum(TotalCoralArea),
            CoralFullProtArea = sum(CoralFullProtArea)) %>% 
  ungroup() %>% 
  mutate(PercCoralFullProt = CoralFullProtArea/TotalCoralArea*100)

percCoralProtByRealmTBL[nrow(percCoralProtByRealmTBL)+1,] <- NA

percCoralProtByRealmTBL[nrow(percCoralProtByRealmTBL),] <- list("Total",
                                                                      sum(percCoralProtByRealmTBL$TotalCoralArea, na.rm = T),
                                                                      sum(percCoralProtByRealmTBL$CoralFullProtArea, na.rm = T),
                                                                      sum(percCoralProtByRealmTBL$CoralFullProtArea, na.rm = T)/sum(percCoralProtByRealmTBL$TotalCoralArea, na.rm = T)*100)

#Save summarized file
write_csv(x = percCoralProtByRealmTBL, file = paste0(rootDir, "PercCoralFullProtByRealmTotal_20220714.csv"))

