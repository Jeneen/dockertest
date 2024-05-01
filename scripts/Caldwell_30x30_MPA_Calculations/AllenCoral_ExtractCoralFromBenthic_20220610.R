###### ALLEN CORAL ATLAS - Extract coral from downloaded benthic files ####
######      Author: Iain R. Caldwell
######    1) For each tropical ecoregion folder
######        a) open the benthic geojson file
######        b) subset to include only those that are "coral/algae"
######        c) save the new polygon file as a geojson in a new folder for the results
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
rootDir <- "C:/AllenCoralAtlas/" #"E:/AllenCoralAtlas/" #"C:/AllenCoralAtlas/" #"D:/AllenCoralAtlas/"
unzipDir <- paste0(rootDir, "AllenCoral_UnzipResultsFiles/")
coralOnlyDir <- paste0(rootDir, "CoralOnlyFiles/")

######    1) For each tropical ecoregion folder #####
#Get list of all unzipped folders 
allenCoralUnZipFolders <- list.files(unzipDir, full.names = F)

# #Remove the Southern Red Sea as it seems to cause problems
# allenCoralUnZipFolders <- allenCoralUnZipFolders[!grepl(pattern = "SouthernRedSea", x = allenCoralUnZipFolders)]

i = 1 #for testing
for(i in 1:length(allenCoralUnZipFolders)) {
  message("Started processing unzipped folder ", i, " of ", length(allenCoralUnZipFolders), ": ", allenCoralUnZipFolders[i])
  folderName <- paste0(unzipDir, allenCoralUnZipFolders[i])
  benthicOrigFilename <- paste0(folderName, "/Benthic-Map/benthic.geojson")
  coralOnlyFilename <- paste0(coralOnlyDir, basename(folderName), "_CoralOnly.geojson")
  
  if(file.exists(coralOnlyFilename)) {
    message("Coral only file already saved")
  } else {
    message("Coral only file does not exist yet")
    #Check if the benthic file is where it should be
    if(file.exists(benthicOrigFilename)) {
      ######        a) open the benthic geojson file ####
      benthicGJ <- rgdal::readOGR(benthicOrigFilename) 
      
      ######        b) subset to include only those that are "coral/algae" ####
      benthicGJ <- benthicGJ %>%
        filter(class == "Coral/Algae")
      
      ######        d) save the new polygon file as a geojson in a new folder for the results ####
      writeOGR(obj = benthicGJ, dsn = coralOnlyFilename, layer = "benthicGJ", driver = "GeoJSON")
      
    } else {
      message("Benthic file does not exist")
    }
  }
}

