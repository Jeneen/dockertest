###### ALLEN CORAL ATLAS - Create separate tropical ecoregions and save (as GeoJSON) ####
######      Author: Iain R. Caldwell
######    1) Open the ecoregion shapefile and subset to tropics (23.5 degress north and south)
######    2) Loop through each ecoregion and save as new GeoJSON

rm(list = ls()) #remove past stored objects
options(scipen = 999) #turn off scientific notation
memory.limit(100000)

####  Load packages and libraries ####
library(tidyverse)
library(rgdal)
library(spdplyr)
library(raster)

####  Set parameters and directories ####
dateNum <- as.character(Sys.Date())
tropicLat <- 23.43644
resultsDir <- "C:/AllenCoralAtlas/"

######    1) Open the ecoregion shapefile and subset to tropics (23.5 degress north and south) ####
meowShp <- readOGR(dsn = paste0(resultsDir, "MEOW"), layer = "meow_ecos", verbose = FALSE)

meowTropShp <- crop(meowShp, extent(-180, 180, -1*tropicLat, tropicLat))

rm(meowShp)

#Create and save a file with the ecoregions, realms, and provinces
meowTBL <- as.data.frame(meowTropShp) %>% 
  dplyr::select(ECOREGION, PROVINCE, REALM) %>% 
  distinct()

#Save this file
write_csv(x = meowTBL, file = paste0(resultsDir, "MarineEcoregionsProvincesRealms.csv"))

######    2) Loop through each ecoregion and save as new GeoJSON ####
allTropEcoregions <- meowTropShp$ECOREGION

i = 1
for(i in 1:length(allTropEcoregions)) {
  message("Started extracting ecoregion: ", allTropEcoregions[i])
  ecoregionFilename <- paste0(resultsDir,
                              "TropEcoGeoJson/",
                              gsub(pattern = '[[:punct:][:blank:]]+', replacement = "", x = allTropEcoregions[i]),
                              "_TropEco.geojson")

  #Save as GeoJSON
  if(file.exists(ecoregionFilename)) {
    message("File already exists")
  } else {
    indEcoregionShp <- meowTropShp %>% 
      filter(ECOREGION == allTropEcoregions[i])
    
    writeOGR(obj = indEcoregionShp, dsn = ecoregionFilename, layer = "indEcoregionShp", driver = "GeoJSON")
  }
}

#Open the Greater Antilles file and split into smaller ones
greaterAntGeojson <- rgdal::readOGR("C:/AllenCoralAtlas/TropEcoGeoJson/GreaterAntilles_TropEco.geojson") #5875 elements

extent(greaterAntGeojson) #xmin = -86.13631, xmax = -64.58074, ymin = 14.69378, ymax = 23.43644

westNorthwestGreatAntGeojson <- crop(greaterAntGeojson, extent(-180, -80, 21, tropicLat))
eastNorthwestGreatAntGeojson <- crop(greaterAntGeojson, extent(-80, -75, 21, tropicLat))
midwestGreatAntGeojson <- crop(greaterAntGeojson, extent(-180, -75, 19, 21))
southwestGreatAntGeojson <- crop(greaterAntGeojson, extent(-180, -75, -1*tropicLat, 19))
eastGreatAntGeojson <- crop(greaterAntGeojson, extent(-75, 180, -1*tropicLat, tropicLat))

#Save these
writeOGR(obj = westNorthwestGreatAntGeojson, dsn = "C:/Users/jc690391/Documents/AllenCoralAtlas/TropEcoGeoJson/WestNorthwestGreaterAntilles_TropEco.geojson",
         layer = "westNorthwestGreatAntGeojson", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = eastNorthwestGreatAntGeojson, dsn = "C:/Users/jc690391/Documents/AllenCoralAtlas/TropEcoGeoJson/EastNorthwestGreaterAntilles_TropEco.geojson",
         layer = "eastNorthwestGreatAntGeojson", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = midwestGreatAntGeojson, dsn = "C:/Users/jc690391/Documents/AllenCoralAtlas/TropEcoGeoJson/MidwestGreaterAntilles_TropEco.geojson",
         layer = "midwestGreatAntGeojson", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = southwestGreatAntGeojson, dsn = "C:/Users/jc690391/Documents/AllenCoralAtlas/TropEcoGeoJson/SouthwestGreaterAntilles_TropEco.geojson",
         layer = "southwestGreatAntGeojson", driver = "GeoJSON")
writeOGR(obj = eastGreatAntGeojson, dsn = "C:/Users/jc690391/Documents/AllenCoralAtlas/TropEcoGeoJson/EastGreaterAntilles_TropEco.geojson",
         layer = "eastGreatAntGeojson", driver = "GeoJSON")

#Open the Bahamian ecoregion file and split into smaller ones
bahamianGeojson <- rgdal::readOGR(dsn = "C:/AllenCoralAtlas/TropEcoGeoJson/Bahamian_TropEco.geojson") #5875 elements

bahamianExtent <- extent(bahamianGeojson)
bahamianLongSplit <- mean(c(bahamianExtent@xmin, bahamianExtent@xmax))

westBahamianGJ <- crop(bahamianGeojson, extent(-180, bahamianLongSplit, -1*tropicLat, tropicLat))
eastBahamianGJ <- crop(bahamianGeojson, extent(bahamianLongSplit, 180, -1*tropicLat, tropicLat))

westBahamianExtent <- extent(westBahamianGJ)
westBahamianLatSplit <- mean(c(westBahamianExtent@ymin, westBahamianExtent@ymax))

northwestBahamianGJ <- crop(westBahamianGJ, extent(-180, 180, westBahamianLatSplit, tropicLat))
southwestBahamianGJ <- crop(westBahamianGJ, extent(-180, 180, -1*tropicLat, westBahamianLatSplit))

eastBahamianExtent <- extent(eastBahamianGJ)
eastBahamianLatSplit <- mean(c(eastBahamianExtent@ymin, eastBahamianExtent@ymax))

northeastBahamianGJ <- crop(eastBahamianGJ, extent(-180, 180, eastBahamianLatSplit, tropicLat))
southeastBahamianGJ <- crop(eastBahamianGJ, extent(-180, 180, -1*tropicLat, eastBahamianLatSplit))

#Save these
writeOGR(obj = northwestBahamianGJ, dsn = "C:/AllenCoralAtlas/TropEcoGeoJson/NorthwestBahamian_TropEco.geojson",
         layer = "northwestBahamianGJ", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = southwestBahamianGJ, dsn = "C:/AllenCoralAtlas/TropEcoGeoJson/SouthwestBahamian_TropEco.geojson",
         layer = "southwestBahamianGJ", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = northeastBahamianGJ, dsn = "C:/AllenCoralAtlas/TropEcoGeoJson/NortheastBahamian_TropEco.geojson",
         layer = "northeastBahamianGJ", driver = "GeoJSON", overwrite_layer = T)
writeOGR(obj = southeastBahamianGJ, dsn = "C:/AllenCoralAtlas/TropEcoGeoJson/SoutheastBahamian_TropEco.geojson",
         layer = "southeastBahamianGJ", driver = "GeoJSON", overwrite_layer = T)
