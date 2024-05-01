#extract SST for sites 
#https://lukemiller.org/index.php/2014/11/extracting-noaa-sea-surface-temperatures-with-ncdf4/
#https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.html 
#Click through to download issues
#SST mean
source("scripts/NOAA_OISST_ncdf4.R") #see function for citation
site  <-  read.csv("data/SERF_SiteData.csv")

lonW = min(site$Site_Long)
lonE = max(site$Site_Long)
latS = min(site$Site_Lat)
latN = max(site$Site_Lat)

sst = extractOISSTweekly("data/sst.wkmean.1990-present.nc","data/lsmask.nc", 
                          lonW=-180, lonE=180,latS=-33,latN = 30,date1='1992-01-01',date2='2013-01-01')

#median sampling year +/- 5 years
data <-  read.csv("data/SERF_FishSizeData_TL.csv")
median(data$Year) # == 2008
max(data$Year)
min(data$Year)

#reshape
sst2 <- melt(sst)

#select sst from 2003 - 2013 (median = 2008+/-5)
sst2$Date <- as.character(sst2$Date)
sst2$year <- as.numeric(str_sub(sst2$Date, 1, 4))
sst2 <- filter(sst2, year > 2002)
sst2 <- filter(sst2, year < 2014)

#get mean annual values for each lat/long
sst3 <- sst2 %>% group_by(Lat, Long) %>%
  summarise(meanSST = mean(value),
            sdSST = sd(value))
sst3 <- filter(sst3, !is.na(meanSST))

#merge by distance
sst4 <- subset(sst3, select = c("Lat", "Long"))
site2 <- subset(site, select = c("Site_Lat", "Site_Long"))
sst4sp <- SpatialPoints(sst4)
site2sp <- SpatialPoints(site2)
site2sp$nearest <- apply(gDistance(sst4sp, site2sp, byid=TRUE), 1, which.min)
site2sp <- as.data.frame(site2sp)

missing <- filter(site2sp, is.na(nearest) )

sst5 <- tibble::rowid_to_column(sst3)
site2_sst <- merge.data.frame(sst5, site2sp, by.x = "rowid", by.y = "nearest")
missing <- filter(site2_sst, is.na(meanSST) )


site2_sst <- cbind(site2_sst, site)
site2_sst <- subset(site2_sst, select=which(!duplicated(names(site2_sst)))) 

#site2_sst <- merge.data.frame(site2_sst, site, by = c("Site_Lat", "Site_Long"))
str(as.factor(site2_sst$UniqueSite))
missing <- filter(site2_sst, is.na(meanSST) )


##change lat long to lat_sst, long_sst
site_sst <- rename(site2_sst, lat_sst = Lat)
site_sst <- rename(site_sst, long_sst = Long)
site_sst <- unique(site_sst)


#save
saveRDS(site_sst, "data/processed/SERF_site_sst.rds")
