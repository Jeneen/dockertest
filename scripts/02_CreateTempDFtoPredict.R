# load data
site_sst <- readRDS("data/processed/SERF_site_sst.rds")
data <-  read.csv("data/SERF_FishSizeData_TL.csv")


##create df of species and temperatures to predict
site <- subset(site_sst, select = c(UniqueSite, meanSST, sdSST))
data2 <- inner_join(site, data)
new_extrap <- subset(data2, select = c(meanSST, genus_sp_correct))
new_extrap$genus_sp <-new_extrap$genus_sp_correct
new_extrap <- subset(new_extrap, select = c(meanSST, genus_sp))

#save
saveRDS(new_extrap, "data/processed/new_extrap.rds")
