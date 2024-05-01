#extrapolate missing spp
missing <- readRDS("data/processed/lm_temp_missing_sp_pred.rds")
site_sst <- readRDS(file= "data/processed/SERF_site_sst.rds")
check <- filter(missing, UniqueSite %!in% site_sst$UniqueSite)

#get sst for missing spp
missing <- subset(missing, select = c("genus_sp", "UniqueSite"))
filled <- left_join(missing, site_sst)

#species and temps to be extrapolated
new_extrap_missing <- subset(filled, select = c("meanSST", "genus_sp"))
new_extrap_missing <- unique(new_extrap_missing)

#save
saveRDS(new_extrap_missing, "data/processed/new_extrap_missing.rds")
