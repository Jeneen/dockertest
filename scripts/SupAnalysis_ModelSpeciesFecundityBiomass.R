#set cores
options(mc.cores = parallel::detectCores())

#data
relDat <- readRDS( "output/SizeProportions_site_fecundity_med1000samp.rds")

### combine all size categories and run a logistic regression
#Proportional biomass of size classes (all fish, not just MATF) 
relSizeDat$LogMeanAllFishBiomassKgHa <- log(relSizeDat$MeanAllFishBiomassKgHa + 1)
relSizeDat$sLogMeanAllFishBiomassKgHa <- standardize(relSizeDat$LogMeanAllFishBiomassKgHa)
relSizeDat$PropSmallBiom[relSizeDat$PropSmallBiom>1] <- 1 # for 1.0000001 values

#check they all sum to 1
relSizeDat$sum <- relSizeDat$PropLargeBiom + relSizeDat$PropMedBiom + relSizeDat$PropSmallBiom

#from wide to long
relSizeDat_long <- gather(relSizeDat, size_cat, proportion, PropSmallBiom:PropLargeBiom, factor_key=TRUE)
relSizeDat_long$size_cat <- factor(relSizeDat_long$size_cat, c("PropSmallBiom", "PropMedBiom", "PropLargeBiom"))

relSizeDat_long2 <- gather(relSizeDat, size_cat, biomassSizeCat, SmallMeanAllFishBiomassKgHa:LargeMeanAllFishBiomassKgHa,
                           factor_key=TRUE)
relSizeDat_long2$size_cat <- factor(relSizeDat_long2$size_cat, c("SmallMeanAllFishBiomassKgHa", 
                                                                 "MedMeanAllFishBiomassKgHa", 
                                                                 "LargeMeanAllFishBiomassKgHa"))


# Fit the model
logisticBfit <- brm(size_cat | weights(proportion) ~ sLogMeanAllFishBiomassKgHa + DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                      sRegional_population_growth+sOcean_prod+sClimate_stress+
                      sLarger_pop_size+sReef_fish_landings_per_km2+
               (1|Larger/ReefCluster), family = categorical (link = logit ), data =  relSizeDat_long,
               control = list(adapt_delta = 0.9),
               iter=10000,  warmup=9000, seed =1,
               chains=4)
summary(logisticBfit)
pp_check(logisticBfit,nsamples=100, type = "bars") # this doesn't account for weights
plot(logisticBfit)

#save
saveRDS(logisticBfit, "output/logisticSize_allfec_biomass_model.rds")



###############################Proportional abundance of fecundity classes##################################################

relFDat <- readRDS("output/FecundityClassProportions_site_fecundity_med1000samp.rds")
relFDat$LogMeanAllFishBiomassKgHa <- log(relFDat$MeanAllFishBiomassKgHa + 1)
relFDat$sLogMeanAllFishBiomassKgHa <- standardize(relFDat$LogMeanAllFishBiomassKgHa)

#recalculate proportions as prop of total abundance of mat F (not total abundance)
relFDat$propAbHighFec <- relFDat$HighCAbundance/relFDat$TotalAbundFec
relFDat$propAbMedFec <- relFDat$MedCAbundance/relFDat$TotalAbundFec
relFDat$propAbLowFec <- relFDat$LowCAbundance/relFDat$TotalAbundFec
relFDat$sum <- relFDat$propAbLowFec + relFDat$propAbHighFec + relFDat$propAbMedFec

#from wide to long
relFDat_long <- gather(relFDat, fec_cat, proportion, propAbLowFec:propAbHighFec, factor_key=TRUE)
relFDat_long$fec_cat <- factor(relFDat_long$fec_cat, c("propAbLowFec", "propAbMedFec", "propAbHighFec"))


# Fit the model
logisticFecAbMFfit <- brm(fec_cat | weights(proportion) ~ sLogMeanAllFishBiomassKgHa +DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                      sRegional_population_growth+sOcean_prod+sClimate_stress+
                      sHDI+sLarger_pop_size+sReef_fish_landings_per_km2+
                      (1|Larger/ReefCluster), family = categorical (link = logit ), data =  relFDat_long,
                    control = list(adapt_delta = 0.9),
                    iter=10000,  warmup=9000, seed =1,
                    chains=4)
summary(logisticFecAbMFfit)
pp_check(logisticFecAbMFfit,nsamples=100, type = "bars") # this doesn't account for weights
plot(logisticFecAbMFfit)

#save
saveRDS(logisticFecAbMFfit, "output/logisticFecAbund_model.rds")




#####################################################################################################
#proportion of fecund female biomass 

relFDat <- readRDS("output/FecundityClassProportions_site_fecundity_med1000samp.rds")
relFDat$LogMeanAllFishBiomassKgHa <- log(relFDat$MeanAllFishBiomassKgHa + 1)
relFDat$sLogMeanAllFishBiomassKgHa <- standardize(relFDat$LogMeanAllFishBiomassKgHa)


relFDat$TotalBiomFec <- relFDat$HighCBiomass + relFDat$MedCBiomass + relFDat$LowCBiomass
relFDat$propBiomHighFec <- relFDat$HighCBiomass/relFDat$TotalBiomFec
relFDat$propBiomMedFec <- relFDat$MedCBiomass/relFDat$TotalBiomFec
relFDat$propBiomLowFec <-relFDat$LowCBiomass/relFDat$TotalBiomFec
relFDat$sum <- relFDat$propBiomLowFec + relFDat$propBiomHighFec + relFDat$propBiomMedFec


#from wide to long
relFDatB_long <- gather(relFDat, fec_cat, proportion, c(propBiomLowFec, propBiomMedFec, propBiomHighFec), factor_key=TRUE)
relFDatB_long$fec_cat <- factor(relFDatB_long$fec_cat, c("propBiomLowFec", "propBiomMedFec", "propBiomHighFec"))


# Fit the model
logisticFecBiomMFfit <- brm(fec_cat | weights(proportion) ~ sLogMeanAllFishBiomassKgHa +DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                            sRegional_population_growth+sOcean_prod+sClimate_stress+
                            sHDI+sLarger_pop_size+sReef_fish_landings_per_km2+
                            (1|Larger/ReefCluster), family = categorical (link = logit ), data =  relFDatB_long,
                          control = list(adapt_delta = 0.9),
                          iter=10000,  warmup=9000, seed =1,
                          chains=4)
summary(logisticFecBiomMFfit)
pp_check(logisticFecBiomMFfit,nsamples=100, type = "bars")
plot(logisticFecBiomMFfit)

#save
saveRDS(logisticFecBiomMFfit, "output/logisticFecBiom_model.rds")


#####################################################################################################
#proportion mature females
#data
biomPropMature <- readRDS("output/proportionMature.rds")

#prep vars
biomPropMature$LogMeanAllFishBiomassKgHa <- log(biomPropMature$MeanAllFishBiomassKgHa +1)
biomPropMature$sLogMeanAllFishBiomassKgHa <- standardize(biomPropMature$LogMeanAllFishBiomassKgHa)

#convert to proportion for beta
proportionMatureFit <- brm(mean_prop_mature ~ sLogMeanAllFishBiomassKgHa + DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                              sRegional_population_growth+sOcean_prod+sClimate_stress+
                              sLarger_pop_size+sReef_fish_landings_per_km2+
                              (1|Larger/ReefCluster), family = Beta(link = "logit"), data =  biomPropMature,
                            control = list(adapt_delta = 0.9),
                            iter=10000,  warmup=9000, seed =1,
                            chains=4)

summary(proportionMatureFit)
plot(proportionMatureFit)
pp_check(proportionMatureFit)

#save
saveRDS(proportionMatureFit , "output/proportionMatureFit.rds")

