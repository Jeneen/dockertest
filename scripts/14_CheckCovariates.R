#data
alldata <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")

#######
VIF.table=as.data.frame(vif.mer(lmer(LogFecundityMean~ LogBiomassKGMean +
                                       DepthCategory+ CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                                       sRegional_population_growth+sOcean_prod+sClimate_stress+
                                       sHDI+sLarger_pop_size+sReef_fish_landings_per_km2+(1|Larger/ReefCluster), data=alldata)))
colnames(VIF.table)="VIF"
print(VIF.table)

pairs(~ LogBiomassKGMean +
        DepthCategory+ CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
        sRegional_population_growth+sOcean_prod+sClimate_stress+
        sHDI+sLarger_pop_size+sReef_fish_landings_per_km2, data= alldata,lower.panel=panel.cor )


