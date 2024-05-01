#set cores
options(mc.cores = parallel::detectCores())

##################################################################
##                  Biomass > 20cm                             ##
##################################################################
#data
alldata <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
alldata$LogBiomass_above20cm=log(alldata$Biomass_above20cm+1)


#default priors (intercept prior = median of the response from the data)
# b is flat prior
priors <- prior(student_t(3, 5.6, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd") + prior(student_t(3, 0, 2.5), class = "sigma")
B20_mod =brm(LogBiomass_above20cm~
                        DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                        sRegional_population_growth+sOcean_prod+sClimate_stress+
                        sLarger_pop_size+sReef_fish_landings_per_km2+
                        (1|Larger/ReefCluster),
                      data=alldata,family=gaussian,prior = priors,
                      iter=10000,  warmup=9000,
                      chains=4, seed =1) 
saveRDS(B20_mod, "output/brms_20cmBiomass.rds")




##################################################################
##                  Fecundity                                  ##
##################################################################
#measurement error model
#https://bookdown.org/content/3890/missing-data-and-other-opportunities.html
priors <- prior(student_t(3, 18.3, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd") + prior(student_t(3, 0, 2.5), class = "sigma")
fec_mod_se =brm(LogFecundityMean | se(LogFecunditySD, sigma = TRUE)~
                  DepthCategory+CleanHabitat+Protection+CensusMethod+
                  sTotal_sampling_area+sgrav_tot2+
                  sRegional_population_growth+sOcean_prod+sClimate_stress+
                  sLarger_pop_size+sReef_fish_landings_per_km2+
                  (1|Larger/ReefCluster),
                data=alldata,family=gaussian(), prior =priors, 
                iter=10000,  warmup=9000,
                chains=4, seed =1) 

saveRDS(fec_mod_se, "output/brms_fecundity_se.rds")



##################################################################
##                  Mature female biomass                       ##
##################################################################
#default brms priors
priors <-  prior(student_t(3, 5.2, 2.5), class = "Intercept") +  
  prior(student_t(3, 0, 2.5), class = "sd")+prior(student_t(3, 0, 2.5), class = "sigma")
matf_mod_se =brm(LogBiomassKGMean | se(LogBiomassKGSD, sigma = TRUE)~
                  DepthCategory+CleanHabitat+Protection+CensusMethod+
                  sTotal_sampling_area+sgrav_tot2+
                  sRegional_population_growth+sOcean_prod+sClimate_stress+
                  sLarger_pop_size+sReef_fish_landings_per_km2+
                  (1|Larger/ReefCluster),
                data=alldata,family=gaussian(),prior = priors,
                iter=10000,  warmup=9000,
                chains=4, seed =1) 
saveRDS(matf_mod_se, "output/brms_matf_se.rds")


##################################################################
##                          Serranidae                          ##
##################################################################
alldata <- readRDS("output/SERRANIDAE_alldata_clean_cinner2020.rda")

#can't have measurement error included in hurdle lognormal
#original without ME
priors <-  prior(student_t(3, 16.6, 4.6), class = "Intercept") + 
  prior(student_t(3, 0, 4.6) , class = "sd") +prior(student_t(3, 0, 4.6), class = "sigma")
fec_model_ser = brm(FecundityMean ~
                          DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+
                      sgrav_tot2+
                          sRegional_population_growth+sOcean_prod+sClimate_stress+
                          sLarger_pop_size+sReef_fish_landings_per_km2+
                          (1|Larger/ReefCluster), 
                        data=alldata, prior = priors,
                        family=hurdle_lognormal(link = "identity", link_sigma = "log",
                                                             link_hu = "logit"),
                        iter=10000,  warmup=9000,chains=4, seed = 1) 


saveRDS(fec_model_ser, "output/SERRANIDAE_brms_fecundity_hurdlelog.rds")



#split hurdle model
# Create a binary variable: 1 if FecundityMean > 0, 0 otherwise
alldata$FecundityBinary <- as.numeric(alldata$FecundityMean > 0)

# Fit the logistic regression model
priors <-  prior(student_t(3, 0, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd")
hurdle_model <- brm(
  FecundityBinary ~ DepthCategory + CleanHabitat + Protection + CensusMethod +
    sTotal_sampling_area + sgrav_tot2 +
    sRegional_population_growth + sOcean_prod + sClimate_stress +
    sLarger_pop_size + sReef_fish_landings_per_km2 +
    (1 | Larger/ReefCluster),
  data = alldata, prior = priors,
  family = bernoulli(link = "logit"),
  iter = 10000, warmup = 9000, chains = 4, seed = 1
)


saveRDS(hurdle_model, "output/SERRANIDAE_brms_fecundity_zeros_hurdle.rds")


# Subset data to include only positive FecundityMean
positive_data <- subset(alldata, FecundityMean > 0)

# Fit the normal model
#default brms priors
priors <-  prior(student_t(3, 18.2, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd") +prior(student_t(3, 0, 2.5), class = "sigma")
normal_model <- brm(
  LogFecundityMean | se(LogFecunditySD, sigma = TRUE)~ DepthCategory + CleanHabitat + Protection + CensusMethod +
    sTotal_sampling_area + sgrav_tot2 +
    sRegional_population_growth + sOcean_prod + sClimate_stress +
    sLarger_pop_size + sReef_fish_landings_per_km2 +
    (1 | Larger/ReefCluster), prior = priors,
  data = positive_data,
  family=gaussian(),
  iter = 10000, warmup = 9000, chains = 4, seed =1
)


#final model = removed sites with 0
saveRDS(normal_model, "output/SERRANIDAE_brms_fecundity.rds")



##################################################################
##                          Scaridae                            ##
##################################################################
#data
alldata <- readRDS("output/SCARIDAE_alldata_clean_cinner2020.rda")

# Subset data to include only positive FecundityMean
positive_data <- subset(alldata, FecundityMean > 0)

#hurdle log-normal
priors <-  prior(student_t(3, 18.1, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd") +prior(student_t(3, 0, 2.5), class = "sigma")
scaridae_hurdlelog = brm(FecundityMean~
                          DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                          sRegional_population_growth+sOcean_prod+sClimate_stress+
                          sLarger_pop_size+sReef_fish_landings_per_km2+
                          (1|Larger/ReefCluster),
                        data=alldata,family=hurdle_lognormal(link = "identity", link_sigma = "log",
                                                             link_hu = "logit"),
                      
                        iter=10000,  warmup=9000,prior = priors,
                        chains=4, seed =1) 

saveRDS(scaridae_hurdlelog, 
        "output/SCARIDAE_brms_fecundity_hurdlelog.rds")


#no zeros model
#default brms priors
priors <-  prior(student_t(3, 18.4, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd")
scaridae_model <- brm(
  LogFecundityMean | se(LogFecunditySD, sigma = TRUE)~ DepthCategory + CleanHabitat + Protection + CensusMethod +
    sTotal_sampling_area + sgrav_tot2 +
    sRegional_population_growth + sOcean_prod + sClimate_stress +
    sLarger_pop_size + sReef_fish_landings_per_km2 +
    (1 | Larger/ReefCluster),
  data = positive_data, prior = priors,
  family=gaussian(),
  iter = 10000, warmup = 9000, chains = 4, seed = 1
)



#final model = removed sites with 0
saveRDS(scaridae_model, "output/SCARIDAE_brms_fecundity.rds")


##################################################################
##                          Lutjanidae                          ##
##################################################################

#data
alldata <- readRDS("output/LUTJANIDAE_alldata_clean_cinner2020.rda")

# Subset data to include only positive FecundityMean
positive_data <- subset(alldata, FecundityMean > 0)

#hurdle log-normal
priors <-  prior(student_t(3, 16.1, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd")
lutjanidae_hurdlelog = brm(FecundityMean~
                           DepthCategory+CleanHabitat+Protection+CensusMethod+sTotal_sampling_area+sgrav_tot2+
                           sRegional_population_growth+sOcean_prod+sClimate_stress+
                           sLarger_pop_size+sReef_fish_landings_per_km2+
                           (1|Larger/ReefCluster),
                         data=alldata,family=hurdle_lognormal(link = "identity", link_sigma = "log",
                                                              link_hu = "logit"),
                         iter=10000,  warmup=9000, prior = priors,
                         chains=4, seed = 1) 

saveRDS(lutjanidae_hurdlelog, 
        "output/LUTJANIDAE_brms_fecundity_hurdlelog.rds")


#no zeros model
#default brms priors
priors <-  prior(student_t(3, 16.1, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "sd")+prior(student_t(3, 0, 2.5), class = "sigma")

lutjanidae_model <- brm(
  LogFecundityMean | se(LogFecunditySD, sigma = TRUE)~ DepthCategory + CleanHabitat + Protection + CensusMethod +
    sTotal_sampling_area + sgrav_tot2 +
    sRegional_population_growth + sOcean_prod + sClimate_stress +
    sLarger_pop_size + sReef_fish_landings_per_km2 +
    (1 | Larger/ReefCluster),
  data = positive_data,
  family=gaussian(), prior = priors,
  iter = 10000, warmup = 9000, chains = 4, seed = 1
)


#final model = removed sites with 0
saveRDS(lutjanidae_model, "output/LUTJANIDAE_brms_fecundity.rds")



