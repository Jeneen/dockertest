#data
alldata <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")

# Add placeholder for very small biomass SD
alldata$LogBiomassKGSD <- ifelse(alldata$LogBiomassKGSD <= 0, .Machine$double.eps, alldata$LogBiomassKGSD)


#run measurement error model with default priors

fec_biom_se <- brm(LogFecundityMean | se(LogFecunditySD, sigma = TRUE) ~ 1 + 
                     (1|Larger/ReefCluster) +
                     LogBiomassKGMean, 
                  prior = c(prior(normal(0, 1), class = "b") + #weak prior
                         prior(normal(18.3, 2.5), class = "Intercept") +  #median of data
                         prior(student_t(3, 0, 2.5), class = "sd") + #default prior
                         prior(student_t(3, 0, 2.5), class = "sigma")), #default prior
                   data=alldata,family=gaussian(),
                   iter=10000,  warmup=9000, 
                   chains=4, cores = 4, seed = 153) 

#save
saveRDS(fec_biom_se, file="output/site_fec_biomass_se.rds")
