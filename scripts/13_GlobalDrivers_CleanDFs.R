#data
site_fecundity <- readRDS("output/siteFecundBiomass_1000.rds")
alldata_df=read.csv("data/SERF_SiteData.csv", header = T)

#merge
merge_with_alldata <- function(df) {
  merge(df, alldata_df, by = "UniqueSite", all = TRUE)}

# Apply the function to each dataframe in the site_fecundity list
alldata <- lapply(site_fecundity, merge_with_alldata)

#get full site list 
sites_all <- site_fecundity %>%
  lapply(function(df) {
    if ("UniqueSite" %in% names(df)) {
      data.frame(UniqueSite = df[["UniqueSite"]])
    } else {
      data.frame(UniqueSite = NA)  # Return a data frame with NA if the column is not present
    }
  }) %>%
  bind_rows() %>%
  distinct()

#check missing sites 
final_sample <- as.data.frame(site_fecundity[100])
filter(sites_all,  UniqueSite %!in% final_sample$UniqueSite) 

##################################################################################
# Just create 1 df with se of the response
# Combine all dataframes in the list into one
combined_df <- bind_rows(site_fecundity)

# Transform meanFecundity and meanBiomassKG using log(x + 1)
combined_df <- combined_df %>%
  mutate(
    logMeanFecundity = log(meanFecundity + 1),
    logMeanBiomassKG = log(meanBiomassKG + 1)
  )


#look at distributions
combined_df %>%
  filter(UniqueSite == 4)%>%
  ggplot(aes(x = logMeanFecundity)) +
  geom_density() +
  facet_wrap(~ UniqueSite) +
  theme_minimal()

# Summarize the combined dataframe
final_summary <- combined_df %>%
  group_by(UniqueSite) %>%
  summarise(
    FecundityMean = mean(meanFecundity, na.rm = TRUE),
    FecunditySD = sd(meanFecundity, na.rm = TRUE),
    Fecundity5th = quantile(meanFecundity, probs = 0.05, na.rm = TRUE),
    Fecundity95th = quantile(meanFecundity, probs = 0.95, na.rm = TRUE),
    LogFecundityMean = mean(logMeanFecundity, na.rm = TRUE),
    LogFecunditySD = sd(logMeanFecundity, na.rm = TRUE),
    LogFecundity5th = quantile(logMeanFecundity, probs = 0.05, na.rm = TRUE),
    LogFecundity95th = quantile(logMeanFecundity, probs = 0.95, na.rm = TRUE),
    BiomassKGMean = mean(meanBiomassKG, na.rm = TRUE),
    BiomassKGSD = sd(meanBiomassKG, na.rm = TRUE),
    BiomassKG5th = quantile(meanBiomassKG, probs = 0.05, na.rm = TRUE),
    BiomassKG95th = quantile(meanBiomassKG, probs = 0.95, na.rm = TRUE),
    LogBiomassKGMean = mean(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKGSD = sd(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKG5th = quantile(logMeanBiomassKG, probs = 0.05, na.rm = TRUE),
    LogBiomassKG95th = quantile(logMeanBiomassKG, probs = 0.95, na.rm = TRUE)
  )


#prep covariates (adapted from https://github.com/JZamborain-Mason/Cinneretal_2020_multiplegoals)
alldata <- merge.data.frame(final_summary, alldata_df, by = "UniqueSite", all = FALSE)
missing <- filter(alldata, is.na(FecundityMean) )

#mpa size and age restrictions
alldata$mpacondition=ifelse(alldata$Protection=="UnfishedHigh" &(alldata$MPAage>4|alldata$MPAage==4) & (alldata$NTZarea>2 |alldata$NTZarea==0),1,0)
alldata$keep=ifelse(!alldata$Protection=="UnfishedHigh",1,ifelse(alldata$Protection=="UnfishedHigh" &alldata$mpacondition==1,1,0))

#reef-scale covariates
alldata$DepthCategory=relevel(factor(alldata$DepthCategory),ref="4-10m")
alldata$CleanHabitat=relevel(factor(alldata$CleanHabitat),ref="Slope")
alldata$Protection=relevel(factor(alldata$Protection),ref="Fished")
alldata$CensusMethod=relevel(factor(alldata$CensusMethod),ref="Standard belt transect")
alldata$sTotal_sampling_area=standardize(log(alldata$Total_sampling_area))

#gravities with different exponents
alldata$sgrav_tot1=standardize(log(alldata$gravtot5001+min(alldata$gravtot5001[alldata$gravtot5001>0])))
alldata$sgrav_tot2=standardize(log(alldata$gravtot5002+min(alldata$gravtot5002[alldata$gravtot5002>0])))
alldata$sgrav_tot3=standardize(log(alldata$gravtot5003+min(alldata$gravtot5003[alldata$gravtot5003>0])))

#reef cluster-scale covariates
alldata$sOcean_prod=standardize(log(alldata$Ocean_prod))
alldata$sClimate_stress=standardize(alldata$Climate_stress)
alldata$sRegional_population_growth=standardize(alldata$Regional_population_growth)

#nation/state-scale covariates
alldata$sReef_fish_landings_per_km2=standardize(log(alldata$Reef_fish_landings_per_km2+1))
alldata$sLarger_pop_size=standardize(log(alldata$Larger_pop_size+1))
alldata$sHDI=standardize(alldata$HDI)


#remove na
alldata <- filter(alldata, !is.na(FecundityMean))


#Remove tanzania sites (unvalidated data)
alldata <- filter(alldata, UniqueSite %!in% c("2889","2890","2891","2892","2893", "2894", "2895", "2896", "2899", "2900"))

#save
saveRDS(alldata, "output/alldata_SDandCIof1000_clean_cinner2020.rda")





##################################################################
##                          Serranidae                          ##
##################################################################
#data
site_fecundity <- readRDS("output/SERRANIDAE_siteFecundBiomass_1000.rds")
alldata_df=read.csv("data/SERF_SiteData.csv", header = T)

# Apply the function to each dataframe in the site_fecundity list
alldata <- lapply(site_fecundity, merge_with_alldata)

#get full site list 
sites_all <- site_fecundity %>%
  lapply(function(df) {
    if ("UniqueSite" %in% names(df)) {
      data.frame(UniqueSite = df[["UniqueSite"]])
    } else {
      data.frame(UniqueSite = NA)  # Return a data frame with NA if the column is not present
    }
  }) %>%
  bind_rows() %>%
  distinct()

#check missing sites 
final_sample <- as.data.frame(site_fecundity[100])
filter(sites_all,  UniqueSite %!in% final_sample$UniqueSite)

# Combine all dataframes in the list into one
combined_df <- bind_rows(site_fecundity)

# Transform meanFecundity and meanBiomassKG using log(x + 1)
combined_df <- combined_df %>%
  mutate(
    logMeanFecundity = log(meanFecundity + 1),
    logMeanBiomassKG = log(meanBiomassKG + 1)
  )


#look at distributions
combined_df %>%
  filter(UniqueSite == 4)%>%
  ggplot(aes(x = logMeanFecundity)) +
  geom_density() +
  facet_wrap(~ UniqueSite) +
  theme_minimal()

# Summarize the combined dataframe
final_summary <- combined_df %>%
  group_by(UniqueSite) %>%
  summarise(
    FecundityMean = mean(meanFecundity, na.rm = TRUE),
    FecunditySD = sd(meanFecundity, na.rm = TRUE),
    Fecundity5th = quantile(meanFecundity, probs = 0.05, na.rm = TRUE),
    Fecundity95th = quantile(meanFecundity, probs = 0.95, na.rm = TRUE),
    LogFecundityMean = mean(logMeanFecundity, na.rm = TRUE),
    LogFecunditySD = sd(logMeanFecundity, na.rm = TRUE),
    LogFecundity5th = quantile(logMeanFecundity, probs = 0.05, na.rm = TRUE),
    LogFecundity95th = quantile(logMeanFecundity, probs = 0.95, na.rm = TRUE),
    BiomassKGMean = mean(meanBiomassKG, na.rm = TRUE),
    BiomassKGSD = sd(meanBiomassKG, na.rm = TRUE),
    BiomassKG5th = quantile(meanBiomassKG, probs = 0.05, na.rm = TRUE),
    BiomassKG95th = quantile(meanBiomassKG, probs = 0.95, na.rm = TRUE),
    LogBiomassKGMean = mean(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKGSD = sd(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKG5th = quantile(logMeanBiomassKG, probs = 0.05, na.rm = TRUE),
    LogBiomassKG95th = quantile(logMeanBiomassKG, probs = 0.95, na.rm = TRUE)
  )



alldata <- merge.data.frame(final_summary, alldata_df, by = "UniqueSite", all =FALSE)
missing <- filter(alldata, is.na(FecundityMean) )

#mpa size and age restrictions
alldata$mpacondition=ifelse(alldata$Protection=="UnfishedHigh" &(alldata$MPAage>4|alldata$MPAage==4) & (alldata$NTZarea>2 |alldata$NTZarea==0),1,0)
alldata$keep=ifelse(!alldata$Protection=="UnfishedHigh",1,ifelse(alldata$Protection=="UnfishedHigh" &alldata$mpacondition==1,1,0))

##Explanatory variables: relevel categorical variables and standardize continuous variables
#reef-scale covariates
alldata$DepthCategory=relevel(factor(alldata$DepthCategory),ref="4-10m")
alldata$CleanHabitat=relevel(factor(alldata$CleanHabitat),ref="Slope")
alldata$Protection=relevel(factor(alldata$Protection),ref="Fished")
alldata$CensusMethod=relevel(factor(alldata$CensusMethod),ref="Standard belt transect")
alldata$sTotal_sampling_area=standardize(log(alldata$Total_sampling_area))

#gravities with different exponents
alldata$sgrav_tot1=standardize(log(alldata$gravtot5001+min(alldata$gravtot5001[alldata$gravtot5001>0])))
alldata$sgrav_tot2=standardize(log(alldata$gravtot5002+min(alldata$gravtot5002[alldata$gravtot5002>0])))
alldata$sgrav_tot3=standardize(log(alldata$gravtot5003+min(alldata$gravtot5003[alldata$gravtot5003>0])))

#reef cluster-scale covariates
alldata$sOcean_prod=standardize(log(alldata$Ocean_prod))
alldata$sClimate_stress=standardize(alldata$Climate_stress)
alldata$sRegional_population_growth=standardize(alldata$Regional_population_growth)

#nation/state-scale covariates
alldata$sReef_fish_landings_per_km2=standardize(log(alldata$Reef_fish_landings_per_km2+1))
alldata$sLarger_pop_size=standardize(log(alldata$Larger_pop_size+1))
alldata$sHDI=standardize(alldata$HDI)


#remove na
alldata <- filter(alldata, !is.na(FecundityMean))


#remove sites
alldata <- filter(alldata, UniqueSite %!in% c("2889","2890","2891","2892","2893", 
                                              "2894", "2895", "2896", "2899", "2900"))

#add remainder of sites as 0
full_dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
full_dat <- subset(full_dat, 
                   select = -c(FecundityMean, FecunditySD, Fecundity5th,
                                         Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                                         LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                                         LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))
sub_dat <- subset(alldata, 
                  select = c(UniqueSite, FecundityMean, FecunditySD, Fecundity5th,
                              Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                              LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                              LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))

alldata2 <- left_join(full_dat, sub_dat)
alldata2 <- alldata2 %>%
  mutate(across(c(FecundityMean, FecunditySD, Fecundity5th, Fecundity95th,
                  LogFecundityMean, LogFecunditySD, LogFecundity5th, LogFecundity95th,
                  BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                  LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th),
                ~replace(., is.na(.), 0)))

#save
saveRDS(alldata2, "output/SERRANIDAE_alldata_clean_cinner2020.rda")




##################################################################
##                          Lutjanidae                          ##
##################################################################
#data
site_fecundity <- readRDS("output/LUTJANIDAE_siteFecundBiomass_1000.rds")
alldata_df=read.csv("data/SERF_SiteData.csv", header = T)


merge_with_alldata <- function(df) {
  merge(df, alldata_df, by = "UniqueSite", all = TRUE)}

# Apply the function to each dataframe in the site_fecundity list
alldata <- lapply(site_fecundity, merge_with_alldata)

#get full site list 
sites_all <- site_fecundity %>%
  lapply(function(df) {
    if ("UniqueSite" %in% names(df)) {
      data.frame(UniqueSite = df[["UniqueSite"]])
    } else {
      data.frame(UniqueSite = NA)  # Return a data frame with NA if the column is not present
    }
  }) %>%
  bind_rows() %>%
  distinct()

#check missing sites 
final_sample <- as.data.frame(site_fecundity[100])
filter(sites_all,  UniqueSite %!in% final_sample$UniqueSite)

# Combine all dataframes in the list into one
combined_df <- bind_rows(site_fecundity)

# Transform meanFecundity and meanBiomassKG using log(x + 1)
combined_df <- combined_df %>%
  mutate(
    logMeanFecundity = log(meanFecundity + 1),
    logMeanBiomassKG = log(meanBiomassKG + 1)
  )




# Summarize the combined dataframe
final_summary <- combined_df %>%
  group_by(UniqueSite) %>%
  summarise(
    FecundityMean = mean(meanFecundity, na.rm = TRUE),
    FecunditySD = sd(meanFecundity, na.rm = TRUE),
    Fecundity5th = quantile(meanFecundity, probs = 0.05, na.rm = TRUE),
    Fecundity95th = quantile(meanFecundity, probs = 0.95, na.rm = TRUE),
    LogFecundityMean = mean(logMeanFecundity, na.rm = TRUE),
    LogFecunditySD = sd(logMeanFecundity, na.rm = TRUE),
    LogFecundity5th = quantile(logMeanFecundity, probs = 0.05, na.rm = TRUE),
    LogFecundity95th = quantile(logMeanFecundity, probs = 0.95, na.rm = TRUE),
    BiomassKGMean = mean(meanBiomassKG, na.rm = TRUE),
    BiomassKGSD = sd(meanBiomassKG, na.rm = TRUE),
    BiomassKG5th = quantile(meanBiomassKG, probs = 0.05, na.rm = TRUE),
    BiomassKG95th = quantile(meanBiomassKG, probs = 0.95, na.rm = TRUE),
    LogBiomassKGMean = mean(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKGSD = sd(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKG5th = quantile(logMeanBiomassKG, probs = 0.05, na.rm = TRUE),
    LogBiomassKG95th = quantile(logMeanBiomassKG, probs = 0.95, na.rm = TRUE)
  )



alldata <- merge.data.frame(final_summary, alldata_df, by = "UniqueSite", all =FALSE)
#alldata <- collapsed_alldata
missing <- filter(alldata, is.na(FecundityMean) )

#mpa size and age restrictions
alldata$mpacondition=ifelse(alldata$Protection=="UnfishedHigh" &(alldata$MPAage>4|alldata$MPAage==4) & (alldata$NTZarea>2 |alldata$NTZarea==0),1,0)
alldata$keep=ifelse(!alldata$Protection=="UnfishedHigh",1,ifelse(alldata$Protection=="UnfishedHigh" &alldata$mpacondition==1,1,0))


##Explanatory variables: relevel categorical variables and standardize continuous variables

#reef-scale covariates
alldata$DepthCategory=relevel(factor(alldata$DepthCategory),ref="4-10m")
alldata$CleanHabitat=relevel(factor(alldata$CleanHabitat),ref="Slope")
alldata$Protection=relevel(factor(alldata$Protection),ref="Fished")
alldata$CensusMethod=relevel(factor(alldata$CensusMethod),ref="Standard belt transect")
alldata$sTotal_sampling_area=standardize(log(alldata$Total_sampling_area))

#gravities with different exponents
alldata$sgrav_tot1=standardize(log(alldata$gravtot5001+min(alldata$gravtot5001[alldata$gravtot5001>0])))
alldata$sgrav_tot2=standardize(log(alldata$gravtot5002+min(alldata$gravtot5002[alldata$gravtot5002>0])))
alldata$sgrav_tot3=standardize(log(alldata$gravtot5003+min(alldata$gravtot5003[alldata$gravtot5003>0])))

#reef cluster-scale covariates
alldata$sOcean_prod=standardize(log(alldata$Ocean_prod))
alldata$sClimate_stress=standardize(alldata$Climate_stress)
alldata$sRegional_population_growth=standardize(alldata$Regional_population_growth)

#nation/state-scale covariates
alldata$sReef_fish_landings_per_km2=standardize(log(alldata$Reef_fish_landings_per_km2+1))
alldata$sLarger_pop_size=standardize(log(alldata$Larger_pop_size+1))
alldata$sHDI=standardize(alldata$HDI)


#remove na
alldata <- filter(alldata, !is.na(FecundityMean))


#remove sites
alldata <- filter(alldata, UniqueSite %!in% c("2889","2890","2891","2892","2893", "2894", "2895", "2896", "2899", "2900"))


#only 690 sites, add remainder of sites as 0
full_dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
full_dat <- subset(full_dat, 
                   select = -c(FecundityMean, FecunditySD, Fecundity5th,
                               Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                               LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                               LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))
sub_dat <- subset(alldata, 
                  select = c(UniqueSite, FecundityMean, FecunditySD, Fecundity5th,
                             Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                             LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                             LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))

alldata2 <- left_join(full_dat, sub_dat)
alldata2 <- alldata2 %>%
  mutate(across(c(FecundityMean, FecunditySD, Fecundity5th, Fecundity95th,
                  LogFecundityMean, LogFecunditySD, LogFecundity5th, LogFecundity95th,
                  BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                  LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th),
                ~replace(., is.na(.), 0)))

#save
saveRDS(alldata2, "output/LUTJANIDAE_alldata_clean_cinner2020.rda")





##################################################################
##                           Scarini                           ##
##################################################################
site_fecundity <- readRDS("output/SCARIDAE_siteFecundBiomass_1000.rds")
alldata_df=read.csv("data/SERF_SiteData.csv", header = T)

# Apply the function to each dataframe in the site_fecundity list
alldata <- lapply(site_fecundity, merge_with_alldata)

#get full site list 
sites_all <- site_fecundity %>%
  lapply(function(df) {
    if ("UniqueSite" %in% names(df)) {
      data.frame(UniqueSite = df[["UniqueSite"]])
    } else {
      data.frame(UniqueSite = NA)  # Return a data frame with NA if the column is not present
    }
  }) %>%
  bind_rows() %>%
  distinct()

#check missing sites 
final_sample <- as.data.frame(site_fecundity[100])
filter(sites_all,  UniqueSite %!in% final_sample$UniqueSite)

# Combine all dataframes in the list into one
combined_df <- bind_rows(site_fecundity)

# Transform meanFecundity and meanBiomassKG using log(x + 1)
combined_df <- combined_df %>%
  mutate(
    logMeanFecundity = log(meanFecundity + 1),
    logMeanBiomassKG = log(meanBiomassKG + 1)
  )




# Summarize the combined dataframe
final_summary <- combined_df %>%
  group_by(UniqueSite) %>%
  summarise(
    FecundityMean = mean(meanFecundity, na.rm = TRUE),
    FecunditySD = sd(meanFecundity, na.rm = TRUE),
    Fecundity5th = quantile(meanFecundity, probs = 0.05, na.rm = TRUE),
    Fecundity95th = quantile(meanFecundity, probs = 0.95, na.rm = TRUE),
    LogFecundityMean = mean(logMeanFecundity, na.rm = TRUE),
    LogFecunditySD = sd(logMeanFecundity, na.rm = TRUE),
    LogFecundity5th = quantile(logMeanFecundity, probs = 0.05, na.rm = TRUE),
    LogFecundity95th = quantile(logMeanFecundity, probs = 0.95, na.rm = TRUE),
    BiomassKGMean = mean(meanBiomassKG, na.rm = TRUE),
    BiomassKGSD = sd(meanBiomassKG, na.rm = TRUE),
    BiomassKG5th = quantile(meanBiomassKG, probs = 0.05, na.rm = TRUE),
    BiomassKG95th = quantile(meanBiomassKG, probs = 0.95, na.rm = TRUE),
    LogBiomassKGMean = mean(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKGSD = sd(logMeanBiomassKG, na.rm = TRUE),
    LogBiomassKG5th = quantile(logMeanBiomassKG, probs = 0.05, na.rm = TRUE),
    LogBiomassKG95th = quantile(logMeanBiomassKG, probs = 0.95, na.rm = TRUE)
  )



alldata <- merge.data.frame(final_summary, alldata_df, by = "UniqueSite", all =FALSE)

missing <- filter(alldata, is.na(FecundityMean) )

#mpa size and age restrictions
alldata$mpacondition=ifelse(alldata$Protection=="UnfishedHigh" &(alldata$MPAage>4|alldata$MPAage==4) & (alldata$NTZarea>2 |alldata$NTZarea==0),1,0)
alldata$keep=ifelse(!alldata$Protection=="UnfishedHigh",1,ifelse(alldata$Protection=="UnfishedHigh" &alldata$mpacondition==1,1,0))

##Explanatory variables: relevel categorical variables and standardize continuous variables
#reef-scale covariates
alldata$DepthCategory=relevel(factor(alldata$DepthCategory),ref="4-10m")
alldata$CleanHabitat=relevel(factor(alldata$CleanHabitat),ref="Slope")
alldata$Protection=relevel(factor(alldata$Protection),ref="Fished")
alldata$CensusMethod=relevel(factor(alldata$CensusMethod),ref="Standard belt transect")
alldata$sTotal_sampling_area=standardize(log(alldata$Total_sampling_area))

#gravities with different exponents
alldata$sgrav_tot1=standardize(log(alldata$gravtot5001+min(alldata$gravtot5001[alldata$gravtot5001>0])))
alldata$sgrav_tot2=standardize(log(alldata$gravtot5002+min(alldata$gravtot5002[alldata$gravtot5002>0])))
alldata$sgrav_tot3=standardize(log(alldata$gravtot5003+min(alldata$gravtot5003[alldata$gravtot5003>0])))

#reef cluster-scale covariates
alldata$sOcean_prod=standardize(log(alldata$Ocean_prod))
alldata$sClimate_stress=standardize(alldata$Climate_stress)
alldata$sRegional_population_growth=standardize(alldata$Regional_population_growth)

#nation/state-scale covariates
alldata$sReef_fish_landings_per_km2=standardize(log(alldata$Reef_fish_landings_per_km2+1))
alldata$sLarger_pop_size=standardize(log(alldata$Larger_pop_size+1))
alldata$sHDI=standardize(alldata$HDI)


#remove na
alldata <- filter(alldata, !is.na(FecundityMean))


#remove sites
alldata <- filter(alldata, UniqueSite %!in% c("2889","2890","2891","2892","2893", "2894", "2895", "2896", "2899", "2900"))


#only 1413 sites, add remainder of sites as 0
full_dat <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
full_dat <- subset(full_dat, 
                   select = -c(FecundityMean, FecunditySD, Fecundity5th,
                               Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                               LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                               LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))
sub_dat <- subset(alldata, 
                  select = c(UniqueSite, FecundityMean, FecunditySD, Fecundity5th,
                             Fecundity95th, LogFecundityMean, LogFecunditySD, LogFecundity5th,
                             LogFecundity95th, BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                             LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th))

alldata2 <- left_join(full_dat, sub_dat)
alldata2 <- alldata2 %>%
  mutate(across(c(FecundityMean, FecunditySD, Fecundity5th, Fecundity95th,
                  LogFecundityMean, LogFecunditySD, LogFecundity5th, LogFecundity95th,
                  BiomassKGMean, BiomassKGSD, BiomassKG5th, BiomassKG95th,
                  LogBiomassKGMean, LogBiomassKGSD, LogBiomassKG5th, LogBiomassKG95th),
                ~replace(., is.na(.), 0)))



#save
saveRDS(alldata2, "output/SCARIDAE_alldata_clean_cinner2020.rda")




