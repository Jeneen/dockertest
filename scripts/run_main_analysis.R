# Function to clear environment at the end of script but keep functions (large amount of memory needed)
clear_environment_keep_functions <- function() {
  lst <- ls()
  for (item in lst) {
    if (!is.function(get(item))) {
      rm(list=item, envir = globalenv())
    }
  }
}

# Load libraries
# List of all packages
packages <- c("readr", "plyr", "tidyverse", "rfishbase", "fishtree", "ape", "Hmisc", "brms",
              "dplyr", "fields", "ncdf4", "tidybayes", "picante", "parallel", "broom", "sp", "purrr", 
              "viridis", "RColorBrewer", "ggrepel", "stringr", "ggbeeswarm", "lme4", "paletteer",
              "scales", "patchwork", "grid", "cowplot", "devtools", "reshape2", "janitor", "sjPlot", 
              "ggplotify", "openxlsx")

# Loop to load each package
sapply(packages, library, character.only = TRUE)


# List of scripts to run in order
scripts <- c(
  "scripts/00_functions.R", # loads packages and functions
  "scripts/01_GetSSTSites.R", # gets sst of sites for length at maturity temperature model
  "scripts/02_CreateTempDFtoPredict.R", # creates a dataframe of temp-species to extrapolate
  "scripts/03_AddMissingSpp.R", # adds missing species
  "scripts/04_SppTempMod.R", # runs lm ~ temperature model
  "scripts/05_ExtrapSppTemp.R", # extrapolates lm ~ temperature model
  "scripts/06_SexRatioMod.R", # run sex ratio model
  "scripts/07_ExtrapSexRatio.R", # extrapolate sex ratio model
  "scripts/08_BarnecheFecMod.R", # run fecundity model
  "scripts/09_PrepMatFdf.R", # prepare mature female dataframe
  "scripts/10_ExtrapFecund.R", # extrapolate fecundity model
  "scripts/11_SampleBySexRatio.R", # sample fish by sex ratio
  "scripts/12_MatFSubsetFamilies.R", # create family specific subsets
  "scripts/13_GlobalDrivers_CleanDFs.R", # clean dataframes for global drivers models
  "scripts/14_CheckCovariates.R", # check covariates for global drivers models
  "scripts/15_FecBiomPowerMod.R", # run fecundity ~ female biomass power law model
  "scripts/16_GlobalDrivers_RunMods.R", # run global drivers models
  "scripts/17_Sim30x30_AllFam.R", # run 30 by 30 simulation for all families
  "scripts/18_Sim30x30_Serranidae.R", # run 30 by 30 simulation for serranidae
  "scripts/19_Fig_MargFecMap.R", # create map
  "scripts/20_Fig_LinearMod.R", # create linear model figure fecundity ~ female biomass
  "scripts/21_Fig_FecDistrib.R", # create fecundity distribution model
  "scripts/22_Fig_Grid.R", # create figure 1 grid
  "scripts/23_Fig_CoefPlot.R", # create coefficient plot figure
  "scripts/24_Fig_MargRatios.R", # create ratio figure
  "scripts/25_Fig_30x30.R" # create 30 by 30 simulation figure
  
)

# Execute each script and clear environment (keep functions)
for (script in scripts) {
  cat("Running script:", script, "\n")
  source(script)
  clear_environment_keep_functions()
}
