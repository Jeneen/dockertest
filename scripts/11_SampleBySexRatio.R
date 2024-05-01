#load data
#fecundity distribution for each fish (n=1000)
fec_extrap <- readRDS("output/fecundity_1000_df_mu.rds") 
serf_matF <- readRDS("output/SERF_biomass_matureF.rds") 
fec_extrap <- lapply(fec_extrap, function(df) {
    df %>% select(mu, biomass_kg_indiv, species)
  })
serf_matF <- subset(serf_matF, select = c(Family, genus_sp,UniqueSite, UniqueTransect, 
                                          TransectNumber, SampArea, Number,
                                          sex_ratio,prob_mat,biomass_kg_indiv))
names(serf_matF) <- c("Family", "species", "UniqueSite", "UniqueTransect", 
                      "TransectNumber", "SampArea", "Number",
                      "sex_ratio", "prob_mat", "biomass_kg_indiv")
serf_matF <- unique(serf_matF)


#merge
# Assuming fec_extrap is a list of dataframes and serf_matF is a dataframe
merge_fec_with_serf_unique <- function(fec_list, serf_df) {
  merged_list <- list()
  
  for (i in seq_along(fec_list)) {
    merged_df <- fec_list[[i]] %>% 
      full_join(serf_df) %>%
      distinct()
    
    merged_list[[i]] <- merged_df
  }
  
  return(merged_list)
}

# Merge each dataframe in fec_extrap with serf_matF and keep only unique rows
all <- merge_fec_with_serf_unique(fec_extrap, serf_matF) 

#saveRDS(all, "output/merged_fec_sex_ratio.rds")


#final
# Function to sample each dataframe
process_and_sample <- function(df, index, total) {
  message(sprintf("Processing dataframe %d of %d", index, total))
  
  # Drop NA values in the sex_ratio column (records without spp names or families dropped from analysis)
  df <- df %>% filter(!is.na(sex_ratio))
  
  # Uncount based on 'Number' and set necessary columns
  df <- uncount(df, weights = Number) 
  df$species <- as.factor(df$species)
  df$UniqueTransect <- as.factor(df$UniqueTransect)
  df$sex_ratio <- as.numeric(df$sex_ratio)
  
  # Group by and split data
  grouped_data <- df %>% 
    group_by(UniqueTransect, species) %>%
    group_split()
  
  sample_group <- function(df) {
    if (nrow(df) > 1) {
      mean_ratio <- mean(df$sex_ratio)
      set.seed(1)
      df %>% slice_sample(prop = mean_ratio)
    } else {
      return(df)
    }
  }
  
  # Sample each group using the sample_group function
  sampled_groups <- map(grouped_data, sample_group)
  
  # Combine sampled groups
  bind_rows(sampled_groups)
}

# Total number of dataframes to process
total_dfs <- length(all)

# Apply the function to each dataframe in the list 'all' with progress update 
# takes a while to run
sampledFemales <- map2(all, seq_along(all), ~process_and_sample(.x, .y, total_dfs))
sites <- as.data.frame(sampledFemales[1])
sites <- filter(sites, prob_mat > 0)
sites <- unique(sites$UniqueSite)


# Save the output
saveRDS(sampledFemales, "output/sampledFemales_1000.rds")

