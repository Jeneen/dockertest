#load files 
fit <- readRDS("output/fecundity_model.rds")
tree <- ape::read.tree("data/grafted_tree.tre")
data <- readRDS("data/barneche_data_processed.rds")
matureF <- readRDS("output/SERF_biomass_matureF.rds") #df to extrapolate

#clean dfs
#fix names of raw data
data$genus_sp <- gsub(" ", "_", data$species)

#clean extrapolation df
#select spp and biomass
new_extrap <- subset(matureF, select = c(genus_sp, biomass_kg_indiv, NumberMatF))
#subset
names(new_extrap) <- c("species", "biomass_kg_indiv", "NumberMatF")
new_extrap <- unique(new_extrap)
#change to ln_g
new_extrap$biomass_g <- new_extrap$biomass_kg_indiv * 1000
new_extrap$biomass_ln_g_to_pred <- log(new_extrap$biomass_g)


#get sp list
species_list <- as.data.frame(new_extrap$species)
names(species_list) <- "genus_sp"

#check vars
get_variables(fit)

#drop excess tips
tree2 <- tree
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "tip.label"
drop <- as.vector(tip %>% filter(tip.label %!in% species_list$genus_sp))
tree2 <- drop.tip(tree2, drop$tip.label)
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "genus_species"
tip <- unique(tip)

#check any missing spp
missing <- filter(species_list, genus_sp %!in% tree2$tip.label)
missing <- filter(species_list, genus_sp %!in% tip$genus_species)
missing <- species_list[species_list$genus_sp %!in% tip$genus_species,]
missing <- unique(missing)

# fit = ln_fecundity ~ ln_mass_g + (1 | gr(species_tree, cov = tree_vcv)) + (1 + ln_mass_g | species)
# so "b_ln_mass_g" is the fixed effect (global) slope for ln_mass_g, 
# and terms like "r_species[species,ln_mass_g]" 
# represent random slopes (and intercepts) for each species. 
r_phylo <- tidybayes::spread_draws(fit, r_species[species,Intercept], ndraws =1000)
##fixed terms b should be the same for all spp (be incl spp in df to double check)
b <- tidybayes::spread_draws(fit, r_species[species,ln_mass_g], b_Intercept, b_ln_mass_g, 
                               ndraws =1000)
r_sp_tree <- tidybayes::spread_draws(fit, r_species_tree[species,Intercept], 
                                       ndraws =1000)


# species intercept
r_phylo_int <- filter(r_phylo, Intercept == "Intercept")
st <- r_phylo_int$r_species
st <- as.data.frame(st)
st$genus_sp <- r_phylo_int$species
st <- unique(st)

# tree intercept
r_sp_tree <- filter(r_sp_tree, Intercept == "Intercept")
t <- r_sp_tree$r_species_tree
t <- as.data.frame(t)
t$genus_sp <- r_sp_tree$species
t <- unique(t)

#and sp slope
r_phylo_slope <- filter(r_phylo, Intercept == "ln_mass_g")
sp_b <- r_phylo_slope$r_species
sp_b <- as.data.frame(sp_b)
sp_b$genus_sp <- r_phylo_slope$species
sp_b <- unique(sp_b)


run_phyEstimate <- function(df, tree2) {
  # Determine the number of rows and segments
  n_rows <- nrow(df)
  n_segments <- 1000
  
  # Initialize lists
  list_of_df <- list()
  est_list <- list()
  
  # Create list of dataframes
  for (i in 1:n_segments) {
    rows_to_extract <- seq(i, n_rows, by = n_segments)
    segment_df <- df[rows_to_extract, , drop = FALSE]
    
    # Fix species names and set as row names
    segment_df$genus_sp <- gsub("\\.", "_", segment_df$genus_sp)
    rownames(segment_df) <- segment_df$genus_sp
    segment_df$genus_sp <- NULL  # Remove the genus_sp column after setting it as rownames
    
    list_of_df[[i]] <- segment_df
  }
  
  # Run phyEstimate for each dataframe
  for (i in seq_along(list_of_df)) {
    df1 <- list_of_df[[i]]
    est <- phyEstimate(tree2, df1, method="pic")
    est_list[[length(est_list) + 1]] <- est
  }
  
  # Return the lists
  return(list(df_list = list_of_df, est_list = est_list))
}


# Function to bind two lists of dataframes by row names and 
# convert row names to a column
bind_lists_by_rowname <- function(list1, list2) {
  bound_list <- list()
  for (i in seq_along(list1)) {
    # Merge dataframes based on row names
    bound_df <- merge(list1[[i]], list2[[i]], by = "row.names", all = TRUE)
    
    # Rename the row names column to 'species'
    names(bound_df)[1] <- "species"
    
    # Add the merged dataframe to the list
    bound_list[[i]] <- bound_df
  }
  return(bound_list)
}

create_r_phylo_list <- function(bound_list) {
  r_phylo_list <- list()
  
  for (i in seq_along(bound_list)) {
    # Extract the current dataframe
    current_df <- bound_list[[i]]
    
    # Create the sp_intercept column
    current_df$sp_intercept <- ifelse(is.na(current_df$st), current_df$estimate, current_df$st)
    
    # Select only the 'species' and 'sp_intercept' columns
    current_df <- current_df[, c("species", "sp_intercept")]
    
    # Add the modified dataframe to the new list
    r_phylo_list[[i]] <- current_df
  }
  
  return(r_phylo_list)
}


create_r_tree_list <- function(bound_list) {
  r_tree_list <- list()
  
  for (i in seq_along(bound_list)) {
    # Extract the current dataframe
    current_df <- bound_list[[i]]
    
    # Create the r_tree column
    current_df$tree_intercept <- ifelse(is.na(current_df$t), current_df$estimate, current_df$t)
    
    # Select only the 'species' and 'tree_intercept' columns
    current_df <- current_df[, c("species", "tree_intercept")]
    
    # Add the modified dataframe to the new list
    r_tree_list[[i]] <- current_df
  }
  
  return(r_tree_list)
}



create_r_sl_list <- function(bound_list) {
  r_sl_list <- list()
  
  for (i in seq_along(bound_list)) {
    # Extract the current dataframe
    current_df <- bound_list[[i]]
    
    # Create the r_phylo_sl column
    current_df$sp_slope <- ifelse(is.na(current_df$sp_b), current_df$estimate, current_df$sp_b)
    
    # Select only the 'species' and 'sp_slope' columns
    current_df <- current_df[, c("species", "sp_slope")]
    
    # Add the modified dataframe to the new list
    r_sl_list[[i]] <- current_df
  }
  
  return(r_sl_list)
}


# apply to rphy
est_list <- run_phyEstimate(st, tree2)
rphy <- bind_lists_by_rowname(est_list$df_list, est_list$est_list)
rphy <- create_r_phylo_list(rphy)


# apply to rtree
est_tree_list <- run_phyEstimate(t, tree2)
rtree <- bind_lists_by_rowname(est_tree_list$df_list, est_tree_list$est_list)
rtree <- create_r_tree_list(rtree)

# apply to rphy_sl
est_sl_list <- run_phyEstimate(sp_b, tree2)
rphy_sl <- bind_lists_by_rowname(est_sl_list$df_list, est_sl_list$est_list)
rphy_sl <- create_r_sl_list(rphy_sl)



#fixed terms b are the same for all spp
#fixed slope
b_ln_mass_g <-  filter(b, ln_mass_g == "ln_mass_g")
set.seed(10)
b_ln_mass_g <- sample(b_ln_mass_g$b_ln_mass_g, 1000, replace =FALSE)
b_ln_mass_g <- as.data.frame(b_ln_mass_g)
names(b_ln_mass_g) <- "fixed_sl"
b_ln_mass_g <- split(b_ln_mass_g, seq(nrow(b_ln_mass_g)))

#fixed intercept
b_Intercept <-  filter(b, ln_mass_g == "Intercept")
set.seed(10)
b_Intercept <- sample(b_Intercept$b_Intercept, 1000, replace =FALSE)
b_Intercept <- as.data.frame(b_Intercept)
names(b_Intercept) <- "fixed_int"
b_Intercept <- split(b_Intercept, seq(nrow(b_Intercept)))

merge_dfs_by_species <- function(list1, list2, list3, list4, list5, new_extrap) {
  merged_list <- list()
  
  # Ensure that all lists have the same length
  if (length(list1) != length(list2) || length(list2) != length(list3) || length(list3) != length(list4) || length(list4) != length(list5)) {
    stop("All lists must have the same length")
  }
  
  # Iterate over the lists and merge dataframes
  for (i in seq_along(list1)) {
    df1 <- list1[[i]]
    df2 <- list2[[i]]
    df3 <- list3[[i]]
    df4_value <- list4[[i]][1, 1] # Assuming each dataframe in list4 has only one row and one column
    df5_value <- list5[[i]][1, 1] # Assuming each dataframe in list5 has only one row and one column
    
    # Merge dataframes by 'species' column and add the single values from list4 and list5
    merged_df <- df1 %>%
      left_join(df2, by = "species") %>%
      left_join(df3, by = "species") %>%
      left_join(new_extrap, by = "species") %>%
      mutate(fixed_sl = df4_value) %>%
      mutate(fixed_int = df5_value) 
    
    merged_list[[i]] <- merged_df
  }
  
  return(merged_list)
}

all_lists <- merge_dfs_by_species(rphy_sl, rtree, rphy, b_ln_mass_g, b_Intercept, 
                                  new_extrap)

#calculate mu
calculate_mu <- function(all_lists) {
  for (i in seq_along(all_lists)) {
    # Access the dataframe
    df <- all_lists[[i]]
    
    # Drop rows where 'biomass_ln_g_to_pred' is NA
    df <- df[!is.na(df$biomass_ln_g_to_pred), ]
    
    # Perform the calculation for 'mu'
    df$mu <- (df$sp_slope * df$biomass_ln_g_to_pred) +
      (df$fixed_sl * df$biomass_ln_g_to_pred) +
      df$sp_intercept +
      df$fixed_int + 
      df$tree_intercept
    
    # Replace the original dataframe in the list with the updated one
    all_lists[[i]] <- df
  }
  return(all_lists)
}

# Apply the function to your list of dataframes
all_lists_with_mu <- calculate_mu(all_lists)

#save
saveRDS(all_lists_with_mu , "output/fecundity_1000_df_mu.rds")

