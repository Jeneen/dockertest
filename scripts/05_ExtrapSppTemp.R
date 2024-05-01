#load data
fit_lm <- readRDS("output/lmat_model_out.rds")
tree <- ape::read.tree("data/grafted_tree.tre")
data <- readRDS(file = "data/processed/final_lmat_data.rds")
species_list <- readRDS("data/processed/SERF_species_list.rds")
new_extrap <- readRDS("data/processed/new_extrap.rds")
new_extrap_missing <- readRDS("data/processed/new_extrap_missing.rds")

#clean
species_list <-  species_list$genus_sp_correct
species_list <- as.data.frame(species_list)
names(species_list) <- "genus_sp"
dup <- filter(species_list, duplicated(genus_sp))

#check vars for extraction
get_variables(fit_lm)

#Extrap script adapted from https://github.com/valerianoparravicini/Trophic_Fish_2020
r_phylo <- tidybayes::spread_draws(fit_lm, r_Species[species,Intercept])
b <- tidybayes::spread_draws(fit_lm, b_temp_scaled,  b_Intercept, r_Species[species,temp_scaled])
r_sp_tree <- tidybayes::spread_draws(fit_lm, r_species_tree[species,Intercept])
sigma <- tidybayes::spread_draws(fit_lm, sigma)

### test extrapolation method
st <- r_phylo$r_Species
st <- as.data.frame(st)
st$genus_sp <- r_phylo$species
st <- unique(st)
st1 <- st %>% group_by(genus_sp) %>%
  summarise(median = median(st))
st1 <- st1 %>% column_to_rownames(var="genus_sp")

#do the same for tree intercept
t <- r_sp_tree$r_species_tree
t <- as.data.frame(t)
t$genus_sp <- r_sp_tree$species
t <- unique(t)
t1 <- t %>% group_by(genus_sp) %>%
  summarise(median = median(t))
t1 <- t1 %>% column_to_rownames(var="genus_sp")


#and sp slope
sp_b <- b$r_Species
sp_b <- as.data.frame(sp_b)
sp_b$genus_sp <- b$species
sp_b <- unique(sp_b)
sp_b1 <- sp_b %>% group_by(genus_sp) %>%
  summarise(median = median(sp_b))
sp_b1 <- sp_b1 %>% column_to_rownames(var="genus_sp")
sp_b1


#drop excess tips
tree2 <- tree
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "tip.label"
drop <- tip %>% filter(tip.label %!in% species_list$genus_sp)
tree2 <- drop.tip(tree2, drop)
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "genus_species"
tip <- unique(tip)
missing <- filter(species_list, genus_sp %!in% tree2$tip.label)
missing <- filter(species_list, genus_sp %!in% tip$genus_species)
missing <- species_list[species_list$genus_sp %!in% tip$genus_species,]
missing <- unique(missing)

#estimate
est_tree <- phyEstimate(tree2, t1, method="pic") 
est <- phyEstimate(tree2, st1, method="pic") 
est_b <- phyEstimate(tree2, sp_b1, method="pic") 


#extrapolated covariance matrix
rtree_m <- data.frame(
  species = rownames(est_tree),
  r_tree = est_tree[,1])
#known from model
rtree <- data.frame(
  species = row.names(t1),
  r_tree = t1$median
)
rtree <- rbind(rtree_m, rtree)
str(rtree)


#extrapolated species intercept 
rphy_m <- data.frame(
  species = rownames(est),
  r_phylo = est[,1])
#known from model
rphy <- data.frame(
  species = row.names(st1),
  r_phylo = st1$median
)

rphy <- rbind(rphy_m, rphy)
str(rphy)

#extrapolated slope
b_m <- data.frame(
  species = rownames(est_b),
  b_sp = est_b[,1])
#known from model
b_known <- data.frame(
  species = row.names(sp_b1),
  b_sp = sp_b1$median
)
b_species <- rbind(b_m, b_known)
str(b_species) 


#fix col names
names(rtree) <- c("species", "intercept_tree")
names(rphy) <- c("species", "intercept")
names(b_species) <- c("species", "slope")


# filter for species in serf spp list
new_tree <- filter(rtree, species %in% species_list$genus_sp)
new <- filter(rphy, species %in% species_list$genus_sp)
new_b <- filter(b_species, species %in% species_list$genus_sp)


#collected data clean up
str(data)
data <- data[,-2]
data <- data[,-3]
names(data) <- c("species", "temp", "logLm")


#merge all new spp intercepts, slopes
new_all <- inner_join(new_tree, new, by = "species")
new_all <- inner_join(new_all, new_b, by = "species")

#merge with known data
new_all <- inner_join(new_all, data, by = "species")

#predict mu for new sp given temp
new_extrap <- rbind(new_extrap, new_extrap_missing)
extrap <- merge.data.frame(new_extrap, new_all, by.x = c("genus_sp"), by.y = c("species"))
extrap$temp_sc <- scale(extrap$temp)
extrap$mu <- ((extrap$slope+median(b$b_temp_scaled)) * extrap$temp_sc) + extrap$intercept_tree + extrap$intercept + median(b$b_Intercept)
plot(extrap$mu, extrap$logLm)


###now for all (inc unknown) species and temps
new_all <- inner_join(new_tree, new, by = "species")
new_all <- inner_join(new_all, new_b, by = "species")
extrap <- merge.data.frame(new_extrap, new_all, by.x = c("genus_sp"), by.y = c("species"))
extrap$temp_sc <- scale(extrap$meanSST)
extrap$mu <- extrap$mu <- (extrap$slope * extrap$temp_sc) + extrap$intercept_tree + extrap$intercept + median(b$b_Intercept) + median(b$b_temp_scaled)
extrap$lm_model <- exp(extrap$mu)
extrap <- filter(extrap, !is.na(meanSST))

##check against fishlife lm
#library(FishLife)
#extrap$genus_sp[c(1,1000,4000,5000,15000)]
#extrap$lm_model[c(1, 1000,4000,5000,15000)]
#Plot_taxa(Search_species(Genus = "Acanthopagrus", Species = "bifasciatus")$match_taxonomy)
#Plot_taxa(Search_species(Genus = "Acanthurus", Species = "lineatus")$match_taxonomy)
#Plot_taxa(Search_species(Genus = "Balistapus", Species = "undulatus")$match_taxonomy)
#test <- Plot_taxa(Search_species(Genus = "Scomberomorus", Species = "commerson")$match_taxonomy)
#test <- Plot_taxa(Search_species(Genus = "Centropyge", Species = "flavissima")$match_taxonomy)
#test[[1]]$Mean_pred[7]
#test[[2]]$Mean_pred[7]


### seems to perform well
#predict <- predict(fit_lm, newdata)
#plot(predict)

#save 
saveRDS(extrap, file = "output/extrapolated_sp_temp.rds")
