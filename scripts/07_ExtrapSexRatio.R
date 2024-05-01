# load data
species_list <- readRDS("data/processed/SERF_species_list.rds")
fit_sex <- readRDS("output/sex_phy_model_noprotog.rds")
tree <- ape::read.tree("data/grafted_tree.tre")
data <- read.csv("data/sex_ratio_lit_protog.csv")
proto <- read.csv("data/protogynous_families.csv")


#clean spp list
species_list <- subset(species_list, select = c(genus_sp_correct))
names(species_list) <- "genus_sp"


#Extrap script adapted from https://github.com/valerianoparravicini/Trophic_Fish_2020
get_variables(fit_sex)
r_phylo <- tidybayes::spread_draws(fit_sex, r_Species[species,Intercept])
b <- tidybayes::spread_draws(fit_sex, b_Intercept, r_Species[species,Intercept]) 
r_sp_tree <- tidybayes::spread_draws(fit_sex, r_species_tree[species,Intercept])


### test extrapolation method
#where r_Species is intercept and temp slope? 
st <- r_phylo$r_Species
st <- as.data.frame(st)
st$genus_sp <- r_phylo$species
st <- unique(st)
st1 <- st %>% group_by(genus_sp) %>%
  summarise(median = median(st))
st1$genus_sp <- gsub("\\.", "_", st1$genus_sp)
st1 <- st1 %>% column_to_rownames(var="genus_sp")




#and sp slope
sp_b <- b$b_Intercept
sp_b <- as.data.frame(sp_b)
sp_b$genus_sp <- b$species
sp_b <- unique(sp_b)
sp_b1 <- sp_b %>% group_by(genus_sp) %>%
  summarise(median = median(sp_b))
sp_b1 <- sp_b1 %>% column_to_rownames(var="genus_sp")
sp_b1


#do the same for tree intercept
t <- r_sp_tree$r_species_tree
t <- as.data.frame(t)
t$genus_sp <- r_sp_tree$species
t <- unique(t)
t1 <- t %>% group_by(genus_sp) %>%
  summarise(median = median(t))
t1 <- t1 %>% column_to_rownames(var="genus_sp")



#drop excess tips
tree2 <- tree
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "tip.label"
drop <- tip %>% filter(tip.label %!in% species_list$genus_sp)
tree2 <- ape::drop.tip(tree2, drop$tip.label)
tip <- as.data.frame(tree2$tip.label)
names(tip) <- "genus_species"
tip <- unique(tip)#1106

missing <- filter(species_list, genus_sp %!in% tree2$tip.label)
missing <- filter(species_list, genus_sp %!in% tip$genus_species)
missing <- species_list[species_list$genus_sp %!in% tip$genus_species,]
missing <- unique(missing)

#estimate for species not in the cov matrix
est_tree <- phyEstimate(tree2, t1, method="pic") 

#est is species level intercept and slope
est <- phyEstimate(tree2, st1, method="pic") 


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


#fix col names
names(rtree) <- c("species", "intercept_tree")
names(rphy) <- c("species", "intercept")


# filter for species in serf spp list
new_tree <- filter(rtree, species %in% species_list$genus_sp)
new <- filter(rphy, species %in% species_list$genus_sp)
str(new_tree)
str(new)


#collected data clean up
str(data)
data <- subset(data, select = c("Species", "RatioProportions"))
data$Species <- gsub(" ", "_", data$Species)
names(data) <- c("species", "ratio")



#merge all new spp intercepts, slopes
new_all <- inner_join(new_tree, new, by = "species")
#merge with known data
new_all <- inner_join(new_all, data, by = "species")

#predict mu for new sp given temp
extrap <- merge.data.frame(species_list, new_all, by.x = c("genus_sp"), by.y = c("species"))
extrap$mu <- inverse_logit(median(sp_b1$median) + extrap$intercept_tree + extrap$intercept)
plot(extrap$mu, extrap$ratio)


###now for all (inc unknown) species 
new_all <- inner_join(new_tree, new, by = "species")
extrap <- merge.data.frame(species_list, new_all, by.x = c("genus_sp"), by.y = c("species"))


#sp_b1$median = 0.5631346 (old 0.5675483)
extrap$mu <- median(sp_b1$median) + extrap$intercept_tree + extrap$intercept
extrap$ratio_pred <- inverse_logit(extrap$mu)


#select + replace all gonochoristic spp with 0.5 ratio

#get species and families from dataset
sp_fam <- readRDS("Data/processed/SERF_species_list.rds")
names(sp_fam) <- c("Family", "genus_sp")
sp_fam <- na.omit(sp_fam)

#merge sp and extrap
all <- merge.data.frame(extrap, sp_fam, by = "genus_sp", all = TRUE)


#replace all gonochore families with 0.5 sex ratio
names(proto) <- c("Family", "Sexual_pattern")
all$ratio_pred2 <- ifelse(all$Family %in% proto$Family, all$ratio_pred, 0.5)

#add 0.5 to 4 missing spp not in sp tree
missing <- filter(all, is.na(ratio_pred2))
all$ratio_pred2 <- ifelse(all$genus_sp %in% missing$genus_sp, 0.5, all$ratio_pred2)
filter(all, is.na(ratio_pred2))

#check missing
missing <- filter(sp_fam, genus_sp %!in% all$genus_sp)

#save
saveRDS(all, "output/extrap_sex_ratio_model.rds")
