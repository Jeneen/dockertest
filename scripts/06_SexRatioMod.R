##########          Run on Lancaster University HPC        ########## 

#load data for all non gonochoristic spp
sex_ratio  <-  read.csv("data/sex_ratio_lit_protog.csv")
grafted_tree = ape::read.tree("data/grafted_tree.tre")

#clean tree
tree_vcv <- ape::vcv(grafted_tree, corr = TRUE)

#get tips
tip <- as.data.frame(grafted_tree$tip.label)


#standardise spp names
sex_ratio$species_tree <- gsub(" ", "_", sex_ratio$Species)


#match spp to tree
sex_ratio <- filter(sex_ratio, species_tree %in% grafted_tree$tip.label)


###################
#model

priors <- prior(normal(0, 0.9), class = "Intercept") + 
  prior(gamma(2, 0.1), class = "sd")+
  prior(gamma(0.01, 0.01), class = "phi") 

# check_priors <- brms::brm(RatioProportions ~ (1 | Species), 
#                            data = sex_ratio, family = Beta(link = "logit"),
#                            chains = 2, cores = 1, iter=1000, warmup=500,  
#                            prior = priors,
#                            sample_prior = TRUE,
#                            control = list(adapt_delta = 0.999, 
#                                           max_treedepth = 20))
# prior_predictions <- prior_draws(check_priors, "Intercept")


sex_phy_model <- brms::brm(RatioProportions ~ 
                             (1 | gr(species_tree, cov = tree_vcv)) +  (1 | Species), 
                           data = sex_ratio, family = Beta(link = "logit"),
                           data2 = list(tree_vcv = tree_vcv), prior = priors,
                           chains = 4, cores = 4, #change number of cores for super computer
                           iter=15000, warmup=7500,
                           seed =1,
                           control = list(adapt_delta = 0.999,
                                          max_treedepth = 20))

#save
saveRDS(sex_phy_model, file = "output/sex_phy_model_noprotog.rds")



















