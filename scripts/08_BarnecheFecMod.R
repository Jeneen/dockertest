##########          Run on Lancaster University HPC        ########## 


# Code from Barneche et al., 2018, Science

##################
# HELPER FUNCTIONS
##################
read_file <- function(file_name, ...) {
  read.csv(file_name, header = TRUE, stringsAsFactors = FALSE, ...)
}

####################
# LENGTH-WEIGHT DATA
####################
fishbase_lw <- function(verbose = TRUE) {
  if (verbose) {
    message("Downloading FB l-w data")
  }
  taxa <- rfishbase::load_taxa() %>% data.frame
  rfishbase::length_weight() %>%
    data.frame %>%
    dplyr::mutate(Family = taxa$Family[match(Species, taxa$Species)]) %>%
    dplyr::select(species = Species, family = Family,
                  score = CoeffDetermination, a, b,
                  length_cm_min = LengthMin, length_cm_max = LengthMax) %>%
    dplyr::mutate(a = ifelse(a == 0, 0.001, a)) %>%
    tidyr::drop_na() %>%
    dplyr::filter(score > 0.9, (b > 2.5 & b < 3.5))
}

################
# FECUNDITY DATA
################
try_find_lw <- function(sp_name) {
  out <- try(fishflux::find_lw(sp_name), silent = TRUE)
  if (!inherits(out, "try-error")) {
    out
  }
}



read_fecundity <- function(length_weight, lw_missing, verbose = TRUE) {
  ########################################
  # download fecundity data from GitHub
  # keep a & b values from lw data
  # with the highest score for each
  # species then merge with fecundity data
  ########################################
  data <- read_file("Diego_update/data/fecundityFemaleSize.csv")
  sub_lw <- length_weight %>%
    plyr::ddply(.(species), function(x)x[which.max(x$score), ])
  out <- merge(x = data, y = sub_lw, by.x = "Species",
               by.y = "species", all.x = TRUE)
  if (verbose) {
    spp_miss_list <- unique(out$Species[is.na(out$a)])
    message("The following species did not have original ",
            "length-weight conversion data on FishBase: ",
            paste0("\n", spp_miss_list, ";"),
            "\n\nGrabbing Bayesian closest LW...\n\n")
    bayesian_lw <- plyr::ldply(spp_miss_list, try_find_lw)
    still_missing <- spp_miss_list[!(spp_miss_list %in% bayesian_lw$species)]
    if (verbose && length(still_missing) > 0) {
      message("The following species did not have Bayesian LW:",
              paste0("\n", still_missing, ";"),
              "\n\nAdding LW manually...\n\n")
      lw_missing <- rbind(lw_missing %>%
                            dplyr::select(-obs),
                          bayesian_lw) %>%
        dplyr::arrange(species) %>%
        dplyr::filter(!duplicated(species)) %>%
        dplyr::select(species, a = lwa_m, b = lwb_m)
    } else {
      lw_missing <- bayesian_lw %>%
        dplyr::select(species, a = lwa_m, b = lwb_m)
    }
  }
  out[is.na(out$a), c("a", "b")] <- lw_missing[match(out$Species[is.na(out$a)],
                                                     lw_missing$species),
                                               c("a", "b")]
  # size needs to be in cm
  out$mass_g <- out$a * (out$FemaleSize_mm / 10) ^ out$b
  out$mass_g[is.na(out$mass_g)] <- out$FemaleMass_g[is.na(out$mass_g)]
  out$ln_mass_g <- log(out$mass_g)
  out %>%
    dplyr::rename(species = Species,
                  spawning_mode = spawningMode,
                  female_mass_g = FemaleMass_g,
                  female_size_mm = FemaleSize_mm,
                  size_type = sizeType,
                  fecundity_eggs_per_female = Fecundity_nOfEggs_per_female,
                  type_of_work_fecundity = typeOfWork_Fecundity,
                  time_to_fecundity_months = timeToFecundityMeasurement_months,
                  location = Location,
                  date = Date,
                  sample_size = sampleSize,
                  fertilized = Fertilized,
                  live = Live,
                  latitude = Latitude,
                  longitude = Longitude,
                  reference_pdf = ReferencePdf,
                  ln_female_size = lnFemaleSize,
                  ln_fecundity = lnFecundity,
                  abs_latitude = absLatitude)
}

################################
# PHYLOGENETIC MULTILEVEL MODELS
################################
clean_data_tree <- function(data, tree, ct, verbose = TRUE) {
  data$species_tree <- gsub(" ", "_", data$species)
  spp <- unique(data$species_tree)
  n <- 0
  for (i in seq_along(spp)) {
    x <- grep(spp[i], tree$tip.label, fixed = TRUE)
    if (length(x) == 0) {
      is_actino <- ct$Actinopterygii[ct$sp == spp[i]]
      if (length(is_actino) == 0) {
        stop(spp[i], " cannot be found")
      } else if (!is_actino) {
        if (verbose) {
          n_drop <- sum(data$species_tree == spp[i])
          message("Dropping ", n_drop, " observations for ", spp[i],
                  " from database; reason: not Actinopterygii.")
          n <- n + n_drop
        }
        data <- data[data$species_tree != spp[i], ]
      } else if (is_actino) {
        is_in_tree <- ct$Tree[ct$sp == spp[i]]
        if (!is_in_tree) {
          stop(spp[i], " is Actinopterygii but is not found in grafted tree?",
               " please revise.")
        }
        new_name <- ct$Taxonomy[ct$sp == spp[i]]
        if (verbose) {
          message("Modifying species name from ", spp[i], " to ", new_name,
                  " to conform with tree.")
        }
        data$species[data$species_tree == spp[i]] <- gsub("_", " ", new_name)
        data$species_tree[data$species_tree == spp[i]] <- new_name
      }
    }
  }
  if (verbose) {
    message("\n", n, " observations dropped in total.\n")
  }
  list(data = data, tree = tree)
}

run_fecundity_model <- function(...) {
  data_and_tree <- clean_data_tree(...)
  data <- data_and_tree$data
  tree <- data_and_tree$tree
  tree_vcv <- ape::vcv(tree, corr = TRUE)
  priors <- prior(normal(0, 1), class = "b") +
    prior(normal(0, 1), class = "Intercept") +
    prior(gamma(2, 0.1), class = "sd") +
    prior(gamma(2, 0.1), class = "sigma")
  set.seed(1)
  brms::brm(ln_fecundity ~ ln_mass_g + (1 | gr(species_tree, cov = tree_vcv)) +
              (1 + ln_mass_g | species), data = data, family = gaussian(),
            data2 = list(tree_vcv = tree_vcv), prior = priors,
            sample_prior = TRUE, chains = 4, cores = 4, iter = 15e3,
            warmup = 7500, control = list(adapt_delta = 0.999,
                                          max_treedepth = 20))
}


load("data/length_weight.rds")
lw_missing = read_file("data/lw_missing.csv")
fecundity = readRDS("data/fecundity.rds")
correct_taxonomy = read_file("data/correct_taxonomy.csv")
grafted_tree = ape::read.tree("data/grafted_tree.tre")
fecundity_model = run_fecundity_model(fecundity, grafted_tree,
                                      correct_taxonomy)
saveRDS(fecundity_model, file="output/fecundity_model.rds")




