cols <- c("Fished" = "red", "Restricted" = "orange", "UnfishedHigh" = "darkgreen")
theme <-  theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size =20),
                axis.title = element_text(size =20),
                axis.line = element_line(size = 0.5, linetype = "solid",
                                         colour = "black"),
                legend.text=element_text(size=20),
                plot.title = element_text(size=30))



#fec <- readRDS("Output/reproductive_potential_serf/brms_fecundity.rds")
#dat<-readRDS("Output/reproductive_potential_serf/alldata_clean_cinner2020.rda")
#dat$LogBiomassMedian <- log(dat$BiomassMedian +1)
fec <- readRDS("output/brms_fecundity_se.rds")
dat <- readRDS("Output/alldata_SDandCIof1000_clean_cinner2020.rda")

#cont_mean<-dat %>% summarise_at(vars(
# 'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
#'sOcean_prod', 'sClimate_stress', 'sHDI', 'sLarger_pop_size', 
#'sReef_fish_landings_per_km2'), ~ mean(.x, na.rm = TRUE))
cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area'), ~ mean(.x, na.rm = TRUE))


vary <- subset(dat, select = c(Protection,Larger, ReefCluster, sgrav_tot2, sRegional_population_growth, 
                               sOcean_prod, sClimate_stress,sLarger_pop_size, 
                               sReef_fish_landings_per_km2, Site_Lat, Site_Long,
                               LogFecunditySD) )

pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_predicted_draws(fec,
                      re_formula = NULL, #include all group level effects
                      category='response',
                      seed =1012,
                      ndraws = 4000)

#get medians of geographic basin
meds <- posts %>% group_by(ReefCluster, Larger, Protection)  %>% 
  dplyr::summarize(median_pred=median(.prediction)) 
# dat <- dat %>% group_by(ReefCluster, Larger, Protection) %>%
#   summarise(median_biom = median(LogBiomassMedian))
dat <- dat %>% group_by(ReefCluster, Larger, Protection) %>%
  summarise(median_biom = median(LogBiomassKGMean))
meds <- inner_join(meds, dat)
meds <- unique(meds)

meds$LargerProtection <- paste0(meds$Larger, meds$Protection)
max <- meds %>%
  group_by(Larger, Protection, LargerProtection, ReefCluster)%>%
  summarise(mean_fec = mean(median_pred),
            mean_biom = mean(median_biom))%>% 
  ungroup()%>%
  slice_max(order_by=mean_fec, n=40)

# mid <- meds %>%
#   group_by(Larger, Protection, LargerProtection, ReefCluster) %>%
#   summarise(mean_fec = mean(median_pred), mean_biom = mean(median_biom), .groups = 'drop') %>%
#   arrange(mean_fec) %>%
#   mutate(row_number = row_number(),
#          total_rows = n()) %>%
#   { 
#     df <- .
#     n_rows <- 10  
#     mid_idx <- floor(df$total_rows / 2)
#     idx <- seq(from = max(1, mid_idx - (n_rows-1)/2), to = min(nrow(df), mid_idx + (n_rows-1)/2), by = 1)
#     idx <- as.integer(idx)  # Ensure idx is integer
#     df <- df[idx, ]
#     select(df, -row_number, -total_rows)
#   }

min <- meds %>%
  group_by(Larger, Protection, LargerProtection, ReefCluster)%>%
  summarise(mean_fec = mean(median_pred),
            mean_biom = mean(median_biom))%>% 
  ungroup()%>%
  slice_min(order_by=mean_fec, n=10)
labels <- rbind(max, min)
#labels <- rbind(labels, mid)

reef_scale_biomass_fec <- ggplot(data = meds, aes(x = median_biom, y =exp(median_pred)))+
  geom_point(aes( color = Protection),alpha = 0.5, size =2)+
  scale_color_manual(values = cols, labels = c("Fished", "Restricted", "Fully protected"))+
  geom_text_repel(data = labels, aes(x = mean_biom, y = exp(mean_fec)), 
                  label = labels$Larger, size = 3, max.overlaps = Inf)+
  xlab("Log Mature Female Biomass (kg/ha) + 1")+
  ylab("Fecundity (eggs/ha)")+
  theme(legend.background = element_rect(fill = "white", color = "white"),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_blank())+
  theme(legend.position= "top")+
  #theme(legend.position= c(0.5,0.9), legend.direction = "horizontal")+
  #ylim(0,700000)+
  xlim(0,10)+
  theme
reef_scale_biomass_fec



pdf(file = "output/posterior_fec_vs_raw_biom.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

reef_scale_biomass_fec

# Step 3: Run dev.off() to create the file!
dev.off()

