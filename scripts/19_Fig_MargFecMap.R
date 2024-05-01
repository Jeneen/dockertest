#set colors
protcols <- c("Fished" = "red", "Restricted" = "darkorange", "UnfishedHigh" = "darkgreen")
mycolours = rev(paletteer_c("scico::broc", 10))
mycolours = mycolours[c(3:6, 10)]

#set theme
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


#load data
dat <-readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
fec <- readRDS("output/brms_fecundity_se.rds")


#prepare df to pred
cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area'), ~ mean(.x, na.rm = TRUE))
vary <- subset(dat, select = c(LogFecunditySD, Protection,Larger, ReefCluster, sgrav_tot2, sRegional_population_growth, 
                               sOcean_prod, sClimate_stress,sLarger_pop_size, 
                               sReef_fish_landings_per_km2, Site_Lat, Site_Long) )
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


#get posterior draws
posts<-pred %>% 
  add_predicted_draws(fec,
                      re_formula = NULL, #include all group level effects
                      category='response',
                      ndraws = 4000,
                      seed =1203)

#get medians of geographic basin
meds <- posts %>% group_by(ReefCluster, Larger)  %>% 
              dplyr::summarize(median_pred=median(.prediction)) 
meds2 <- unique(data.frame(Larger = dat$Larger, ReefCluster = dat$ReefCluster,
                    Geographic_Basin = dat$Geographic_Basin))
meds <- inner_join(meds, meds2)
meds_summary <- meds %>% group_by(Geographic_Basin) %>% median_qi(median_pred)
saveRDS(meds_summary, "output/median_marginal_fecundity_geographic_basin.rds")


#Make a table for grouping by protection
meds <- posts %>% group_by(ReefCluster, Larger, Protection)  %>% 
  dplyr::summarize(median_pred=median(.prediction)) 
counts <- dat %>% group_by(Geographic_Basin, Protection) %>% dplyr::summarize(n = n())
meds2 <- unique(data.frame(Larger = dat$Larger, ReefCluster = dat$ReefCluster,
                           Geographic_Basin = dat$Geographic_Basin, Protection = dat$Protection))
meds <- inner_join(meds, meds2)
meds_summary <- meds %>% group_by(Geographic_Basin, Protection) %>% median_qi(median_pred)
meds_summary_count <- inner_join(counts, meds_summary)

tab_df(meds_summary_count, title = "Supplementary Table 1. Median marginal fecundity (log + 1) by marine ecoregion and protection",
       file="output/supp_median_fecundityByGB&Protection_summary.doc") 
#plot
mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))

#marg_fec <- subset(posts, select = c("Site_Lat", "Site_Long", ".prediction" ))
marg_fec <- posts %>% group_by(Site_Lat, Site_Long, Protection) %>%
                         dplyr::summarise(mean_fec = mean(.prediction))

#jitter points and fix lat/long for neg sites
marg_fec$Site_Lat2=marg_fec$Site_Lat+runif(length(marg_fec$Site_Lat), min=0, max=4)
marg_fec$Site_Long2=marg_fec$Site_Long+runif(length(marg_fec$Site_Long), min=0, max=3)
marg_fec$Site_Lat2=ifelse(marg_fec$Site_Lat2>23.5, marg_fec$Site_Lat,marg_fec$Site_Lat2)
marg_fec$lon2 <- ifelse(marg_fec$Site_Long2 < -25, marg_fec$Site_Long2 + 360, marg_fec$Site_Long2) 

quantile(marg_fec$mean_fec)

#plot
marg_fec_map <- ggplot() + 
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group), fill = "grey78") +
  coord_fixed(ylim = c(-30, 30), xlim = c(25,320), ratio = 1.3) +
  geom_jitter(data=marg_fec, aes(x=lon2, y=Site_Lat2, fill = mean_fec, size = mean_fec), pch=21) +
  scale_size(range = c(1,5), breaks = c(16.5, 17, 18, 18.5, 19), name = "Log Fecundity + 1 (eggs/ha)",
             guide = guide_legend(override.aes = list(fill = mycolours)),
             labels = c(expression("\u226416.5"), "17.0", "17.5", "18.0", 
                        expression("\u226519.0"))) +
  scale_fill_gradientn(colors = mycolours, name = "Log Fecundity + 1 (eggs/ha)", 
                       breaks = c(16.5, 17.0, 18.0, 18.5, 19)) +
  #guides(fill = guide_legend(override.aes = list(size = 3))) + # Adjust fill legend
  guides(fill = "none")+
  geom_hline(yintercept = 23.43695, lty = 2) +
  geom_hline(yintercept = -23.43695, lty = 2) +
  scale_x_continuous("", breaks = c(80, 180, 280), labels = c(-100, 0, 100)) +
  scale_y_continuous("", breaks = c(-20, -10, 0, 10, 20)) + 
  theme_classic() +
  theme(text = element_text(size = 15), legend.spacing.y = unit(1.0, 'cm'))

marg_fec_map


#save the rda
saveRDS(marg_fec_map, "output/map_margCateg_fec.rds")

#save pdf
marg_fec_map
pdf("output/map_margCateg_fec.pdf", width = 10, height =5)
marg_fec_map
dev.off()

