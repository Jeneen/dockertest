#set theme
theme <-  theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size =14),
                axis.title = element_text(size =14),
                axis.line = element_line(size = 0.5, linetype = "solid",
                                         colour = "black"),
                legend.text=element_text(size=14), 
                #legend.title=element_text(face="bold"),
                plot.title = element_text(size=14))

#functions
se <- function(x, ...) sqrt(var(x, ...)/length(x))

#colors
cols <- c("Fished" = "red", "Restricted" = "orange", "UnfishedHigh" = "darkgreen")


#data
fec <- readRDS("output/brms_fecundity_se.rds")
dat <-fec$data

#posterior draws
cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area'), ~ mean(.x, na.rm = TRUE))
vary <- subset(dat, select = c(Protection,Larger, ReefCluster, sgrav_tot2, sRegional_population_growth, 
                                   sOcean_prod, sClimate_stress,  sLarger_pop_size, 
                                   sReef_fish_landings_per_km2, LogFecunditySD) ) #sHDI
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))
pred <- pred
posts<-pred %>% 
  add_predicted_draws(fec,
                      re_formula = NULL, #include all group level effects
                      category='response',
                      seed =10,
                      ndraws = 4000)



#get top 5
fec_country_prot <- posts %>% group_by(Larger, Protection,)%>%
  summarise(LogFecundityMedian = median(.prediction))
fec_country_prot_max <- posts %>% group_by(Larger, Protection)%>%
  summarise(LogFecundityMedian = median(.prediction)) %>%
  ungroup()%>%
  slice_max(order_by=LogFecundityMedian, n=5)
fec_country_prot_min <- posts %>% group_by(Larger, Protection)%>%
  summarise(LogFecundityMedian = median(.prediction)) %>%
  ungroup()%>%
  slice_min(order_by=LogFecundityMedian, n=5)

fec_country_prot_max <- posts %>% group_by(Larger, Protection)%>%
  median_qi(.prediction) %>%
  ungroup()%>%
  slice_max(order_by=.prediction, n=5)
fec_country_prot_min <- posts %>% group_by(Larger, Protection)%>%
  median_qi(.prediction) %>%
  ungroup()%>%
  slice_min(order_by=.prediction, n=5)
fec_country_all <- rbind(fec_country_prot_max, fec_country_prot_min)


#plot
dist <- ggplot(data = posts, aes(x = .prediction))+
  geom_density(size =2)+
  ylab("Density (posterior draws)")+
  xlab("")+
  theme(legend.background = element_rect(fill = "white", color = "white"),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x=element_blank())+
  theme(legend.position= "none")+theme+xlim(10, 25)
dist

fec_country_all$LargerProtection <- paste0(fec_country_all$Larger, fec_country_all$Protection)

top_5 <- ggplot(fec_country_all, aes(x =  reorder(LargerProtection, -.prediction), y = .prediction)) +
  geom_point(aes(color=Protection), alpha = 0.5, size =5)+
  geom_errorbar(aes(ymin=.lower, ymax= .upper, color = Protection), width=0,size =1,
                position=position_dodge(.9))+
  scale_color_manual(values = cols, labels = c("Fished", "Restricted", "Unfished"))+
  geom_text(aes(label=Larger),  position = position_dodge(width = 1), vjust = -0.9)+
  theme+
  theme(legend.background = element_rect(fill = "white", color = "white"),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_blank(),
        legend.position = "none")+
  ylab("Log Fecundity + 1 (eggs/ha)")+
  xlab("")+
  theme+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())+
  coord_flip()+ylim(10, 25)

top_5 

fec_distribution_fig <- dist/top_5
fec_distribution_fig

# save legend seperatelly
get_leg <- ggplot(fec_country_all, aes(x =  LargerProtection, y = .prediction)) +
  geom_point(aes(color=Protection), size = 5)+
  scale_color_manual(values = cols, labels = c("Fished", "Restricted", "Fully protected"))+
  theme(legend.position= "top",legend.title=element_blank())+theme

leg = cowplot::get_plot_component(get_leg, 'guide-box-top', return_all = TRUE)
leg =cowplot::ggdraw(leg)
leg


saveRDS(leg, file="output/prot_leg.rds")
saveRDS(fec_distribution_fig, file="output/fec_distribution_fig.rds")
