#use one core
options(mc.cores = 1)

#set themes
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

#colors
col <- brewer.pal(9, "Blues")[5:9]



#######################################################################################################################################
##########################################  BIOMASS > 20 CM       #####################################################################
#######################################################################################################################################

biom <- readRDS("output/brms_20cmBiomass.rds")

dat<-biom$data

#data to pred
cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress',  'sLarger_pop_size', 
  'sReef_fish_landings_per_km2'), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))

#get posteriors

posts<-pred %>% 
  add_epred_draws(biom,
                  re_formula = NA,
                  category='response',
                  seed =1,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")


fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))

#get range
ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"
median_qi(ratios$fished_unfished)
median_qi(ratios$fished_rest)

#plot
ratio_b20 <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha = 0.5,  
               .width = c(0.50, 0.95),density = "bounded")+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha = 0.5,  
               .width = c(0.50, 0.95),density = "bounded")+
  theme+
  xlim(0, 5)+
  ylab("")+
  ggtitle("Biomass > 20 cm")+
 theme(axis.title = element_blank(), axis.text.x = element_blank())
ratio_b20 

#######################################################################################################################################
##########################################  MATURE FEMALE BIOMASS        ##############################################################
#######################################################################################################################################

#repeat process as above for other categories
biom <- readRDS("output/brms_matf_se.rds")
dat<-biom$data

cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress', 'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogBiomassKGSD"), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_epred_draws(biom,
                  re_formula = NA,
                  category='response',
                  seed =2,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")

fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))

median_qi(fished)
median_qi(unfished)
median_qi(ratios$fished_unfished)

ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"


ratio_matF <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha = 0.5,  .width = c(0.66, 0.95),)+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha = 0.5,  .width = c(0.66, 0.95),)+
  theme+
  xlab("Ratio")+
  ylab("")+
  theme(axis.text.y = element_blank())+
 xlim(0, 5)+
  ggtitle("Mature female biomass")+
  theme(axis.title = element_blank(), axis.text.x = element_blank())
ratio_matF

#######################################################################################################################################
##########################################     FECUNDITY         ######################################################################
#######################################################################################################################################
#repeat again as above for fecundity
fec <- readRDS("output/brms_fecundity_se.rds")
dat <- fec$data

cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress', 'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogFecunditySD"), ~ mean(.x, na.rm = TRUE))
cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])
vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_epred_draws(fec,
                  re_formula = NA,
                  category='response',
                  seed =22,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")

fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))


ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"

median_qi(ratios$fished_unfished)
median_qi(ratios$fished_rest)

ratio_fec <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha = 0.5,  .width = c(0.66, 0.95),)+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha = 0.5,  .width = c(0.66, 0.95),)+
  theme+
  xlab("Ratio")+
  ylab("")+
  theme(axis.text.y = element_blank())+
  xlim(0, 5)+
  ggtitle("Fecundity (all families)")
ratio_fec   

#######################################################################################################################################
##########################################     Lutjanidae        ######################################################################
#######################################################################################################################################
fec <- readRDS("output/LUTJANIDAE_brms_fecundity.rds")
dat<- fec$data


cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress', 'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogFecunditySD"), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])

vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)

pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_epred_draws(fec,
                  re_formula = NA,
                  category='response',
                  seed =23,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")

fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))


ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"

median_qi(ratios$fished_unfished)

ratio_lut <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha =0.5,  .width = c(0.66, 0.95),)+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha =0.5,  .width = c(0.66, 0.95),)+
  theme+
  xlab("Ratio")+
  ylab("")+
  theme(axis.text.y = element_blank())+
  xlim(0,15)+
  ggtitle("Fecundity Lutjanidae")+
  theme(axis.title = element_blank(), axis.text.x = element_blank())


ratio_lut
#######################################################################################################################################
##########################################     Scaridae        ######################################################################
#######################################################################################################################################
fec <- readRDS("output/SCARIDAE_brms_fecundity.rds")
dat<- fec$data


cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress', 'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogFecunditySD"), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])

vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)
pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_epred_draws(fec,
                  re_formula = NA,
                  category='response',
                  seed = 25,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")

fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))


ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"


median_qi(ratios$fished_unfished)

ratio_scar <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha =0.5,  .width = c(0.66, 0.95),)+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha =0.5,  .width = c(0.66, 0.95),)+
  theme+
  xlab("Ratio")+
  ylab("")+
  theme(axis.text.y = element_blank())+
  xlim(1,15)+
  ggtitle("Fecundity Labridae (Scarini)")+
  theme(axis.title = element_blank(), axis.text.x = element_blank())

ratio_scar
#######################################################################################################################################
##########################################     Serranidae        ######################################################################
#######################################################################################################################################
fec <- readRDS("output/SERRANIDAE_brms_fecundity.rds")
dat<- fec$data


cont_mean<-dat %>% summarise_at(vars(
  'sTotal_sampling_area', 'sgrav_tot2', 'sRegional_population_growth', 
  'sOcean_prod', 'sClimate_stress', 'sLarger_pop_size', 
  'sReef_fish_landings_per_km2', "LogFecunditySD"), ~ mean(.x, na.rm = TRUE))

cat_ref<-data.frame(DepthCategory = levels(dat$DepthCategory)[1],
                    CleanHabitat = levels(dat$CleanHabitat)[1],
                    CensusMethod = levels(dat$CensusMethod)[1])


#ratio
vary <- subset(dat, select = c(Protection) )
vary <- unique(vary)

pred<-data.frame(vary %>% cbind(cont_mean, cat_ref))


posts<-pred %>% 
  add_epred_draws(fec,
                  re_formula = NA,
                  category='response',
                  seed =23,
                  ndraws = 4000)

fished <- posts %>% filter(Protection == "Fished")
restricted <- posts %>% filter(Protection == "Restricted")
unfished <- posts %>% filter(Protection == "UnfishedHigh")

fished_rest <- (exp(restricted$.epred-1))/(exp(fished$.epred-1))
fished_rest <- as.data.frame(fished_rest)
names(fished_rest) <- c("fished_rest")

fished_unfished <- (exp(unfished$.epred-1))/(exp(fished$.epred-1))
fished_unfished <- as.data.frame(fished_unfished)
names(fished_unfished) <- c("fished_unfished")

ratios <- as.data.frame(cbind(fished_rest, fished_unfished))

median_qi(ratios$fished_unfished)

ratios$f_u<- "Fished vs unfished"
ratios$f_r <- "Fished vs restricted"


median_qi(ratios$fished_rest)
median_qi(ratios$fished_unfished)

ratio_ser <- ggplot()+ 
  stat_halfeye(data = ratios, aes(x = fished_unfished, y = f_u), fill = col[5], alpha =0.5,  .width = c(0.66, 0.95),
               trim = FALSE)+ 
  stat_halfeye(data = ratios, aes(x = fished_rest, y = f_r), fill = col[1], alpha =0.5,  .width = c(0.66, 0.95),
               trim = FALSE)+
  theme+
  xlab("Ratio")+
  ylab("")+
  theme(axis.text.y = element_blank())+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15), limits = c(1,15), 
                     labels = label_number(accuracy = 1))+
  ggtitle("Fecundity Serranidae")

ratio_ser

#plot all together
patch <- (ratio_b20/ratio_matF/ratio_fec)|(ratio_lut/ratio_scar/ratio_ser)
all <- patch + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
all

#######################################################################################################################################
##########################################         MAKE FIGURE           ##############################################################
#######################################################################################################################################


pdf(file = "output/ratio_management.pdf",   
    width = 15, # The width of the plot in inches
    height = 10) # The height of the plot in inches

all

dev.off()


