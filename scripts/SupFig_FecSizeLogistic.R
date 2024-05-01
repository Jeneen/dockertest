#Load models
Fit_FecClassBiomass<- readRDS("output/logisticFecBiom_model.rds")
summary(Fit_FecClassBiomass)
Fit_FecClassAbundance <- readRDS("output/logisticFecAbund_model.rds")
summary(Fit_FecClassAbundance)
Fit_SizeClassBiomass <- readRDS("output/logisticSize_allfec_biomass_model.rds")
summary(Fit_SizeClassBiomass)

#extract conditional effects
#Fecundity class as proportion of abundance model
ce_ProtAbund <- conditional_effects(Fit_FecClassAbundance, "Protection", categorical = TRUE, prob = 0.95, ndraws =1000,
                                    re_formula = NULL)
ce_ProtAbund <- as.data.frame(ce_ProtAbund$`Protection:cats__`)
ce_BiomAbund <- conditional_effects(Fit_FecClassAbundance, "sLogMeanAllFishBiomassKgHa", categorical = TRUE,
                                    prob = 0.95, ndraws =1000, re_formula = NULL)
ce_BiomAbund <- as.data.frame(ce_BiomAbund$`sLogMeanAllFishBiomassKgHa:cats__`)


#Fecundity class as proportion of the biomass model
ce_ProtFecBiom <- conditional_effects(Fit_FecClassBiomass, "Protection", categorical = TRUE,prob = 0.95, ndraws =1000)
ce_ProtFecBiom <- as.data.frame(ce_ProtFecBiom$`Protection:cats__`)
ce_BiomFecBiom <- conditional_effects(Fit_FecClassBiomass, "sLogMeanAllFishBiomassKgHa", categorical = TRUE,
                                    prob = 0.95, ndraws =1000, re_formula = NULL)
ce_BiomFecBiom <- as.data.frame(ce_BiomFecBiom$`sLogMeanAllFishBiomassKgHa:cats__`)


#Size class (all fish) as a proportion of the biomass model
ce_ProtSizeBiom <- conditional_effects(Fit_SizeClassBiomass, "Protection", categorical = TRUE, ,prob = 0.95,
                                       ndraws =1000, re_formula = NULL)
ce_ProtSizeBiom <- as.data.frame(ce_ProtSizeBiom$`Protection:cats__`)
ce_BiomSizeBiom  <- conditional_effects(Fit_SizeClassBiomass, "sLogMeanAllFishBiomassKgHa", categorical = TRUE,
                                      prob = 0.95, ndraws =1000, re_formula = NULL)
ce_BiomSizeBiom <- as.data.frame(ce_BiomSizeBiom$`sLogMeanAllFishBiomassKgHa:cats__`)



##################################################  Plot  ################################################## 

#color palette
palette <- magma(n=10)
cols <- c("propAbLowFec" = palette[3], "propAbMedFec" = palette[5], "propAbHighFec" = palette[7])

#Fecundity class as proportion of abundance model
#protection
ProtAbund_fig <- ggplot(data = ce_ProtAbund, aes(x = Protection, y = estimate__, color = cats__))+
  geom_point(position=position_dodge(0.4), size = 5)+
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.3,
                position=position_dodge(0.4), linewidth = 1.5)+
  scale_color_manual(values = cols, labels = c("Low fecundity \n(<= 10.40 log eggs/fish)", 
                                               "Medium fecundity \n(10.40-11.84 log eggs/fish)",
                                               "High fecundity \n(>= 11.84 log eggs/fish)"))+
  theme +
  theme(legend.title = element_blank())+
  guides(color = guide_legend(byrow = TRUE))+ 
  scale_x_discrete(labels = c("Fished", "Restricted", "Unfished"))+
  ylab("Proportion of mature female abundance")+
  xlab("")+
  ggtitle("Fecundity classes as a proportion of mature female abundance")+
  ylim(0,1)
ProtAbund_fig 

#biomass
BiomAbund_fig <- ggplot(data = ce_BiomAbund, aes(x = sLogMeanAllFishBiomassKgHa, y = estimate__, fill = cats__))+
  geom_line()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.6)+
  scale_fill_manual(values = cols, labels = c("Low fecundity \n(<= 10.40 log eggs/fish)", 
                                              "Medium fecundity \n(10.40-11.84 log eggs/fish)",
                                              "High fecundity \n(>= 11.84 log eggs/fish)"))+
  theme +
  theme(legend.title = element_blank(), legend.position = 'none')+
  ylab("Proportion of mature female abundance")+
  xlab("Scaled biomass")+
 #ggtitle("Fecundity classes as a proportion of mature female abundance")+
ylim(0,1)
BiomAbund_fig






#Fecundity class as proportion of the biomass model
#protection
cols <- c("propBiomLowFec" = palette[3], "propBiomMedFec" = palette[5], "propBiomHighFec" = palette[7])
ProtFecBiom_fig <- ggplot(data = ce_ProtFecBiom, aes(x = Protection, y = estimate__, color = cats__))+
  geom_point(position=position_dodge(0.4), size = 5)+
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.3,
                position=position_dodge(0.4), linewidth = 1.5)+
  scale_color_manual(values = cols, labels = c("Low fecundity \n(<= 10.40 log eggs/fish)", 
                                               "Medium fecundity \n(10.40-11.84 log eggs/fish)",
                                               "High fecundity \n(>= 11.84 log eggs/fish)"))+
  theme +
  theme(legend.title = element_blank())+
  guides(color = guide_legend(byrow = TRUE))+ #add to theme to increase space legend.spacing.y = unit(1, 'cm')
  scale_x_discrete(labels = c("Fished", "Restricted", "Unfished"))+
  ylab("Proportion of mature female biomass")+
  xlab("")+
  ggtitle("Fecundity classes as a proportion of mature female biomass")+
  ylim(0,1)
ProtFecBiom_fig

#biomass
BiomFecBiom_fig <- ggplot(data = ce_BiomFecBiom, aes(x = sLogMeanAllFishBiomassKgHa, y = estimate__, fill = cats__))+
  geom_line()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.6)+
  scale_fill_manual(values = cols, labels = c("Low fecundity \n(<= 10.40 log eggs/fish)", 
                                              "Medium fecundity \n(10.40-11.84 log eggs/fish)",
                                              "High fecundity \n(>= 11.84 log eggs/fish)"))+
  theme +
  theme(legend.title = element_blank(), legend.position = 'none')+
  ylab("Proportion of mature female biomass")+
  xlab("Scaled biomass")+
  #ggtitle("Fecundity classes as a proportion of mature female biomass")+
ylim(0,1)
BiomFecBiom_fig 





#Size class (all fish) as a proportion of the biomass model
#protection 
cols <- c("PropSmallBiom" = palette[3], "PropMedBiom" = palette[5], "PropLargeBiom" = palette[7])
ProtSizeBiom_fig <- ggplot(data = ce_ProtSizeBiom, aes(x = Protection, y = estimate__, color = cats__))+
  geom_point(position=position_dodge(0.4), size = 5)+
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.3,
                position=position_dodge(0.4), linewidth = 1.5)+
  scale_color_manual(values = cols, labels = c("Small (<=17.5cm)", "Medium size (17.5-27.5cm)", "Large size (>=27.5cm)"))+
  theme +
  theme(legend.title = element_blank())+
  scale_x_discrete(labels = c("Fished", "Restricted", "Unfished"))+
  ylab("Proportion of total biomass")+
  xlab("")+
  ggtitle("Size classes as a proportion of the total biomass")+
  ylim(0,1)
ProtSizeBiom_fig

#biomass
SizeBiom_fig <- ggplot(data = ce_BiomSizeBiom, aes(x = sLogMeanAllFishBiomassKgHa, y = estimate__, fill = cats__))+
  geom_line()+
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.6)+
  scale_fill_manual(values = cols, labels = c("Small (<=17.5cm)", "Medium size (17.5-27.5cm)", "Large size (>=27.5cm)"))+
  theme +
  theme(legend.title = element_blank(), legend.position = 'none')+
  ylab("Proportion of total biomass")+
  xlab("Scaled biomass")+
  ylim(0,1)
  #ggtitle("Size classes as a proportion of the total biomass")
SizeBiom_fig 


###############
proportionMatureFit <- readRDS("output/proportionMatureFit.rds")
summary(proportionMatureFit)
ce_propMature <- conditional_effects(proportionMatureFit, "sLogMeanAllFishBiomassKgHa",
                                       prob = 0.95, ndraws =1000, re_formula = NULL) 
ce_propMature <- as.data.frame(ce_propMature$sLogMeanAllFishBiomassKgHa)


dat <- proportionMatureFit$data

propMature_fig <- ggplot(data = ce_propMature, aes(x = sLogMeanAllFishBiomassKgHa, y = estimate__))+
  geom_line(size = 1)+
  geom_point(data = dat, aes(x = sLogMeanAllFishBiomassKgHa, mean_prop_mature), alpha = 0.2)+   
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2, fill = palette[1])+
  theme +
  theme(legend.title = element_blank(), legend.position = 'none')+
  ylab("Proportion of fish above/at length at maturity")+
  xlab("Scaled biomass")+
  ylim(0,1)
#ggtitle("Size classes as a proportion of the total biomass")
propMature_fig

ce_propMature <- conditional_effects(proportionMatureFit, "Protection",
                                       prob = 0.95, ndraws =1000, re_formula = NULL) 
ce_propMature <- as.data.frame(ce_propMature$Protection)


#proportion mature vs. protection
proportionMatureFit <- readRDS("output/proportionMatureFit.rds")
ce_propMature <- conditional_effects(proportionMatureFit, "Protection",
                                       prob = 0.95, ndraws =1000, re_formula = NULL) 
ce_propMature  <- as.data.frame(ce_propMature$Protection)

ProtMatureBiom_fig <- ggplot(data = ce_propMature, aes(x = Protection, y = estimate__))+
  geom_point(position=position_dodge(0.4), size = 5)+
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.3,
                position=position_dodge(0.4), linewidth = 1.5)+
  theme +
  theme(legend.title = element_blank())+
  scale_x_discrete(labels = c("Fished", "Restricted", "Unfished"))+
  ylab("Proportion of total biomass")+
  xlab("")+
  ggtitle("Fish >= length at maturity as a \nproportion of the total biomass")+
  ylim(0,1)
ProtMatureBiom_fig



####################
proportions_fig <- cowplot::plot_grid(ProtFecBiom_fig, ProtAbund_fig, ProtSizeBiom_fig, BiomFecBiom_fig, 
                             BiomAbund_fig,SizeBiom_fig, labels = "AUTO", align = "h", label_size = 20)
proportions_fig


pdf(file = "output/logistic_models_grid_fec&size.pdf",  
    width = 25, 
    height = 10) 
proportions_fig
dev.off()






pdf(file = "Output/supplement/proportionMature_vsBiomass_model.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

propMature_fig

# Step 3: Run dev.off() to create the file!
dev.off()


pdf(file = "Output/supplement/proportionMature_vsProtection_model.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ProtMatureBiom_fig

# Step 3: Run dev.off() to create the file!
dev.off()

