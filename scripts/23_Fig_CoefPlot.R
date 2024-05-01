#set theme
theme <-  theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size =15),
                axis.title = element_text(size =15),
                axis.line = element_line(size = 0.5, linetype = "solid",
                                         colour = "black"),
                legend.text=element_text(size=15), 
                #legend.title=element_text(face="bold"),
                plot.title = element_text(size=15))

#load models
fec_mod_se <- readRDS("output/brms_fecundity_se.rds")
fec_mod_ser <- readRDS("output/SERRANIDAE_brms_fecundity.rds")
fec_mod_scar <- readRDS("output/SCARIDAE_brms_fecundity.rds")
fec_mod_lut <- readRDS("output/LUTJANIDAE_brms_fecundity.rds")

#function to make plot
make_coefplot <- function(model, title = "Coefficient Plot", y_limits = c(-3.5, 3.5)) {
  posteriorestimates <- as.data.frame(posterior_summary(model, probs = c(0.05, 0.95)))
  
  # Coefficient plot setup
  coefplot <- posteriorestimates[2:17,]
  coefplot$variable <- c("> 10m Depth", "< 4m Depth", "Reef crest", "Reef flat", "Reef lagoon", 
                         "Fishing restricted", "High compliance reserve", "Distance sampling method", 
                         "Point intercept method", "Sampling area", "Total gravity", "Local population growth", 
                         "Ocean productivity", "Climate stress index", "Population size", "Reef fish landings")
  coefplot$sign <- ifelse(coefplot$Q5 < 0 & coefplot$Q95 < 0, "negative", ifelse(coefplot$Q5 > 0 & coefplot$Q95 > 0, "positive", "no effect"))
  coefplot$strength <- ifelse(coefplot$sign == "no effect", "open", "closed")
  coefplot$order <- c(4, 5, 6, 7, 8, 11, 12, 3, 2, 1, 16, 15, 10, 9, 13, 14)
  coefplot <- coefplot[order(coefplot$order),]
  coefplot$variable <- factor(coefplot$variable, levels = coefplot$variable[order(coefplot$order)])
  
  # Creating the plot
  coefplot_gg <- ggplot(coefplot, aes(x = variable, y = Estimate, ymin = Q5, ymax = Q95)) +
    geom_pointrange(aes(colour = sign, shape = strength), size = 1.5) +
    scale_color_manual(values = c("no effect" = "black", "negative" = "black", "positive" = "grey"), guide = FALSE) +
    scale_shape_manual(values = c("open" = 1, "closed" = 19), guide = FALSE) +
    theme(legend.position = "none") +
    geom_hline(aes(yintercept = 0), colour = "dark grey", linetype = "dashed") +
    scale_y_continuous("", limits = y_limits) +
    ggtitle(title) +
    xlab("") +
    theme +
    coord_flip()
  
  return(coefplot_gg)
}


all_plot <- make_coefplot(fec_mod_se, "All families")
lut_plot <- make_coefplot(fec_mod_lut, "Lutjanidae")
scar_plot <- make_coefplot(fec_mod_scar, "Labridae (Scarini)")
ser_plot <- make_coefplot(fec_mod_ser, "Serranidae")
# Modify the plots to remove y-axis labels from lut_plot, scar_plot, and ser_plot
lut_plot <- lut_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
scar_plot <- scar_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ser_plot <- ser_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

#make combined plot
combined_plot <- cowplot::plot_grid(all_plot, lut_plot, scar_plot, ser_plot, 
                           ncol = 4, 
                           rel_widths = c(0.46, 0.18, 0.18, 0.18),
                           labels = c("A", "B", "C", "D"), # Label each plot
                           label_size = 18)
combined_plot 

#save
pdf(file = "output/coef_plot_comb.pdf",   
    width = 20, 
    height = 10) 

combined_plot

dev.off()



############# supplementary coef plot ################
ser_hurdlelog <- readRDS("output/SERRANIDAE_brms_fecundity_hurdlelog.rds")
scar_hurdlelog <- readRDS("output/SCARIDAE_brms_fecundity_hurdlelog.rds")
lut_hurdlelog <- readRDS("output/LUTJANIDAE_brms_fecundity_hurdlelog.rds")

lut_plot <- make_coefplot(lut_hurdlelog, "Lutjanidae")
scar_plot <- make_coefplot(scar_hurdlelog, "Labridae (Scarini)")
ser_plot <- make_coefplot(ser_hurdlelog, "Serranidae")
# Modify the plots to remove y-axis labels from lut_plot, scar_plot, and ser_plot
lut_plot <- lut_plot 
scar_plot <- scar_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ser_plot <- ser_plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


combined_plot <- cowplot::plot_grid(lut_plot, scar_plot, ser_plot, 
                           ncol = 3, 
                           rel_widths = c(0.45, 0.275, 0.275),
                           labels = c("A", "B", "C"), # Label each plot
                           label_size = 18)
combined_plot 

#save
pdf(file = "output/supp_hurdlelog_coef_plot_comb.pdf",   
    width = 20, 
    height = 10) 

combined_plot

dev.off()
