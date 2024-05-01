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
                plot.title = element_text(size=20))


#load data
alldata <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")

#all figs
den_plot <- readRDS(file="output/fec_distribution_fig.rds")
#fec_vs_biom <- readRDS("output/raw_fec_vs_biom.rds")
leg <- readRDS("output/prot_leg.rds")
marg_fec_map <- readRDS("output/map_margCateg_fec.rds")
#country_intercept_fig <- readRDS("output/country_intercept_fig.rds")
linear_fig <- readRDS("output/power_law_linear_fig.rds")
#int_fig <- readRDS("output/top_int_dist_fig.rds")


#make grid
r1 <- cowplot::plot_grid(marg_fec_map, labels= "A", nrow =1, label_size = 15)
r2 <- cowplot::plot_grid(den_plot,  linear_fig,  labels = c('B', 'C'), nrow =1, label_size = 15)
fig <- cowplot::plot_grid(r1, r2,  leg, nrow =3, align="v", label_size = 15)
fig


#save as pdf
pdf("output/main_fig_grid.pdf", width = 15, height =25)
fig
dev.off()
