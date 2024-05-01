#colors
cols <- brewer.pal(n=8, "Dark2")[c(1:3,6)]

#theme
theme <-  theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size =20),
                axis.title = element_text(size =20),
                axis.line = element_line(size = 0.5, linetype = "solid",
                                         colour = "black"),
                legend.title=element_text(size=20), 
                legend.text=element_text(size=20),
                plot.title = element_text(size=30))

#read in output from simulations and get summaries
posts_30GB_mean <- readRDS("output/fecundity_potential_mean_GB_allfish.rds")
posts_30GB_mean <- posts_30GB_mean %>% group_by(Geographic_Basin) %>% mutate(mean_percent_diff_by_gb =
                                                                               mean(percent_diff))
posts_30GB_mean_ser <- readRDS("output/fecundity_potential_GB_serranidae.rds")
posts_30GB_mean_ser <- posts_30GB_mean_ser %>% group_by(Geographic_Basin) %>% mutate(mean_percent_diff_by_gb =
                                                                               mean(percent_diff))
posts_30GB_mean$FACTOR <- "factor"
posts_30GB_mean$FACTOR <- as.factor(posts_30GB_mean$FACTOR)
posts_30GB_mean %>% group_by(Geographic_Basin) %>% median_qi(percent_diff)
posts_30GB_mean %>% group_by(Geographic_Basin) %>% median_qi(log(mean30+1))

posts_30GB_mean_ser %>% group_by(Geographic_Basin) %>% median_qi(percent_diff)
posts_30GB_mean_ser %>% group_by(Geographic_Basin) %>% median_qi(log(meanGB+1))
posts_30GB_mean_ser %>% group_by(Geographic_Basin) %>% median_qi(log(mean30+1))


#plot all families
all <- ggplot()+
  stat_pointinterval(data = posts_30GB_mean, aes(x = reorder(label, -orderFig), y = percent_diff,
                                                 color = label, size = mean_percent_diff_by_gb), 
                     alpha = 1)+
  scale_size(range = c(0, 15))+
  scale_color_manual(values = cols)+
  theme+
  coord_flip()+
  ylab("Percent gain in fecundity")+
  xlab(" ") +
  theme(legend.position = "none")+
  ggtitle("All families")+ylim(0,40)

all 





#plot serranidae
serranidae <- ggplot()+
  stat_pointinterval(data = posts_30GB_mean_ser, aes(x = reorder(label, -orderFig), y =(percent_diff),
                                                 color = label, size = mean_percent_diff_by_gb),
                     alpha = 1)+
  scale_size(range = c(50, 100))+
  scale_color_manual(values = cols)+
  theme+
  coord_flip()+
  ylab("Percent gain in fecundity")+
  xlab(" ") +
  theme(legend.position = "none")+
  theme(axis.text.y = element_blank())+
  ggtitle("Serranidae")+ylim(0,200)
serranidae

#plot together
fecundity_potential <- all | serranidae
fecundity_potential <- fecundity_potential + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 20))
fecundity_potential

########################################################
#save 
pdf(file = "output/fecundity_potential_GB.pdf",   
    width = 15, # The width of the plot in inches
    height = 10) # The height of the plot in inches

fecundity_potential

dev.off()








