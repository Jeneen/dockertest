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

#functions
se <- function(x, ...) sqrt(var(x, ...)/length(x))

#data
alldata <- readRDS("output/alldata_SDandCIof1000_clean_cinner2020.rda")
fec_biom <- readRDS(file="output/site_fec_biomass_se.rds")


#posterior draws
pred <- data.frame(LogBiomassKGMean = alldata$LogBiomassKGMean,
                   LogFecunditySD = alldata$LogFecunditySD)
posts<-pred %>% 
  add_predicted_draws(fec_biom,
                      re_formula = NA,
                      category='response',
                      ndraws = 4000)

####linear figure
r2<- bayes_R2(fec_biom)
median(r2)

posts <- as_tibble(posts)
posts %>% median_qi(LogBiomassKGMean)
summary(fec_biom)

linear_fig <- ggplot(posts, aes(x = (LogBiomassKGMean), y =(.prediction))) +
  stat_lineribbon(.width = c(.95,.50))+
  geom_point(data=alldata, aes(x = LogBiomassKGMean, y = (LogFecundityMean)), 
             size = 1, alpha = 0.05)+
  theme+
  xlab("Log Biomass + 1 (kg/ha)")+
  ylab("Log Fecundity + 1 (eggs/ha)")+
  scale_fill_manual(values = brewer.pal(8, "PuBuGn")[c(3,5)])+
  guides(fill=guide_legend(title="Credible Interval"))+
  geom_abline(slope = 1, intercept = 12.61, linetype = "dotted",  color = "red", 
              size = 1.5)+
  geom_abline(slope = 1.18, intercept = 12.61, linetype = "dashed", color = "red",
              size =1.5)+
  theme(legend.position = "none")+
  theme+
  ggtitle(bquote('Power relationship, slope=1.02-1.12, median'~R^2~'=0.98'))+
  #  theme(axis.text = element_text(size =10),
  #axis.title = element_text(size =15))+
  scale_x_continuous(breaks = c(1,4,8))
linear_fig


saveRDS(linear_fig, "output/power_law_linear_fig.rds")

