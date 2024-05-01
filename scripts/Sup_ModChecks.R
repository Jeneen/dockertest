#model checks 

# List of model file paths
model_paths <- c("output/lmat_model_out.rds", "output/sex_phy_model_noprotog.rds",
                 "output/fecundity_model.rds", "output/site_fec_biomass_se.rds",
                 "output/brms_20cmBiomass.rds", "output/brms_fecundity_se.rds",
                 "output/brms_matf_se.rds", "output/SERRANIDAE_brms_fecundity_hurdlelog.rds",
                 "output/SERRANIDAE_brms_fecundity_zeros_hurdle.rds",
                 "output/SERRANIDAE_brms_fecundity.rds", "output/SCARIDAE_brms_fecundity_hurdlelog.rds",
                 "output/SCARIDAE_brms_fecundity.rds", "output/LUTJANIDAE_brms_fecundity_hurdlelog.rds",
                 "output/LUTJANIDAE_brms_fecundity.rds")



#function
load_and_prepare_plots <- function(model_path, plot_title) {
  # Load model
  model <- readRDS(model_path)
  
  # Diagnostic plot (as ggplot)
  model_traceplot <- bayesplot::mcmc_trace(model, facet_args = list(ncol = 3),
                                           pars = vars(1:8)) + 
    ggtitle(paste("Trace Plot -", plot_title))+
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
  
  # Posterior predictive checks 
  model_ppcheck <- pp_check(model, ndraws = 10) + ggtitle(paste("PP Check -", plot_title))+
    theme(plot.title = element_text(hjust = 0.5, vjust = -0.5))
  
  # Combine plots for the model
  combined_plots <- cowplot::plot_grid(model_traceplot, model_ppcheck, ncol = 1, labels = "AUTO")
  
  return(combined_plots)}

# save as pdf
pdf("output/model_checks_combined.pdf", width = 15, height = 15)

# Apply function to each model and generate plots
invisible(lapply(model_paths, function(path) {
  file_name <- tools::file_path_sans_ext(basename(path))
  plot_title <- gsub("_", " ", file_name)
  plot <- load_and_prepare_plots(path, plot_title)
  print(plot)
}))

# close pdf
dev.off()

#save summary data
load_and_capture_summary <- function(model_path) {
  # Load the model from the saved RDS file
  model <- readRDS(model_path)
  
  # Use capture.output to grab the summary text
  summary_text <- capture.output(summary(model))
  
  return(summary_text)
}


# Create a new workbook
wb <- createWorkbook()

# Process each model
for (path in model_paths) {
  # Generate the summary text using the function
  summary_text <- load_and_capture_summary(path)
  
  # Get a valid sheet name by truncating to the first 30 characters
  sheet_name <- substr(tools::file_path_sans_ext(basename(path)), 1, 30)
  
  # Add a new worksheet for each model summary
  addWorksheet(wb, sheet_name)
  
  # Write the summary text to the new sheet, one line per row
  writeData(wb, sheet_name, x = as.data.frame(summary_text), startCol = 1, startRow = 1)
}

# Save the workbook
saveWorkbook(wb, "output/model_summaries.xlsx", overwrite = TRUE)
