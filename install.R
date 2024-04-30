
install.packages(c("readr", "plyr", "tidyverse", "rfishbase", "fishtree", "ape", "Hmisc", "brms", 
                     "dplyr",  "fields", "ncdf4", "tidybayes", "picante", "parallel", 
                     "broom", "sp", "purrr", "viridis", "RColorBrewer", "ggrepel", 
                     "stringr",  "ggbeeswarm", "lme4", "paletteer", "scales", "patchwork", 
                     "grid", "cowplot", "devtools", "reshape2", "ggrepel", "janitor", "cowplot",
                     "sjPlot", "ggplotify", "openxlsx"))
install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz", 
                 repos=NULL, type="source")
devtools::install_github("nschiett/fishflux", dependencies=TRUE)
