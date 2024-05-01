# List of packages to install and load
packages <- unique(c("plyr", "tidyverse", "rfishbase", "fishtree", "ape", "Hmisc", "brms", 
                     "dplyr",  "fields", "ncdf4", "tidybayes", "picante", "parallel", 
                      "broom", "sp", "purrr", "viridis", "RColorBrewer", "ggrepel", 
                     "stringr",  "ggbeeswarm", "lme4", "paletteer", "scales", "patchwork", 
                     "grid", "cowplot", "devtools", "reshape2", "ggrepel", "janitor", "cowplot",
                     "sjPlot", "ggplotify", "openxlsx"))

# Function to check if package is installed, install it if not, then load it
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Apply the function over the list of packages
invisible(sapply(packages, install_and_load))


#install fishflux from devtools
devtools::install_github("nschiett/fishflux", dependencies=TRUE)
library(fishflux)

#rgeos is no longer on cran, will have to install archived version 
install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz", 
                 repos=NULL, type="source")
library(rgeos)

############################# Generic functions #######################################

'%!in%' <- function(x,y)!('%in%'(x,y))

inverse_logit = function(x){
  exp(x)/(1+exp(x))
}


vif.mer = function (fit) {
  ## adapted from rms::vif
  v <-vcov(fit)
  nam = names(fixef(fit))
  ## exclude intercepts
  ns = sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v = v[-(1:ns), -(1:ns), drop = FALSE]
    nam = nam[-(1:ns)]
  }
  d = diag(v)^0.5
  v = diag(solve(v/(d %o% d)))
  names(v) = nam
  v
}


standardize = function(x){(x-mean(x, na.rm=T))/(2*sd(x, na.rm=T))} 

panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = abs(cor(x, y, method = "pearson",use = "complete.obs"))
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r*2)
}

