# This script is for functionality that is very commonly used across
# the actual analysis scripts 

# counter for checking if we have run setup.R before
if(!exists(".setup_counter")){
  .setup_counter <- 0
  quiet <- FALSE
} else {
  quiet <- TRUE
}

source("R/library2.R")
# LIBRARIES 
# General tools
library2("dplyr")
library2("tidyr")
library2("tidyverse")
library2("readxl")
library2("openxlsx")
library2("writexl")
library2("lubridate")
library2("janitor")
library2("tictoc") # processing time
library2("purrr")
library2("RColorBrewer")
library2("conflicted")

  # For spatial analysis
library2("terra")
library2("sp")
library2("sf")
library2("tidyterra")
library2("raster")

# library("spcstyle") # See https://github.com/PacificCommunity/sdd-spcstyle-r-package
# library2("scales")
# library2("rsdmx")
# library2("glue")

# library2("ISOcodes")
# library2("patchwork")   # layout multiple charts in one image
# library2("ggrepel")     # add tet labels to points without overlapping

# CONFLICT WITH FUNCTIONS 
conflict_prefer("select", "dplyr", quiet = quiet)
conflict_prefer("filter", "dplyr", quiet = quiet)
conflict_prefer("year", "lubridate", quiet = quiet)
conflict_prefer("first", "dplyr", quiet = quiet)
conflict_prefer("lag", "dplyr", quiet = quiet)
conflict_prefer("intersect","base", quiet=quiet)

# silence a ubiquitous and annoying message
options(dplyr.summarise.inform = FALSE)
