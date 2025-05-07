# This script collects, filters and process WorldPop population grids available
# within UNFPA repository to carry out regional accessibility assessment

# WE PRODUCE THE TOTAL POPULATION GRID FOR THE LAC REGION

# Luis de la Rua January 2024

# SETTINGS ----
source("setup.R")

# Additional libraries

# LOAD ALL THE DATASETS IN ONE GO DIRECTLY FROM THE REPOSITORY
# libraries to deal with datasets stored on websites and download them directly
# install.packages("rvest")
# install.packages("xml2")
# 
# library(rvest)
# library(httr)

# Paths to the directory we store the data
dir <- "C:/GIS/UNFPA GIS/HF/LAC/"

# Deal with how to avoid big workspace, deal with data INPUT/ OUTPUT
# The approach is to add something like this before we start an expensive process
# if(file.exists("data/xxx.rda")){
#   load("data/xxx.rda")
# } else {
#   xxx <- expensive_creation()
#   save(xxx, file = "data/xxx.rda")
# }

# to control what we download and what countries datasets are missing
# List of countries
iso3codes <- c('abw','aia','arg','atg','bhs','blz','bmu','bol','brb','bra',
               'chl','col','cri','cub','cuw','cym','dma','dom','ecu','glp',
               'grd','gtm','guf','guy','hnd','hti','jam','kna','lca','mex',
               'msr','mtq','nic','pan','per','pri','pry','slv','sur','tca',
               'tto','ury','vct','ven','vgb')

# 1. DOWNLOAD ALL DATASETS FROM THE REPOSITORY THAT STORES THE WPOP LAYERS =====

# Both methods download the tif files corrupted so it is not possible to operate
# them afterwards -> I downloaded the rasters manually from 
# "https://data.worldpop.org/repo/prj/UNFPA_LACRO"


# # URL of the data repository
# url <- "https://data.worldpop.org/repo/prj/UNFPA_LACRO"
# 
# # Read the HTML content of the page
# page <- read_html(url)
# 
# # Extract all download links from the page
# download_links <- page %>%
#   html_nodes("a") %>%
#   html_attr("href")
# 
# # Filter links that point to files (you might need to adjust this depending on the structure of the website)
# file_links <- grep(".*\\.tif$", download_links, value = TRUE)
# length(file_links)
# 
# # Base URL
# base_url <- "https://data.worldpop.org/repo/prj/UNFPA_LACRO/"
# 
# # List of file names
# file_names <- c("ppp_ABW_v2.tif", "ppp_AIA_v1.tif")  # Add all file names
# 
# # Destination directory
# dest_dir <- paste0(dir,"data/input_data/wpop")
# 
# # Set timeout in seconds
# timeout_seconds <- 3600  # 1 hour
# 
# # Download files
# tic()
# for (file_name in file_names) {
#   url <- paste0(base_url, file_name)
#   dest_path <- file.path(dest_dir, file_name)
# 
#   # Use httr for more control, set timeout
#   response <- httr::GET(url, timeout(timeout_seconds))
#   httr::write_disk(dest_path, overwrite = TRUE)
# }
# toc()

# # Download files
# for (file_link in file_links) {
#   # jump to next dataset in case of error
#   tryCatch(
#     {
#   download.file(paste0(url,"/",file_link),
#                 destfile = paste0(dir,"data/input_data/wpop/", file_link))
# },
# error = function(e) {
#   cat("Error downloading", paste0(url,"/",file_link), "Error message:", conditionMessage(e), "\n")
# }
#   )
# }
### download.file download the images corrupted something happens with image files that get problems when loading the rasters

# The datasets that are too big we download them manually (ARG,BOL,BRA,CHL, COL, ECU, MEX, NIC, PAN, PER, PRY, SUR, VEN)

# Also find the missing countries download from worldpop site
wpop_files <- list.files(path = paste0(dir,"data/input_data/wpop/"), pattern = "\\.tif$", full.names = F)
wpop_redux <- substr(wpop_files,5,nchar(wpop_files)-7) %>%
  tolower() %>% 
  print()

missing <- iso3codes[!(iso3codes %in% wpop_redux)] %>%
  print()

# 2. RASTERS PROCESSING.=======================================================

# 2.0 Reproject and resample all the rasters ----
# Deal with long processes that are not necessary if they have been already run
# list of the rasters reprojected in case they are available in the machine

proc_files <- list.files(path = paste0(dir,"data/input_data/wpop_proc_adj/"), pattern = "\\.tif$", full.names = T)

if(all(file.exists(proc_files))) {
  print("PROCESSED RASTERS AVAILABLE")
  
} else {
  tic()
  wpop_files <- list.files(path = paste0(dir,"data/input_data/wpop/"), pattern = "\\.tif$", full.names = F)
  for (file in wpop_files){
    r <- rast(paste0(dir,"data/input_data/wpop/",file))
    
    # process reproject + resample to 1km
    # it would be faster to reproject first but dont know how to resolve the 
    # extent afterwards
    r_rep <- project(r,"ESRI:54034",
                     method = 'bilinear')
    # Calculate total population for each and determine ratio to adjust the total
    # population afterwards
    r_sum <- cellStats(raster(r),stat='sum')
    r_rep_sum <- cellStats(raster(r_rep),stat='sum')
    pop_ratio <- r_sum/r_rep_sum
    
    # Ressample to 1km resoultuion
    r_res <- rast()
    ext(r_res) <- ext(r_rep)
    crs(r_res) <- crs(r_rep)
    res(r_res) <- c(1000,1000)
    r_res <- terra::resample(r_rep,r_res, method = 'sum')
    r_res <- pop_ratio * r_res
    r_res_sum <- cellStats(raster(r_res),stat='sum')
    
    # summarize results
    cat(file,"/ orig_pop:", r_sum, "/ rep_pop:", r_rep_sum, "/ ratio:", pop_ratio, "/ r_res_pop:", r_res_sum,"\n")
    
    # save results
    writeRaster(r_res, paste0(dir,"data/input_data/wpop_proc_adj/proc_", file), overwrite=T)
  }
  toc()
}

# 2.1 Merge all population grids into one  using mosaic function ----
proc_files <- list.files(path = paste0(dir,"data/input_data/wpop_proc_adj/"), pattern = "\\.tif$", full.names = T)

if(file.exists(paste0(dir, "data/input_data/wpop_merged/wpop_lac_54032.tif"))) {
  wpop_lac <- rast(paste0(dir, "data/input_data/wpop_merged/wpop_lac_54032.tif"))
  print("MERGED WPOP DATASET AVAILABLE")
  
} else {
  
  # More efficient than loop, use SpatRasterCollection and run function over it
  tic()
  # List of rasters
  rlist <- lapply(proc_files,rast)
  
  # SpatRasterCollection
  sprc_list <- sprc(rlist)
  
  # Mosaic function
  wpop_lac <- mosaic(sprc_list, fun = 'sum')

  # # First raster
  # tic()
  # wpop_lac <- rast(proc_files[1])
  # 
  # # Loop over the rest of the rasters
  # for (file in proc_files[-1]){
  #   r <- rast(file)
  #   wpop_lac <- mosaic(wpop_lac,r,
  #                     fun='sum')
  # }
  varnames(wpop_lac) <- "ppp_LAC_unfpa" 
  names(wpop_lac) <- "ppp_LAC_unfpa" 
  # Plot to see how it looks like
  plot(wpop_lac)
  
  # Export into tif
  writeRaster(wpop_lac, paste0(dir, "data/input_data/wpop_merged/wpop_lac_54032.tif"), overwrite = TRUE)
  toc()
}


# Make sure all is fine, review totals of the original layers, the processed and also the merged.

# LAC population
LAC_pop <- cellStats(raster(wpop_lac),stat='sum')

# TOTAL POPULATION FROM ORIGINAL DATASETS

wpop_files <- list.files(path = paste0(dir,"data/input_data/wpop/"), pattern = "\\.tif$", full.names = F)
# Initialize a variable to store the total sum
totpop_wpop <- 0
# loop over all the files in folder
for (file in wpop_files){

  r <- rast(paste0(dir,"data/input_data/wpop/",file))
  file_sum <- cellStats(raster(r),stat='sum')
  totpop_wpop <- totpop_wpop + file_sum
  cat("Total sum for", file, ":", file_sum, "\n")
}

# TOTAL POPULATION FROM PROCESSED DATASETS
# Initialize a variable to store the total sum
totpop_wpop_proc <- 0
# loop over all the files in folder
for (file in proc_files){
  
  r <- rast(paste0(file))
  file_sum <- cellStats(raster(r),stat='sum')
  totpop_wpop_proc <- totpop_wpop_proc + file_sum
  cat("Total sum for", file, ":", file_sum, "\n")
}

print(totpop_wpop_proc)
print(totpop_wpop)

totpop_wpop_proc - LAC_pop
totpop_wpop - LAC_pop
totpop_wpop - totpop_wpop_proc

cat("difference of ", totpop_wpop - LAC_pop, " people after the processing")
cat("represents a ", (totpop_wpop - LAC_pop)*100/LAC_pop, "%" )





