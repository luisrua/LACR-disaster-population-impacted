# This script collects, filters and process WorldPop population grids available
# in WorldPop website and merge them to get age ranges population datasets
# for the whole LAC region


# Luis de la Rua February 2024

# SETTINGS ----
source("setup.R")


library(remotes)
# iso3codes <- c('abw','aia','arg','atg','bhs','blz','bmu','bol','brb','bra',
#               'chl','col','cri','cub','cuw','cym','dma','dom','ecu','glp',
#               'grd','gtm','guf','guy','hnd','hti','jam','kna','lca','mex',
#               'msr','mtq','nic','pan','per','pri','pry','slv','sur','tca',
#               'tto','ury','vct','ven','vgb')

iso3codes <- c('mex')

test_country <- c('bmu','bol','brb','bra',
                  'chl','col','cri','cub','cuw')

# Removing the vbig datasets that give error on the bulk download
iso3codes_bulk <- c('abw','aia','atg','bhs','blz','bmu','bol','brb',
               'chl','col','cri','cub','cuw','cym','dma','dom','ecu','glp',
               'grd','gtm','guf','guy','hnd','hti','jam','kna','lca',
               'msr','mtq','nic','pan','per','pri','pry','slv','sur','tca',
               'tto','ury','vct','ven','vgb')


# Paths to the directory we store the data
dir <- "C:/GIS/UNFPA GIS/HF/LAC/"


# # TEST wopr ----
# remotes::install_github("ropensci/geojsonlint")
# remotes::install_github("wpgp/wopr", dependencies = T)
# library(wopr)
# 
# # Paths to the directory we store the data
# dir <- "C:/GIS/UNFPA GIS/HF/LAC/"
# 
# # 1. SEARCH DATA WE ARE AFTER IN WPOP SERVER
# iso3codes <- c('abw','aia','arg','atg','bhs','blz','bmu','bol','brb','bra',
#                'chl','col','cri','cub','cuw','cym','dma','dom','ecu','glp',
#                'grd','gtm','guf','guy','hnd','hti','jam','kna','lca','mex',
#                'msr','mtq','nic','pan','per','pri','pry','slv','sur','tca',
#                'tto','ury','vct','ven','vgb')
# iso3codes<- toupper(iso3codes)
# # Retrieve the WOPR data catalogue
# catalogue <- getCatalogue()
# testsel <- catalogue %>%
#   subset(country == "BFA" & category == "Population" )
# 
# # wopr repository only for AFRICA



# PACKAGE wpgpDownloadR =======================================================
# load package
install.packages("devtools")
devtools::install_github("wpgp/wpgpDownloadR")

library(wpgpDownloadR)

list_iso <- wpgpListCountries()
# testlist <- wpgpListCountryDatasets(ISO3="ABW")

# 1. DOWNLOAD ALL NECESSARY DATASETS FROM WORLDPOP SERVER ======================
 
# 1.1 Create age groups to retrieve data from the server -----

# Youth - Male and female 0-19 years old
youth <- c("agesex_f_0_2020","agesex_f_1_2020","agesex_f_5_2020","agesex_f_10_2020","agesex_f_15_2020",
           "agesex_m_0_2020","agesex_m_1_2020","agesex_m_5_2020","agesex_m_10_2020","agesex_m_15_2020") 

# Women fertility age (15-49)
wfage <- c("agesex_f_15_2020","agesex_f_20_2020","agesex_f_25_2020","agesex_f_30_2020","agesex_f_35_2020",
           "agesex_f_40_2020","agesex_f_45_2020")

# Elder Male and Female (65+)
old <- c("agesex_f_65_2020","agesex_f_70_2020","agesex_f_75_2020","agesex_f_80_2020",
         "agesex_m_65_2020","agesex_m_70_2020","agesex_m_75_2020","agesex_m_80_2020")

old60 <- c("agesex_f_60_2020","agesex_f_65_2020","agesex_f_70_2020","agesex_f_75_2020","agesex_f_80_2020",
           "agesex_m_60_2020","agesex_m_65_2020","agesex_m_70_2020","agesex_m_75_2020","agesex_m_80_2020")


# 1.2 Bulk Download YOUTH WFAG AND ELDER -----
# I am not looping altogether as the process seems to be very long and likely to 
# get interrupted
# ARG, BRA and MEX bring corrupted files, download manually
# https://hub.worldpop.org/geodata/listing?id=30
# Youth
tic()
for (iso in iso3codes_bulk){
  for (covariate in youth){
  df <- wpgpGetCountryDataset(ISO3 = iso,
                              covariate = covariate,
                              destDir = paste0(dir, "data/input_data/wpop_youth"))
  # Get file information
  file_path <- paste0(dir, "data/input_data/wpop_youth/", iso, "_", covariate, ".tif")
  file_info <- file.info(file_path)
  
    # Check if the file is empty
    if (file_info$size == 0) {
      cat("Warning: File is empty -", iso, covariate, "\n")
    }
  }
}
toc()

# Women Reproductive Age
tic()
for (iso in iso3codes_bulk){
  for (covariate in wfage){
    df <- wpgpGetCountryDataset(ISO3 = iso,
                                covariate = covariate,
                                destDir = paste0(dir, "data/input_data/wpop_wfage"))
    # Get file information
    file_path <- paste0(dir, "data/input_data/wpop_wfage/", iso, "_", covariate, ".tif")
    file_info <- file.info(file_path)
    
    # Check if the file is empty
    if (file_info$size == 0) {
      cat("Warning: File is empty -", iso, covariate, "\n")
    }
  }
}
toc()

# Elder
tic()
for (iso in iso3codes_bulk){
  for (covariate in old){
    df <- wpgpGetCountryDataset(ISO3 = iso,
                                covariate = covariate,
                                destDir = paste0(dir, "data/input_data/wpop_old"))
    # Get file information
    file_path <- paste0(dir, "data/input_data/wpop_old/", iso, "_", covariate, ".tif")
    file_info <- file.info(file_path)
    
    # Check if the file is empty
    if (file_info$size == 0) {
      cat("Warning: File is empty -", iso, covariate, "\n")
    }
  }
}
toc()

# Elder 60+

tic()
for (iso in iso3codes_bulk){
  for (covariate in old60){
    df <- wpgpGetCountryDataset(ISO3 = iso,
                                covariate = covariate,
                                destDir = paste0(dir, "data/input_data/wpop_old60"))
    # Get file information
    file_path <- paste0(dir, "data/input_data/wpop_old60/", iso, "_", covariate, ".tif")
    file_info <- file.info(file_path)
    
    # Check if the file is empty
    if (file_info$size == 0) {
      cat("Warning: File is empty -", iso, covariate, "\n")
    }
  }
}
toc()

# 2. PROCESSING RASTERS BROUGHT FROM WORLDPOP ==================================

# 2.1 SUM ALL THE RASTERS FOR EACH COUNTRY AND AGE TARGET ----------------------

# Review that all the datasets are bigger than 0 otherwise may create errors / 
# introduce this check in loops

# 2.1.1 YOUTH ----- 

# Youth output dir 
y_dir <- paste0(dir, "data/input_data/wpop_youth_sum/")

for (iso in iso3codes) {
  
  iso_files <- list.files(
    paste0(dir, "data/input_data/wpop_youth"),
    pattern = paste0("^", iso, ".*\\.tif$"),
    full.names = TRUE
  )
  
  print(paste("Processing", iso, "- found files:"))
  print(iso_files)
  
  if (length(iso_files) > 0) {
    # Load all .tif files as multilayer raster
    r_stack <- rast(iso_files)
    
    # Sum layers efficiently
    r_sum <- sum(r_stack, na.rm = TRUE)
    
    # Compute total population
    total_pop <- global(r_sum, sum, na.rm = TRUE)[1, 1]
    print(paste0(iso, "_youth: ", as.integer(total_pop)))
    
    # Save the summed raster with compression
    writeRaster(
      r_sum,
      filename = paste0(y_dir, iso, "_wpop_youth.tif"),
      overwrite = TRUE,
      wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE")
    )
    
  } else {
    cat("No files found for ISO:", iso, "\n")
  }
}

# 2.1.2 WOMEN OF REPRODUCTIVE AGE 15-49 -----
w_dir <- paste0(dir, "data/input_data/wpop_wfage_sum/")

for (iso in iso3codes) {
  
  iso_files <- list.files(
    paste0(dir, "data/input_data/wpop_wfage"),
    pattern = paste0("^", iso, ".*\\.tif$"),
    full.names = TRUE
  )
  
  print(paste("Processing", iso, "- found files:"))
  print(iso_files)
  
  if (length(iso_files) > 0) {
    # Load all .tif files as multilayer raster
    r_stack <- rast(iso_files)
    
    # Sum layers efficiently
    r_sum <- sum(r_stack, na.rm = TRUE)
    
    # Compute total population
    total_pop <- global(r_sum, sum, na.rm = TRUE)[1, 1]
    print(paste0(iso, "_wfag: ", as.integer(total_pop)))
    
    # Save the summed raster with compression
    writeRaster(
      r_sum,
      filename = paste0(w_dir, iso, "_wpop_wfag.tif"),
      overwrite = TRUE,
      wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE")
    )
    
  } else {
    cat("No files found for ISO:", iso, "\n")
  }
}

# 2.1.3 ELDER 65 + YEARS OLD -----
o_dir <- paste0(dir, "data/input_data/wpop_old_sum/")

for (iso in iso3codes) {
  
  iso_files <- list.files(
    paste0(dir, "data/input_data/wpop_old"),
    pattern = paste0("^", iso, ".*\\.tif$"),
    full.names = TRUE
  )
  
  print(paste("Processing", iso, "- found files:"))
  print(iso_files)
  
  if (length(iso_files) > 0) {
    # Load all .tif files as multilayer raster
    r_stack <- rast(iso_files)
    
    # Sum layers efficiently
    r_sum <- sum(r_stack, na.rm = TRUE)
    
    # Compute total population
    total_pop <- global(r_sum, sum, na.rm = TRUE)[1, 1]
    print(paste0(iso, "_old: ", as.integer(total_pop)))
    
    # Save the summed raster with compression
    writeRaster(
      r_sum,
      filename = paste0(o_dir, iso, "_wpop_old.tif"),
      overwrite = TRUE,
      wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE")
    )
    
  } else {
    cat("No files found for ISO:", iso, "\n")
  }
}

# 2.1.4 ELDER 60 + YEARS OLD -----
o60_dir <- paste0(dir, "data/input_data/wpop_old_sum60/")

# Loop through each ISO code
for (iso in iso3codes) {
  
  iso_files <- list.files(
    paste0(dir, "data/input_data/wpop_old60"),
    pattern = paste0("^", iso, ".*\\.tif$"),
    full.names = TRUE
  )
  
  print(paste("Processing", iso, "- found files:"))
  print(iso_files)
  
  if (length(iso_files) > 0) {
    # Load all .tif files as multilayer raster
    r_stack <- rast(iso_files)
    
    # Sum layers efficiently
    r_sum <- sum(r_stack, na.rm = TRUE)
    
    # Compute total population
    total_pop <- global(r_sum, sum, na.rm = TRUE)[1, 1]
    print(paste0(iso, "_old: ", as.integer(total_pop)))
    
    # Save the summed raster with compression
    writeRaster(
      r_sum,
      filename = paste0(o60_dir, iso, "_wpop_old60.tif"),
      overwrite = TRUE,
      wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE")
    )
    
  } else {
    cat("No files found for ISO:", iso, "\n")
  }
}
# 3. GENERATE THE LAC REGION POPULATION DATASETS RESAMPLE AND REPROJECT =========

# 3.1 REPROJECT TO 54034 AND RESAMPLE TO 1KM RESOLUTION

# 3.1.1 YOUTH REP + RESAMP  ----
tic()
files <- list.files(path = paste0(dir,"data/input_data/wpop_youth_sum/"), pattern = "\\.tif$", full.names = F)
for (file in files){
  r <- rast(paste0(dir,"data/input_data/wpop_youth_sum/",file))
  
  # process reproject + resample to 1km
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
  writeRaster(r_res, paste0(dir,"data/input_data/wpop_youth_proc/proc_", file), overwrite=T, 
              wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE"))
}
toc()

# 3.1.2 WOMEN  REP AGE REP + RESAMP  ----
tic()
files <- list.files(path = paste0(dir,"data/input_data/wpop_wfage_sum/"), pattern = "\\.tif$", full.names = F)
for (file in files){
  r <- rast(paste0(dir,"data/input_data/wpop_wfage_sum/",file))
  
  # process reproject + resample to 1km
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
  writeRaster(r_res, paste0(dir,"data/input_data/wpop_wfage_proc/proc_", file), overwrite=T,
              wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE"))
}
toc()


# 3.1.3 OLD REP + RESAMP ----
tic()
files <- list.files(path = paste0(dir,"data/input_data/wpop_old_sum/"), pattern = "\\.tif$", full.names = F)
for (file in files){
  r <- rast(paste0(dir,"data/input_data/wpop_old_sum/",file))
  
  # process reproject + resample to 1km
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
  writeRaster(r_res, paste0(dir,"data/input_data/wpop_old_proc/proc_", file), overwrite=T,
              wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE"))
}
toc()

# 3.1.4 OLD60 REP + RESAMP ----
tic()
files <- list.files(path = paste0(dir,"data/input_data/wpop_old_sum60/"), pattern = "\\.tif$", full.names = F)
for (file in files){
  r <- rast(paste0(dir,"data/input_data/wpop_old_sum60/",file))
  
  # process reproject + resample to 1km
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
  writeRaster(r_res, paste0(dir,"data/input_data/wpop_old_proc60/proc_", file), overwrite=T,
              wopt = list(datatype = "INT4S", gdal = "COMPRESS=DEFLATE"))
}
toc()

# 3.2 MOSAIC THE RASTERS TO CREATE THE LAC REGION DATASETS
# Create region template raster using the total population one
wpop_tot <- rast(paste0(dir, "data/input_data/wpop_merged/wpop_lac_54032.tif"))


# 3.2.1 YOUTH ----
files <- list.files(path = paste0(dir,"data/input_data/wpop_youth_proc/"), pattern = "\\.tif$", full.names = T)
  
# This still does not solve the merge issue but definitivelly improves the code.
rlist <- lapply(files,rast)
sprc_list <- sprc(rlist)
wpop_lac <- mosaic(sprc_list, fun = 'sum')


# Plot to see how it looks like
plot(wpop_lac)
  
# Export into tif
writeRaster(wpop_lac, paste0(dir, "data/input_data/wpop_merged/wpop_lac_youth_54032.tif"), overwrite = TRUE)


# 3.2.1 WOMEN REP AGE ----
files <- list.files(path = paste0(dir,"data/input_data/wpop_wfage_proc/"), pattern = "\\.tif$", full.names = T)

# This still does not solve the merge issue but definitivelly improves the code.
rlist <- lapply(files,rast)
sprc_list <- sprc(rlist)
wpop_lac <- mosaic(sprc_list, fun = 'sum')


# Plot to see how it looks like
plot(wpop_lac)

# Export into tif
writeRaster(wpop_lac, paste0(dir, "data/input_data/wpop_merged/wpop_lac_wfage_54032.tif"), overwrite = TRUE)



# 3.2.3 OLD ----
files <- list.files(path = paste0(dir,"data/input_data/wpop_old_proc/"), pattern = "\\.tif$", full.names = T)

# This still does not solve the merge issue but definitivelly improves the code.
rlist <- lapply(files,rast)
sprc_list <- sprc(rlist)
wpop_lac <- mosaic(sprc_list, fun = 'sum')


# Plot to see how it looks like
plot(wpop_lac)

# Export into tif
writeRaster(wpop_lac, paste0(dir, "data/input_data/wpop_merged/wpop_lac_old_54032.tif"), overwrite = TRUE)



# 3.2.4 OLD60 ----
files <- list.files(path = paste0(dir,"data/input_data/wpop_old_proc60/"), pattern = "\\.tif$", full.names = T)


rlist <- lapply(files,rast)
sprc_list <- sprc(rlist)
wpop_lac <- mosaic(sprc_list, fun = 'sum')


# Plot to see how it looks like
plot(wpop_lac)

# crop with ab
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"
ab <- vect(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))
wpop_lac <- crop(wpop_lac, ab)

# Export into tif
writeRaster(wpop_lac, paste0(dir, "data/input_data/wpop_merged/rep_crop_wpop_old60_sum.tif"), overwrite = TRUE)
3