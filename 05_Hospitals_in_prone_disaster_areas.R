# QUICK ASSESSMENT OF HEALTH FACILITIES IN PRONE DISASTER AREAS LAC REGION #
## Luis de la Rua - July 2025

# SETTINGS =================================
source("setup.R")

# Paths
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"
man_path <- "C:/GIS/UNFPA GIS/HF/LAC/layers/raw/manual_hf/" # where layers downloaded manually are stored

# 1. HEALTH FACILITIES REGIONAL LAYER =========================================
# Use rdhx library to explore and download data info in "https://dickoa.gitlab.io/rhdx/"
library(rhdx)
library(readr)

# Locate and load HF datasets for the region directly from HDX and save them as csv tables
# we use in following steps
# if this does not work we can use HF list
# https://docs.google.com/spreadsheets/d/1lDcPCItS1xfkbZ5PH1xNmEqiGCBknez1YoUMrKDQn7A/edit?usp=drive_link

# List of small countries
iso3codes <- c('abw','aia','arg','atg','bhs','blz','bmu','bol','brb','bra',
               'chl','col','cri','cub','cuw','cym','dma','dom','ecu','glp',
               'grd','gtm','guf','guy','hnd','hti','jam','kna','lca','mex',
               'msr','mtq','nic','pan','per','pri','pry','slv','sur','tca',
               'tto','ury','vct','ven','vgb')
for (code in iso3codes){
  
  # We exclude big countries
  if(!(code %in% c('bra','mex'))) {
    hf <- pull_dataset(paste0("hotosm_",code,"_health_facilities")) %>% 
      get_resource(1) %>% 
      read_resource() %>%
      mutate(geo = sf::st_as_text(geometry)) %>% 
      #Create field with country iso3
      mutate(iso3 = code) %>% 
      
      # Remove geometry that is giving issues when exporting to csv
      st_drop_geometry() %>% 
      write.csv( paste0(layers,"hf/",code, "_health_facilities.csv"))
  }
}

# For big countries we do this separately as the are some particularities for these datasets
# MEXICO fromn hotosm 
search_datasets("mexico Health OSM", rows=20)

# Get the resource
hf <- pull_dataset("hotosm_mex_health_facilities") %>% 
  get_resource(3)

# Download the resource (returns zip path)
zip_path <- download_resource(hf, filename = "mex_health_facilities.zip")

# Unzip (returns the path(s) of extracted files)
unzipped_files <- unzip(zip_path, exdir = "data/mex_hf")

# Read the GeoPackage (assumes only one .gpkg file inside)
gpkg_file <- unzipped_files[grepl("\\.gpkg$", unzipped_files)]
hf_data <- read_sf(gpkg_file)

hf_data %>%
  mutate(
    iso3 = 'mex',
    geo = sf::st_as_text(geom)  # fully qualify to be safe
  ) %>%
  st_drop_geometry() %>%
  write.csv(paste0(layers, "hf/mex_health_facilities.csv"), row.names = FALSE)


# BRASIL (dataset in OSM has been merged)
search_datasets("brazil Health OSM", rows=40)

hf_df <- "hotosm_bra_health_facilities"

hf <- pull_dataset(hf_df) %>% 
  get_resource(4) 
# Download the resource (returns zip path)
zip_path <- download_resource(hf, filename = "bra_health_facilities.zip")

# Unzip (returns the path(s) of extracted files)
unzipped_files <- unzip(zip_path, exdir = "data/bra_hf")

# Read the GeoPackage (assumes only one .gpkg file inside)
gpkg_file <- unzipped_files[grepl("\\.gpkg$", unzipped_files)]

hf_data <- read_sf(gpkg_file)

hf_data %>%
  mutate(
    iso3 = 'mex',
    geo = sf::st_as_text(geom)  # fully qualify to be safe
  ) %>%
  st_drop_geometry() %>%
  write.csv(paste0(layers, "hf/bra_health_facilities.csv"), row.names = FALSE)


# 2. HARMONISE, CLEAN AND CONVERT TO SPATIAL ===================================
# Open all raw datasets check same structure and merge
# Path
raw_csv <- paste0(layers,"hf/")

# Get a list of all CSV files in the folder
csv_files <- list.files(path = raw_csv, pattern = "\\.csv$", full.names = TRUE)

# Check what are the common variables
get_column_names <- function(csv_file) {
  df <- read.csv(csv_file, nrow = 1)  # Read only the first row to get column names
  colnames(df)
}
# Get column names for each CSV file
columns_list <- map(csv_files, get_column_names)

# Find common fields across all CSV files
common_fields <- Reduce(intersect, columns_list)

print(common_fields)

# Merging in one single dataframe and converting into a spatial object

comb_hf <- do.call(rbind, lapply(csv_files, function(csv_file) {
  # Read each CSV file
  df <- read_csv(csv_file, col_types = cols_only(
    iso3 = col_character(),
    osm_id = col_character(),
    name = col_character(),
    amenity = col_character(),
    healthcare = col_character(),
    geo = col_character()
  ))
  # Convert to spatial oject
  sf::st_as_sf(df, wkt= 'geo', crs = 'EPSG:4326')
}))
# plot(comb_hf)

# 3 SUMMARISE BY COUNTRY AND CATEGORIES AS WELL AS CLEANING NAS ===============

# What categories we are going to include check first how data looks like
sum_hf <- comb_hf %>% 
  as_data_frame() %>% 
  group_by(iso3,amenity) %>% 
  summarise(num_hf = n()) %>%
  filter(amenity %in% c('clinic', 'hospital')) %>% 
  print()

sum_hf_allcat <- comb_hf %>% 
  as.data.frame() %>% 
  group_by(iso3,amenity, healthcare) %>% 
  summarise(num_hf = n()) %>%
  print()

# Identify countries with no hospitals or clinics to find inconsistencies in data or countries
# we need to get more datasets for

# Bring in manual hospitals detected using GMaps in countries with no hospitals.
nohosp_iso <- c('aia', 'bmu', 'kna','msr','vgb')
list_df <- list()
# Import hospital locations from manual datasets and merge into one dataset

for (iso in nohosp_iso) {
  list_df[[iso]] <- read_csv(paste0("C:/GIS/UNFPA GIS/HF/LAC/data/input_data/hf/",iso,"_health_facilities.csv"))
}

hf_man <- bind_rows(list_df)
nrow(hf_man)
nrow(comb_hf)

hf_man_spat <-   sf::st_as_sf(hf_man, wkt= 'geo', crs = 'EPSG:4326')

hf_man_spat <- hf_man_spat %>% 
  select(name, amenity, healthcare, osm_id, geo, iso3)

plot(hf_man_spat)

# harmonise dataframe with comb_hf 
hf_man_spat <- hf_man_spat %>%
  mutate(across(c(iso3, osm_id, name, amenity, healthcare), as.character))

# Combine manual locations with comb_hf
hf_completed <- bind_rows(comb_hf,hf_man_spat)

# Check if combined df workded summary number facitliies by country
sum_hf_clhosp <- hf_completed %>% 
  as.data.frame() %>% 
  filter(amenity %in% c('clinic', 'hospital')) %>% 
  group_by(iso3,amenity) %>% 
  summarise(num_hf = n()) %>%
  print()

# write.csv(sum_hf_all,paste0(dir,"tables/sum_hf_all.csv"))

# Identify countries without Hospitals
sum_hf_clhosp %>% 
  filter(is.na(num_hf)) %>% 
  print()

# 4. Filter HF by category, convert to spatial and reproject ===================

# Convert to spatial and reproject into World Cylindrical Equal Area
hf_spat <- hf_completed %>% 
  filter(amenity %in% c('clinic', 'hospital')) %>%
  st_as_sf() %>%
  st_set_crs(st_crs(4326)) %>% 
  st_transform(crs = st_crs('ESRI:54034'))

# Create unique id
hf_spat <- hf_spat %>%   
  mutate(hfid = row_number()) 


st_write(hf_spat,paste0(layers,"/hf/lac_hf_54034.gpkg"), append=F)
