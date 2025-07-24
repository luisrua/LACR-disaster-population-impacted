# QUICK ASSESSMENT OF HEALTH FACILITIES IN PRONE DISASTER AREAS LAC REGION #
## Luis de la Rua - July 2025

# SETTINGS =================================
source("setup.R")

# Paths
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"
man_path <- "C:/GIS/UNFPA GIS/HF/LAC/layers/raw/manual_hf/" # where layers downloaded manually are stored
tables <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/tables/"
plots <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/plots/"

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

# 5. Intersect with Admin Boundaries and Hazard zones to get number of Health Facilities in Hazard prone areas =====
# Admin boundaries
ab <- st_read(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))

# Hazard prone areas
# Impact zones
req_pol <- st_read(paste0(layers,"processed/req_pol_54034.gpkg"))
rwind_pol <- st_read(paste0(layers,"processed/rwind_pol_54034.gpkg"))
rflood_pol <- st_read(paste0(layers,"processed/flood_lac_54034.gpkg"))
two_hazard <- st_read(paste0(layers, "processed/two_hazard.gpkg"))
three_hazard <- st_read(paste0(layers, "processed/three_hazard.gpkg"))

# Named list of your three impact zones
impact_zones <- list(
  req_pol = req_pol,
  rwind_pol = rwind_pol,
  rflood_pol = rflood_pol,
  two_hazard = two_hazard,
  three_hazard = three_hazard
)

# Loop through each hazard polygon and join
hf_counts <- map_dfr(names(impact_zones), function(hazard_name) {
  
  # Get the polygon
  hz <- impact_zones[[hazard_name]]
  
  # Spatial join: facilities intersecting this hazard zone
  hf_in_zone <- st_join(hf_spat, hz, left = FALSE)
  
  # Add hazard name as a column
  hf_in_zone$hazard_type <- hazard_name
  
  # Group and count
  hf_in_zone %>%
    st_drop_geometry() %>%
    group_by(iso3, hazard_type, amenity) %>%
    summarise(n_facilities = n(), .groups = "drop")
})

# Reshape to get number of HF by hazard zone and by amentity
hf_summary_wide <- hf_counts %>%
  pivot_wider(
    names_from = c(hazard_type, amenity),  # multiple columns into name
    values_from = n_facilities,
    values_fill = 0
  )

# Calculate total of Health Facilities too
hf_summary <- hf_summary_wide %>% 
  mutate(
    total_hf_req = rowSums(select(., starts_with("req_pol")), na.rm = T),
    total_hf_rwind = rowSums(select(., starts_with("rwind_pol")), na.rm = T),
    total_hf_rflood = rowSums(select(., starts_with("rflood_pol")), na.rm = T),
    total_hf_two_hazard = rowSums(select(., starts_with("two_hazard")), na.rm = T),
    total_hf_three_hazard = rowSums(select(., starts_with("three_hazard")), na.rm = T)
    ) %>% 
  relocate(total_hf_req, .before = req_pol_clinic) %>%
  relocate(total_hf_rwind, .before = rwind_pol_clinic) %>%
  relocate(total_hf_rflood, .before = rflood_pol_clinic) %>%
  relocate(total_hf_two_hazard, .before = two_hazard_clinic) %>%
  relocate(total_hf_three_hazard, .before = three_hazard_clinic)

# Calculate all number of healthfacilities by country and amenity
hf_summary_all <- hf_spat %>% 
  as.data.frame() %>% 
  group_by(iso3, amenity) %>%
  summarize(n_hf = n()
            ) %>% 
  pivot_wider(
    names_from = amenity,
    values_from = n_hf,
    values_fill = 0
  ) %>% 
  mutate(total_hf  = clinic + hospital)

# Merge both tables
hf_in_hzones <- merge(hf_summary_all , hf_summary, by = "iso3")

# Calculate the percentages
hf_in_hzones <- hf_in_hzones %>%
  mutate(
    pct_req_clinic = (req_pol_clinic / clinic) * 100,
    pct_req_hospital = (req_pol_hospital / hospital) * 100,
    pct_rec_total_hf = (total_hf_req / total_hf)*100,
    pct_rwind_clinic = (rwind_pol_clinic / clinic) * 100,
    pct_rwind_hospital = (rwind_pol_hospital / hospital) * 100,
    pct_rwind_total_hf = (total_hf_rwind / total_hf) * 100,
    pct_rflood_clinic = (rflood_pol_clinic / clinic) * 100,
    pct_rflood_hospital = (rflood_pol_hospital / hospital) * 100,
    pct_rflood_total_hf = (total_hf_rflood / total_hf) * 100,
    pct_two_hazard_clinic = (two_hazard_clinic / clinic) * 100,
    pct_two_hazard_hospital = (two_hazard_hospital / hospital) * 100,
    pct_two_hazard_total_hf = (total_hf_two_hazard/ total_hf) * 100,
    pct_three_hazard_clinic = (three_hazard_clinic / clinic) * 100,
    pct_three_hazard_hospital = (three_hazard_hospital / hospital) * 100,
    pct_three_hazard_total_hf = (total_hf_three_hazard / total_hf) * 100,
  )

# Export
write.csv(hf_in_hzones, paste0(tables,"lac_hf_in_hzones.csv"), row.names = FALSE)

# 6. Plot some graphs to better explain the trends
# Prepare tables too
country_codes <- data.frame(
  ISO3 = c("ABW", "AIA", "ARG", "ATG", "BHS", "BLZ", "BMU", "BOL", "BRA", "BRB",
           "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GLP",
           "GRD", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MEX",
           "MSR", "MTQ", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "TCA",
           "TTO", "URY", "VCT", "VEN", "VGB"),
  Country = c("Aruba", "Anguilla", "Argentina", "Antigua and Barbuda", "Bahamas",
              "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Chile", "Colombia",
              "Costa Rica", "Cuba", "Curaçao", "Cayman Islands", "Dominica", "Dominican Republic",
              "Ecuador", "Guadeloupe", "Grenada", "Guatemala", "French Guiana", "Guyana",
              "Honduras", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia",
              "Mexico", "Montserrat", "Martinique", "Nicaragua", "Panama", "Peru", "Puerto Rico",
              "Paraguay", "El Salvador", "Suriname", "Turks and Caicos Islands", "Trinidad and Tobago",
              "Uruguay", "Saint Vincent and the Grenadines", "Venezuela", "British Virgin Islands")
)

# Separate country codes for Latin America Discuss classification this is from UNSD
#https://unstats.un.org/unsd/methodology/m49/

car_count_list <-  c(
  "AIA", "ATG", "ABW", "BHS", "BMU", "BRB", "BLZ", "BES", "VGB", "CYM", "CUW",
  "DMA", "GRD", "GLP", "HTI", "JAM", "MTQ", "MSR", "PRI", "BLM", "KNA",
  "LCA", "MAF", "VCT", "SXM", "SUR", "TTO", "TCA", "VIR", "GUF", "GUY"
)

hf_long_pct <- hf_in_hzones %>%
  select(iso3, starts_with("pct_")) %>%
  select(iso3, ends_with("total_hf")) %>% 
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "hazard_type",
    values_to = "percentage"
  ) %>%
  mutate(hazard_type = gsub("pct_hf_", "", hazard_type)) %>%   # clean names
  mutate(iso3 = toupper(iso3))

hf_long_pct_lat <- hf_long_pct %>% 
  filter(!(iso3 %in% car_count_list))

hf_long_pct_car <- hf_long_pct %>% 
  filter(iso3 %in% car_count_list)

plot_hf_per_lat <- ggplot(hf_long_pct_lat, aes(x = iso3, y = percentage, fill = hazard_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of Health Facilities by Hazard Prone Zone - Latin America Region (%)",
    x = "Country (ISO3)",
    y = "Percentage",
    fill = "Hazard Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c(
      pct_rec_total_hf = "#bf9000",
      pct_rwind_total_hf  = "#34a853",
      pct_rflood_total_hf = "#4a86e8",
      pct_two_hazard_total_hf = "#9900ff",
      pct_three_hazard_total_hf  = "#ff6d01"
    ),
    labels = c(
      pct_rec_total_hf = "Health Facilities - Earthquake zone",
      pct_rwind_total_hf = "Health Facilities - Hurricane winds zone",
      pct_rflood_total_hf = "Health Facilities - Riverine floods zone",
      pct_two_hazard_total_hf = "Health Facilities - Two Hazards zone",
      pct_three_hazard_total_hf = "Health Facilities - Three Hazards zone"
    )
  )
plot_hf_per_lat

ggsave(paste0(plots,"plot_hf_per_lat.png"), plot = plot_hf_per_lat, 
       width = 10, height = 6, dpi = 300, bg = "white")

plot_hf_per_car <- ggplot(hf_long_pct_car, aes(x = iso3, y = percentage, fill = hazard_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percentage of Health Facilities by Hazard Prone Zone - Caribbean Region (%)",
    x = "Country (ISO3)",
    y = "Percentage",
    fill = "Hazard Type"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c(
      pct_rec_total_hf = "#bf9000",
      pct_rwind_total_hf  = "#34a853",
      pct_rflood_total_hf = "#4a86e8",
      pct_two_hazard_total_hf = "#9900ff",
      pct_three_hazard_total_hf  = "#ff6d01"
    ),
    labels = c(
      pct_rec_total_hf = "Health Facilities - Earthquake zone",
      pct_rwind_total_hf = "Health Facilities - Hurricane winds zone",
      pct_rflood_total_hf = "Health Facilities - Riverine floods zone",
      pct_two_hazard_total_hf = "Health Facilities - Two Hazards zone",
      pct_three_hazard_total_hf = "Health Facilities - Three Hazards zone"
    )
  )
plot_hf_per_car

ggsave(paste0(plots,"plot_hf_per_car.png"), plot = plot_hf_per_car, 
       width = 10, height = 6, dpi = 300, bg = "white")
