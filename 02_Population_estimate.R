# 02. POPULATION ESTIMATE
# Overlap processed impact zones with Worldpop dataset to estimate population 
# ootentially impacted by disasters.

# SETTINGS
# Libraries
# Libraries
library(terra)
library(tidyterra)
library(tidyverse)
library(exactextractr)
library(openxlsx)

# Paths 
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"
results <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/tables/"

# 1. IMPORT LAYERS =============================================================
# Admin boundaries
ab <- vect(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))

# Population datasets
tpop <- rast(paste0(layers, "population/wpop_lac_54032.tif"))

# Impact zones
req_pol <- vect(paste0(layers,"processed/req_pol_54034.gpkg"))
rwind_pol <- vect(paste0(layers,"processed/rwind_pol_54034.gpkg"))
rflood_pol <- vect(paste0(layers,"processed/flood_lac_54034.gpkg"))

# 2. ESTIMATE POPULATION WITHIN IMPACT ZONES BY COUNTRY

# Named list of your three impact zones
impact_zones <- list(
  req_pol = req_pol,
  rwind_pol = rwind_pol,
  rflood_pol = rflood_pol
)

# Convert countries to sf for exactextractr
ab_sf <- sf::st_as_sf(ab)

# Initialize results list
results <- list()

# Loop through each impact zone
for (name in names(impact_zones)) {
  zone <- impact_zones[[name]]
  zone <- project(zone, crs(tpop))  # ensure CRS matches
  
  # Mask population raster with the impact zone
  pop_cropped <- crop(tpop, zone)
  pop_masked <- mask(pop_cropped, zone)
  
  # Exact extract population sum by country
  pop_by_country <- exact_extract(pop_masked, ab_sf, 'sum')
  
  # Store results
  temp <- data.frame(
    ISO3 = ab_sf$GID_0,
    impact_area = name,
    population = pop_by_country
  )
  
  results[[name]] <- temp
}

# Combine results into one dataframe
final_result <- bind_rows(results)

# Reshape the table 
final_result_long <- final_result %>%
  pivot_wider(names_from = impact_area, values_from = population) %>%
  rename(
    req_pol = `req_pol`,
    rwind_pol = `rwind_pol`,
    rflood_pol = `rflood_pol`
  )

# Calculate total population by country and rbind
tpop_iso <- exact_extract(tpop,ab_sf, 'sum')

tpop_iso <- data.frame(
  ISO3 = ab_sf$GID_0,
  tpop = tpop_iso
)

# Merge with total population 
tpop_impact <- merge (tpop_iso, final_result_long, by = "ISO3")

# Calculate percentages of population impacted
tpop_impact <- tpop_impact %>%
  mutate(
    req_pol_per = (req_pol / tpop) * 100,
    rwind_pol_per = (rwind_pol / tpop) * 100,
    rflood_pol_per = (rflood_pol / tpop) * 100
  )

# Lopp over the 4 population rasters
# Reshape table, bidwith tpop and calculate % 
# rename variables
# and export into csv to share