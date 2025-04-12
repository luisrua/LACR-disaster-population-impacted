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
tables <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/tables/"

# 1. IMPORT LAYERS =============================================================
# Admin boundaries
ab <- vect(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))


# Impact zones
req_pol <- vect(paste0(layers,"processed/req_pol_54034.gpkg"))
rwind_pol <- vect(paste0(layers,"processed/rwind_pol_54034.gpkg"))
rflood_pol <- vect(paste0(layers,"processed/flood_lac_54034.gpkg"))

# Population grids
tpop <- rast(paste0(layers, "population/wpop_lac_54032.tif"))
ypop <- rast(paste0(layers,"population/rep_crop_wpop_youth_sum.tif_resamp.tif"))
wfagpop <- rast(paste0(layers,"population/rep_crop_wpop_wfag_sum.tif_resamp.tif"))
oldpop <- rast(paste0(layers,"population/rep_crop_wpop_old_sum.tif_resamp.tif"))

# 2. ESTIMATE POPULATION WITHIN IMPACT ZONES BY COUNTRY

# Named list of your three impact zones
impact_zones <- list(
  req_pol = req_pol,
  rwind_pol = rwind_pol,
  rflood_pol = rflood_pol
)

pop_rasters <- list(
  tpop = tpop,
  ypop = ypop,
  wfagpop = wfagpop,
  oldpop = oldpop
)

# Convert countries to sf for exactextractr
ab_sf <- sf::st_as_sf(ab)


results <- list()

for (pop_name in names(pop_rasters)) {
  pop_r <- pop_rasters[[pop_name]]
  
  for (impact_name in names(impact_zones)) {
    zone <- impact_zones[[impact_name]]
    zone <- project(zone, crs(pop_r))  # ensure CRS matches
    
    # Mask and crop
    pop_masked <- mask(crop(pop_r, zone), zone)
    
    # Extract per country
    pop_sum <- exact_extract(pop_masked, ab_sf, 'sum')
    
    # Store result
    temp <- data.frame(
      ISO3 = ab_sf$GID_0,
      impact_area = impact_name,
      population_type = pop_name,
      population = pop_sum
    )
    
    results[[paste(pop_name, impact_name, sep = "_")]] <- temp
  }
}

# Combine all results
final_result <- bind_rows(results)

# View or export
head(final_result)

# Reshape table
wide_impactpop <- final_result %>%
  mutate(type_zone = paste(population_type, impact_area, sep = "_")) %>%
  select(ISO3, type_zone, population) %>%
  pivot_wider(
    names_from = type_zone,
    values_from = population
  )

# # Initialize results list
# results <- list()
# 
# # Loop through each impact zone
# for (name in names(impact_zones)) {
#   zone <- impact_zones[[name]]
#   zone <- project(zone, crs(tpop))  # ensure CRS matches
#   
#   # Mask population raster with the impact zone
#   pop_cropped <- crop(tpop, zone)
#   pop_masked <- mask(pop_cropped, zone)
#   
#   # Exact extract population sum by country
#   pop_by_country <- exact_extract(pop_masked, ab_sf, 'sum')
#   
#   # Store results
#   temp <- data.frame(
#     ISO3 = ab_sf$GID_0,
#     impact_area = name,
#     population = pop_by_country
#   )
#   
#   results[[name]] <- temp
# }
# 
# # Combine results into one dataframe
# final_result <- bind_rows(results)


# Calculate total population by country and rbind
# tpop_iso <- exact_extract(tpop,ab_sf, 'sum')
# 
# tpop_iso <- data.frame(
#   ISO3 = ab_sf$GID_0,
#   tpop = tpop_iso
# )

# Initialize results list
pop_results <- list()

# Loop over rasters
for (pop_name in names(pop_rasters)) {
  pop_r <- pop_rasters[[pop_name]]
  
  # Ensure CRS is matched
  ab_sf_proj <- sf::st_transform(ab_sf, crs(pop_r))
  
  # Extract total population per country
  pop_sums <- exact_extract(pop_r, ab_sf_proj, 'sum')
  
  # Store result
  df <- data.frame(
    ISO3 = ab_sf_proj$GID_0,
    population_type = pop_name,
    population = pop_sums
  )
  
  pop_results[[pop_name]] <- df
}

# Combine results into one dataframe
final_pop_result <- bind_rows(pop_results)

# Optional: reshape to wide format
pop_wide <- final_pop_result %>%
  tidyr::pivot_wider(
    names_from = population_type,
    values_from = population
  )

# View or export
print(pop_wide)

# Merge with total population 
tpop_impact <- merge (pop_wide, wide_impactpop, by = "ISO3")

# Calculate percentages of population impacted for all the population targets
tpop_impact_per <- tpop_impact %>% 
  mutate(
    tpop_req_per = (tpop_req_pol  / tpop) * 100,
    tpop_rwind_per = (tpop_rwind_pol  / tpop) * 100,
    tpop_rflood_per = (tpop_rflood_pol  / tpop) * 100,
    ypop_req_per = (ypop_req_pol  / ypop) * 100,
    ypop_rwind_per = (ypop_rwind_pol  / ypop) * 100,
    ypop_rflood_per = (ypop_rflood_pol  / ypop) * 100,
    wfagpop_req_per = (wfagpop_req_pol  / wfagpop) * 100,
    wfagpop_rwind_per = (wfagpop_rwind_pol  / wfagpop) * 100,
    wfagpop_rflood_per = (wfagpop_rflood_pol  / wfagpop) * 100,
    oldpop_req_per = (oldpop_req_pol  / oldpop) * 100,
    oldpop_rwind_per = (oldpop_rwind_pol  / oldpop) * 100,
    oldpop_rflood_per = (oldpop_rflood_pol  / oldpop) * 100
  )


write.csv(tpop_impact_per, paste0(tables, "tpop_impact_per.csv"))

## Need to deal with NAs especially when estimating impact population 