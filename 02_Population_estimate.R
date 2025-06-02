# 02. POPULATION ESTIMATE
# Overlap processed impact zones with Worldpop dataset to estimate population 
# potentially impacted by disasters.

# SETTINGS
# Libraries
# Libraries
library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
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
two_hazard <- vect(paste0(layers, "processed/two_hazard.gpkg"))
three_hazard <- vect(paste0(layers, "processed/three_hazard.gpkg"))

# Population grids
tpop <- rast(paste0(layers, "population/wpop_lac_54032.tif"))
ypop <- rast(paste0(layers,"population/rep_crop_wpop_youth_sum.tif_resamp.tif"))
wfagpop <- rast(paste0(layers,"population/rep_crop_wpop_wfag_sum.tif_resamp.tif"))
oldpop <- rast(paste0(layers,"population/rep_crop_wpop_old60_sum.tif")) # here the 60+

# 2. ESTIMATE POPULATION WITHIN IMPACT ZONES BY COUNTRY ========================

# Named list of your three impact zones
impact_zones <- list(
  req_pol = req_pol,
  rwind_pol = rwind_pol,
  rflood_pol = rflood_pol,
  two_hazard = two_hazard,
  three_hazard = three_hazard
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

names(wide_impactpop)
# Calculate total population by country 
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



# 3. TABLES AND GRAPHS ========================================================
# 3.1 BASIC TABLES ----
# Calculate percentages of population impacted for all the population targets
tpop_impact_per <- tpop_impact %>% 
  mutate(
    tpop_req_per = (tpop_req_pol  / tpop) * 100,
    tpop_rwind_per = (tpop_rwind_pol  / tpop) * 100,
    tpop_rflood_per = (tpop_rflood_pol  / tpop) * 100,
    tpop_two_hazard_per = (tpop_two_hazard  / tpop) * 100,
    tpop_three_hazard_per = (tpop_three_hazard  / tpop) * 100,
    ypop_req_per = (ypop_req_pol  / ypop) * 100,
    ypop_rwind_per = (ypop_rwind_pol  / ypop) * 100,
    ypop_rflood_per = (ypop_rflood_pol  / ypop) * 100,
    ypop_two_hazard_per = (ypop_two_hazard  / ypop) * 100,
    ypop_three_hazard_per = (ypop_three_hazard  / ypop) * 100,
    wfagpop_req_per = (wfagpop_req_pol  / wfagpop) * 100,
    wfagpop_rwind_per = (wfagpop_rwind_pol  / wfagpop) * 100,
    wfagpop_rflood_per = (wfagpop_rflood_pol  / wfagpop) * 100,
    wfagpop_two_hazard_per = (wfagpop_two_hazard  / wfagpop) * 100,
    wfagpop_three_hazard_per = (wfagpop_three_hazard  / wfagpop) * 100,
    oldpop_req_per = (oldpop_req_pol  / oldpop) * 100,
    oldpop_rwind_per = (oldpop_rwind_pol  / oldpop) * 100,
    oldpop_rflood_per = (oldpop_rflood_pol  / oldpop) * 100,
    oldpop_two_hazard_per = (oldpop_two_hazard  / oldpop) * 100,
    oldpop_three_hazard_per = (oldpop_three_hazard  / oldpop) * 100
  )

write.csv(tpop_impact, paste0(tables, "tpop_impact.csv"))
write.csv(tpop_impact_per, paste0(tables, "tpop_impact_per.csv"))

# When tables already calculated


## Need to deal with NAs especially when estimating impact population 

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

latam_countries <- country_codes %>% 
  filter(!ISO3 %in% car_count_list)
car_countries <- country_codes %>% 
  filter(ISO3 %in% car_count_list)

# Tables for subregions
tot_impact_car <- tpop_impact %>% 
  filter(ISO3 %in% car_count_list)
tot_impac_lat <- tpop_impact %>% 
  filter(!ISO3 %in% car_count_list)

write.csv(tot_impact_car, paste0(tables, "tot_impact_car.csv"))
write.csv(tot_impac_lat, paste0(tables, "tot_impact_lat.csv"))

# View the dataframe
print(country_codes)

tpop_impact_per_table <- tpop_impact_per %>% 
  merge(. , country_codes, by = "ISO3" ) %>% 
  select(c( ISO3, Country, tpop_req_per, tpop_rwind_per, tpop_rflood_per, tpop_two_hazard_per,tpop_three_hazard_per,
            ypop_req_per, ypop_rwind_per, ypop_rflood_per, ypop_two_hazard_per, ypop_three_hazard_per,
            wfagpop_req_per, wfagpop_rwind_per, wfagpop_rflood_per, wfagpop_two_hazard_per, wfagpop_three_hazard_per,
            oldpop_req_per, oldpop_rwind_per, oldpop_rflood_per, oldpop_two_hazard_per, oldpop_three_hazard_per))
  
# Separate for latam
write.csv(tpop_impact_per_table, paste0(tables, "tpop_impact_per_table.csv"))

# Separate for latam
tpop_impact_per_table_latam <- tpop_impact_per_table %>%
  filter(!ISO3 %in% car_count_list)

# And Caribbean
tpop_impact_per_table_car <- tpop_impact_per_table %>%
  filter(ISO3 %in% car_count_list)
# Export both datasets
write.csv(tpop_impact_per_table_latam, paste0(tables, "tpop_impact_per_table_latam.csv"))
write.csv(tpop_impact_per_table_car, paste0(tables, "tpop_impact_per_table_car.csv"))

# 3.2 PLOTS ---
# Plot percentages by country for latam



  
  
