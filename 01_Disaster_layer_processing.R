
# 01 DISASTER RASTER PROCESSING 
# SETTINGS =====================================================================

# Libraries
library(terra)
library(tidyterra)


# Patths
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"

# 1. IMPORT LAYERS =============================================================

# IMPACT ZONES RASTERS
req <- rast(paste0(layers,"GAR Atlas hazard layers/EQ/PGA_475y.grd"))
rwind <- rast(paste0(layers,"GAR Atlas hazard layers/TC WIND/VIENTO_MUNDO_TR100_INT1.grd"))

# for flood there is a raster for each country
floodlist <- list.files(paste0(layers,"GAR Atlas hazard layers/FL/"), pattern = "\\.grd$", full.names = TRUE)

# ADMIN BOUNDARIES
ab <- vect(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))

# 2. CREATE IMPACT ZONES FOR EACH OF THE EVENTS ================================
# Para inundaciones: 50cm
# Para vientos huracanados: 178 m/h (el límite de categoría 3 en la categoría Safir-Simpson)
# Para terremotos: 150cm/s2 (el límite usado en muchos reglamentos de construcción para definir la zona de amenaza intermedia
                           
# 2.1 EARTHQUAKE -----
# The threshold facilitated by UNDDR us 150cm/s2 which is the limit used in building 
# regulation to define intermediate threathen.
eq_limit <- 150

# Set projection for raster
crs(req) <- "EPSG:4326"

# Crop the raster within LAC Region
ab_crop <- project(ab, "EPSG:4326")
req <- crop(req,ab_crop)

# Create the area that includes the Impact zone
req_impact <- req >= eq_limit
plot(req_impact)

req_pol <- as.polygons(req_impact, dissolve = T)

names(req_pol) <- "eq"
req_pol <- req_pol %>% 
  filter(eq == 1)
plot(req_pol)

# And project back into Equal Area Projection
req_pol <- project(req_pol, "ESRI:54034")

writeVector(req_pol, paste0(layers,"processed/req_pol_54034.gpkg"), overwrite = T)

# 2.2 WINDS ------
# 178m/h this is the limit for CAT 3 in Safir - Simpson scale
wind_limit <- 178

# Set projection for raster
crs(rwind) <- "EPSG:4326"

# Crop the raster within LAC Region
rwind <- crop(rwind,ab_crop)
plot(rwind)

# Create the area that includes the Impact zone
rwind_impact <- rwind >= wind_limit
plot(rwind_impact)

rwind_pol <- as.polygons(rwind_impact, dissolve = T)
names(rwind_pol) <- "wind"
rwind_pol <- rwind_pol %>% 
  filter(wind == 1)

plot(rwind_pol)

writeVector(rwind_pol, paste0(layers,"processed/rwind_pol_54034.gpkg"), overwrite = T)

# 2.3 FLOODS -----
# For Floods the threshold is 50cm
fl_limit <- 50
ab_crop
output_dir <- paste0(layers,"processed/flood/")

# Loop through each raster
for (rfile in floodlist) {
  
  # Load raster
  r <- rast(rfile)
  
  # Extract ISO3 code from file name (e.g., "Hazard_ARG__50.tif" → "ARG")
  iso3 <- sub(".*_(\\w{3})__.*", "\\1", basename(rfile))
  
  # Find the matching country polygon
  country_poly <- ab_crop[ab_crop$GID_0 == iso3, ]

  # Crop and mask
  r_crop <- crop(r, country_poly)
  r_mask <- mask(r_crop, country_poly)
  
  # MAsk raster over the areas of impact
  rfl_impact <- r_mask >= fl_limit 
  rfl_pol <- as.polygons(rfl_impact,dissolve = T)
  names(rfl_pol) <- "flood"
  # Write output vector layers where 
  out_path <- file.path(output_dir, paste0("haz_wind_", iso3, "_cropped.gpkg"))
  writeVector(rfl_pol, out_path, overwrite = TRUE)
  
  cat("✓ Processed", iso3, "\n")
}

# Merge all the resulting layers
vect_list <- list.files(paste0(layers,"processed/flood/"), pattern = "\\.gpkg$", full.names = TRUE)

v_list <- lapply(vect_list, vect)
v_merged <- do.call(rbind, v_list)

# Keep only impact zones
v_merged <- v_merged %>% 
  filter(flood == 1)

# Project and Export
v_merged <- project(v_merged, "ESRI:54034")
writeVector(v_merged, paste0(layers,"processed/flood_lac_54034.gpkg"))
