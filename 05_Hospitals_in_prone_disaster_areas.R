# QUICK ASSESSMENT OF HEALTH FACILITIES IN PRONE DISASTER AREAS LAC REGION #
## Luis de la Rua - July 2025

# SETTINGS =================================
source("setup.R")

# Paths
layers <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/layers/"
man_path <- "C:/GIS/UNFPA GIS/HF/LAC/layers/raw/manual_hf/" # where layers downloaded manually are stored
tables <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/tables/"
plots <- "C:/GIS/UNFPA GIS/Spatial Analysis Regional/Disaster_popestimates/plots/"

# 1. HEALTH FACILITIES REGIONAL LAYER FROM PAHO DATABASE =========================================
# Metadata available here
# https://who.maps.arcgis.com/home/item.html?id=3533f8b02bfd44c2b8834313612286e9#overview
# Dataset included in this Real time natural hazards dashboard
# https://www.arcgis.com/apps/webappviewer/index.html?id=bbb6909009994e60a025549b02e8e07c

# 1.1 Import dbf table and convert into spatial ----
library(foreign)
hf_table <- read.dbf(paste0(layers,"hf_paho/emergency_hospitals_2021.dbf"))
str(hf_table)

# Cleaning of the database.
# identify duplicates in Venezuela record (PAHO_ID VE-P-69505)
hf_table <- hf_table %>% 
  filter(!(PAHO_ID == "VE-P-69505" & SurgeryRm == 'No'))
# Identify records with duplicate coordinates in Brasil
dup_coord <- hf_table %>% 
  group_by(across(all_of(c("Longitude", "Latitude")))) %>%
  filter(n() > 1) %>% 
  ungroup()

df_bra <- hf_table %>% 
  filter(CTRYISOA3 == "BRA")

df_other<- hf_table %>% 
  filter(CTRYISOA3 != 'BRA')

df_bra_unique <- df_bra %>% 
  distinct(Latitude, Longitude, .keep_all = TRUE)

hf_table <- bind_rows(df_bra_unique, df_other)

# Check categories
table(hf_table$H_Level)
table(hf_table$H_Sector)
table(hf_table$SurgeryRm)
table(hf_table$IntensiveR)

# H_level
hf_table %>% 
group_by(H_Level) %>%
  summarize(
    H_Level_count = n(),
    na_count = sum(is.na(H_Level))
  )

# H_sector
hf_table %>% 
  group_by(H_Sector) %>%
  summarize(
    H_Sector_count = n(),
    na_count = sum(is.na(H_Sector))
  )

# Surgery
hf_table %>% 
  group_by(SurgeryRm) %>%
  summarize(
    SurgeryRm_count = n(),
    na_count = sum(is.na(SurgeryRm))
  )

# Intensive R
hf_table %>% 
  group_by(IntensiveR) %>%
  summarize(
    IntensiveR_count = n(),
    na_count = sum(is.na(IntensiveR))
  )
# Correct Intensive R 
hf_table <- hf_table %>%
  mutate(IntensiveR = fct_recode(IntensiveR, "No" = "nO"))

# Intensive R
hf_table %>% 
  group_by(IntensiveR) %>%
  summarize(
    IntensiveR_count = n(),
    na_count = sum(is.na(IntensiveR))
  )

# 2. SUMMARISE BY COUNTRY AND CATEGORIES AS WELL AS CLEANING NAS ===============

# What categories we are going to include check first how data looks like
sum_hf <- hf_table %>% 
  as.data.frame() %>% 
  group_by(CTRYISOA3 ,H_Level) %>% 
  summarise(num_hf = n()) %>%
  print()

sum_hf_surgery <- hf_table %>% 
  as.data.frame() %>% 
  group_by(CTRYISOA3 ,SurgeryRm) %>% 
  summarise(num_hf = n()) %>%
  print()

# Identify countries with no hospitals or clinics to find inconsistencies in data or countries

# Identify countries without Hospitals
sum_hf %>% 
  filter(num_hf == 0) %>% 
  print()
# No Hospital countries detected in the dataset


# Find duplicates in HF ids
anyDuplicated(hf_table$GlobalID)

# Find null coordinates
hf_table %>% 
  filter(Longitude == 0)
hf_table %>% 
  filter(Latitude == 0)
hf_table %>% 
  filter(is.na(Longitude))
hf_table %>% 
  filter(is.na(Latitude))

# Find overlapped hf_table 
overlapped_coords <- hf_table %>%
  group_by(Longitude, Latitude) %>%
  summarize(count = n(), .groups = 'drop') %>%
  filter(count > 1)

# Important to document this issue in the final analysis
overlapped_hosp <- hf_table %>% 
  group_by(Longitude, Latitude ) %>%
  filter(n() > 1) %>%
  ungroup() 

overlapped_count <- table(overlapped_hosp$CTRYISOA3)

# Clean a bit the dataset to make it lighter.> Later

# 3. Filter HF by category, convert to spatial and reproject ===================
# Remove Canada and USA
hf_table <- hf_table %>% 
  filter(!(CTRYISOA3 %in% c('CAN', 'USA'))) %>% 
  droplevels()

# Convert into spatial
hf_spat <- st_as_sf(hf_table, coords = c("Longitude", "Latitude"), crs = 4326)
plot(hf_spat["SurgeryRm"])

# Reproject into World Cylindrical Equal Area
hf_spat <- hf_spat %>% 
  st_transform(crs = st_crs('ESRI:54034'))

# Rename some of the variables to make easier the analysis
hf_spat <- hf_spat %>% 
  rename(iso3 = CTRYISOA3,
         ctry_name = CTRYISON)

st_write(hf_spat,paste0(layers,"/hf_paho/lac_hf_paho_54034.gpkg"), append=F)

# 4. Intersect with Admin Boundaries and Hazard zones to get number of Health Facilities in Hazard prone areas =====
# Admin boundaries
ab <- st_read(paste0(layers,"ab/lac_ab_pol_54034.gpkg"))

# Hazard prone areas
# Impact zones
req <- st_read(paste0(layers,"processed/req_pol_54034.gpkg"))
rwind <- st_read(paste0(layers,"processed/rwind_pol_54034.gpkg"))
rflood <- st_read(paste0(layers,"processed/flood_lac_54034.gpkg"))
two_hazard <- st_read(paste0(layers, "processed/two_hazard.gpkg"))
three_hazard <- st_read(paste0(layers, "processed/three_hazard.gpkg"))

# Named list of your three impact zones
impact_zones <- list(
  req = req,
  rwind = rwind,
  rflood = rflood,
  two_hazard = two_hazard,
  three_hazard = three_hazard
)

# 4.1 By Surgery Services Availability - Loop through each hazard polygon and join  ----

# Calculate all number of health facilities by country and surgery
hf_summary_all_surgery <- hf_spat %>% 
  as.data.frame() %>% 
  group_by(iso3, SurgeryRm) %>%
  summarize(total_hf = n()
  ) %>% 
  pivot_wider(
    names_from = SurgeryRm,
    values_from = total_hf,
    values_fill = 0
  ) %>% 
  mutate(total_hf = No + Yes) %>% 
  rename(yes_surgery = Yes,
         no_surgery = No) %>% 
  relocate(total_hf, .before = no_surgery)


hf_counts_surgery <- map_dfr(names(impact_zones), function(hazard_name) {
  
  # Get the polygon
  hz <- impact_zones[[hazard_name]]
  
  # Spatial join: facilities intersecting this hazard zone
  hf_in_zone <- st_join(hf_spat, hz, left = FALSE)
  
  # Add hazard name as a column
  hf_in_zone$hazard_type <- hazard_name
  
  # Group and count
  hf_in_zone %>%
    st_drop_geometry() %>%
    group_by(iso3, hazard_type, SurgeryRm ) %>%
    summarise(n_facilities = n(), .groups = "drop")
})

# Reshape to get number of HF by hazard zone and by amentity
hf_summary_wide_surgery <- hf_counts_surgery %>%
  pivot_wider(
    names_from = c(hazard_type, SurgeryRm),  # multiple columns into name
    values_from = n_facilities,
    values_fill = 0 
  ) %>% 
  rename_with(~ paste0(.x, "_surgery"), !iso3) %>% 
  rename_with(tolower)


# Calculate total of Health Facilities too
hf_summary_surgery <- hf_summary_wide_surgery %>% 
  mutate(
    req_hf_total = rowSums(select(., starts_with("req")), na.rm = T),
    rwind_hf_total = rowSums(select(., starts_with("rwind")), na.rm = T),
    rflood_hf_total = rowSums(select(., starts_with("rflood")), na.rm = T),
    two_hazard_hf_total= rowSums(select(., starts_with("two_hazard")), na.rm = T),
    three_hazard_hf_total  = rowSums(select(., starts_with("three_hazard")), na.rm = T)
    ) %>% 
  relocate(req_hf_total, .before = req_no_surgery ) %>%
  relocate(rwind_yes_surgery, .after = rwind_no_surgery) %>% 
  relocate(rwind_hf_total, .before = rwind_no_surgery ) %>%
  relocate(rflood_hf_total, .before = rflood_no_surgery ) %>%
  relocate(two_hazard_hf_total, .before = two_hazard_yes_surgery) %>%
  relocate(three_hazard_hf_total, .before = three_hazard_no_surgery)

# Keep columns with all facilities by hazard zone for later.
# hf_summary_hazard_zones <- hf_summary_surgery %>% 
#   select(c(iso3, req_hf_total, rwind_hf_total,rflood_hf_total,
#            two_hazard_hf_total, three_hazard_hf_total))


# Merge both tables
hf_in_hzones_surgery <- merge(hf_summary_all_surgery , hf_summary_surgery, by = "iso3")

# Calculate the percentages
hf_in_hzones_surgery <- hf_in_hzones_surgery %>%
  mutate(
    pct_req_yes_surgery = (req_yes_surgery  / req_hf_total ) * 100,
    pct_req_no_surgery = (req_no_surgery  / req_hf_total ) * 100,
    pct_req_total_hf = (req_hf_total  / total_hf)*100,
    pct_rwind_yes_surgery = (rwind_yes_surgery / rwind_hf_total ) * 100,
    pct_rwind_no_surgery  = (rwind_no_surgery / rwind_hf_total ) * 100,
    pct_rwind_total_hf = (rwind_hf_total / total_hf) * 100,
    pct_rflood_yes_surgery = (rflood_yes_surgery/ rflood_hf_total ) * 100,
    pct_rflood_no_surgery  = (rflood_no_surgery/ rflood_hf_total ) * 100,
    pct_rflood_total_hf = (rflood_hf_total / total_hf) * 100,
    pct_two_hazard_yes_surgery= (two_hazard_yes_surgery / two_hazard_hf_total) * 100,
    pct_two_hazard_no_surgery  = (two_hazard_no_surgery / two_hazard_hf_total) * 100,
    pct_two_hazard_total_hf = (two_hazard_hf_total/ total_hf) * 100,
    pct_three_hazard_yes_surgery = (three_hazard_yes_surgery / three_hazard_hf_total) * 100,
    pct_three_hazard_no_surgery  = (three_hazard_no_surgery / three_hazard_hf_total) * 100,
    pct_three_hazard_total_hf = (three_hazard_hf_total / total_hf) * 100,
  ) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

# # Export into an excel table by_surgery tab
# 
# write.xlsx(
#   x = hf_in_hzones_surgery,
#   file = paste0(tables,"lac_hf_in_hzones.xlsx"),
#   sheetName = "by_surgery"
# )

# 4.2 Intensive Services Availability in hazard prone areas ----
# Calculate all number of health facilities by country and Intensive 
hf_summary_all_intensive <- hf_spat %>% 
  as.data.frame() %>% 
  group_by(iso3, IntensiveR) %>%
  summarize(total_hf = n()
  ) %>% 
  pivot_wider(
    names_from = IntensiveR,
    values_from = total_hf,
    values_fill = 0
  ) %>% 
  mutate(total_hf = No + Yes) %>% 
  rename(yes_intensive = Yes,
         no_intensive = No) %>% 
  relocate(total_hf, .before = no_intensive)


hf_counts_intensive <- map_dfr(names(impact_zones), function(hazard_name) {
  
  # Get the polygon
  hz <- impact_zones[[hazard_name]]
  
  # Spatial join: facilities intersecting this hazard zone
  hf_in_zone <- st_join(hf_spat, hz, left = FALSE)
  
  # Add hazard name as a column
  hf_in_zone$hazard_type <- hazard_name
  
  # Group and count
  hf_in_zone %>%
    st_drop_geometry() %>%
    group_by(iso3, hazard_type, IntensiveR ) %>%
    summarise(n_facilities = n(), .groups = "drop")
})

# Reshape to get number of HF by hazard zone and by amentity
hf_summary_wide_intensive <- hf_counts_intensive %>%
  pivot_wider(
    names_from = c(hazard_type, IntensiveR),  # multiple columns into name
    values_from = n_facilities,
    values_fill = 0 
  ) %>% 
  rename_with(~ paste0(.x, "_intensive"), !iso3) %>% 
  rename_with(tolower)


# Calculate total of Health Facilities too
hf_summary_intensive  <- hf_summary_wide_intensive  %>% 
  mutate(
    req_hf_total = rowSums(select(., starts_with("req")), na.rm = T),
    rwind_hf_total = rowSums(select(., starts_with("rwind")), na.rm = T),
    rflood_hf_total = rowSums(select(., starts_with("rflood")), na.rm = T),
    two_hazard_hf_total= rowSums(select(., starts_with("two_hazard")), na.rm = T),
    three_hazard_hf_total  = rowSums(select(., starts_with("three_hazard")), na.rm = T)
  ) %>% 
  relocate(req_hf_total, .before = req_no_intensive ) %>%
  relocate(rwind_yes_intensive, .after = rwind_no_intensive) %>% 
  relocate(rwind_hf_total, .before = rwind_no_intensive ) %>%
  relocate(rflood_hf_total, .before = rflood_no_intensive ) %>%
  relocate(two_hazard_hf_total, .before = two_hazard_yes_intensive) %>%
  relocate(three_hazard_hf_total, .before = three_hazard_no_intensive)


# Merge both tables
hf_in_hzones_intensive <- merge(hf_summary_all_intensive , hf_summary_intensive, by = "iso3")

# Calculate the percentages
hf_in_hzones_intensive <- hf_in_hzones_intensive %>%
  mutate(
    pct_req_yes_intensive = (req_yes_intensive  / req_hf_total ) * 100,
    pct_req_no_intensive = (req_no_intensive  / req_hf_total ) * 100,
    pct_req_total_hf = (req_hf_total  / total_hf)*100,
    pct_rwind_yes_intensive = (rwind_yes_intensive / rwind_hf_total ) * 100,
    pct_rwind_no_intensive  = (rwind_no_intensive / rwind_hf_total ) * 100,
    pct_rwind_total_hf = (rwind_hf_total / total_hf) * 100,
    pct_rflood_yes_intensive = (rflood_yes_intensive/ rflood_hf_total ) * 100,
    pct_rflood_no_intensive  = (rflood_no_intensive/ rflood_hf_total ) * 100,
    pct_rflood_total_hf = (rflood_hf_total / total_hf) * 100,
    pct_two_hazard_yes_intensive= (two_hazard_yes_intensive / two_hazard_hf_total) * 100,
    pct_two_hazard_no_intensive  = (two_hazard_no_intensive / two_hazard_hf_total) * 100,
    pct_two_hazard_total_hf = (two_hazard_hf_total/ total_hf) * 100,
    pct_three_hazard_yes_intensive = (three_hazard_yes_intensive / three_hazard_hf_total) * 100,
    pct_three_hazard_no_intensive  = (three_hazard_no_intensive / three_hazard_hf_total) * 100,
    pct_three_hazard_total_hf = (three_hazard_hf_total / total_hf) * 100,
  ) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

# Export into an excel table by_surgery tab
# wb <- loadWorkbook(paste0(tables,"lac_hf_in_hzones.xlsx"))
# addWorksheet(wb, "by_intensive")
# writeData(wb, sheet = "by_intensive", x = hf_in_hzones_intensive)
# saveWorkbook(wb, paste0(tables,"lac_hf_in_hzones.xlsx"), overwrite = T)

# 4.3 Emergency Services Availability in hazard prone areas ----
print(table(hf_spat$EmergenRm))
# All hospitals evaluated have emergency services so no point to analyse this




# 4.4 Health facilities by Hospital Level in hazard prone areas ----
hf_summary_all_hlevel <- hf_spat %>% 
  as.data.frame() %>% 
  group_by(iso3, H_Level) %>%
  summarize(total_hf = n()
  ) %>% 
  pivot_wider(
    names_from = H_Level,
    values_from = total_hf,
    values_fill = 0
  ) %>% 
  rename(big = `BIG - High medical specialties`,
         medium = `MEDIUM -Two Specialty services`,
         small = `SMALL - General surgery and open 24 hrs`) %>% 
  mutate(total_hf = big + medium + small) %>% 
  relocate(total_hf, .before = big)


hf_counts_hlevel <- map_dfr(names(impact_zones), function(hazard_name) {
  
  # Get the polygon
  hz <- impact_zones[[hazard_name]]
  
  # Spatial join: facilities intersecting this hazard zone
  hf_in_zone <- st_join(hf_spat, hz, left = FALSE)
  
  # Add hazard name as a column
  hf_in_zone$hazard_type <- hazard_name
  
  # Group and count
  hf_in_zone %>%
    st_drop_geometry() %>%
    group_by(iso3, hazard_type, H_Level ) %>%
    summarise(n_facilities = n(), .groups = "drop")
})

# Recode categories' names
hf_counts_hlevel <- hf_counts_hlevel %>% 
  mutate(H_Level = recode(H_Level,
                          'BIG - High medical specialties' = "big",
                          'MEDIUM -Two Specialty services' = "medium",
                          'SMALL - General surgery and open 24 hrs' = "small"))

# Reshape to get number of HF by hazard zone and by amentity
hf_summary_wide_hlevel <- hf_counts_hlevel %>%
  pivot_wider(
    names_from = c(hazard_type, H_Level),  # multiple columns into name
    values_from = n_facilities,
    values_fill = 0 
  ) %>% 
  rename_with(~ paste0(.x, "_hlevel"), !iso3) %>% 
  rename_with(tolower)


# Calculate total of Health Facilities too
hf_summary_hlevel  <- hf_summary_wide_hlevel  %>% 
  mutate(three_hazard_big_hlevel = 0,
         three_hazard_small_hlevel = 0) %>% 
  mutate(
    req_hf_total = rowSums(select(., starts_with("req")), na.rm = T),
    rwind_hf_total = rowSums(select(., starts_with("rwind")), na.rm = T),
    rflood_hf_total = rowSums(select(., starts_with("rflood")), na.rm = T),
    two_hazard_hf_total= rowSums(select(., starts_with("two_hazard")), na.rm = T),
    three_hazard_hf_total  = rowSums(select(., starts_with("three_hazard")), na.rm = T)
  ) %>% 
  relocate(req_hf_total, .before = req_big_hlevel) %>%
  relocate(rwind_hf_total, .before = rwind_big_hlevel) %>%
  relocate(rflood_hf_total, .before = rflood_big_hlevel) %>%
  relocate(two_hazard_hf_total, .before = two_hazard_big_hlevel) %>%
  relocate(three_hazard_hf_total, .before = three_hazard_big_hlevel) %>%
  relocate(rwind_small_hlevel, .after = rwind_medium_hlevel)


# Merge both tables
hf_in_hzones_hlevel<- merge(hf_summary_all_hlevel , hf_summary_hlevel, by = "iso3")

# Calculate the percentages
hf_in_hzones_hlevel <- hf_in_hzones_hlevel %>%
  mutate(
    pct_req_big_hlevel = (req_big_hlevel / req_hf_total ) * 100,
    pct_req_medium_hlevel = (req_medium_hlevel / req_hf_total ) * 100,
    pct_req_small_hlevel  = (req_small_hlevel / req_hf_total ) * 100,
    pct_req_total_hf = (req_hf_total / total_hf) * 100,
    pct_rwind_big_hlevel = (rwind_big_hlevel / rwind_hf_total ) * 100,
    pct_rwind_medium_hlevel = (rwind_medium_hlevel / rwind_hf_total ) * 100,
    pct_rwind_small_hlevel  = (rwind_small_hlevel / rwind_hf_total ) * 100,
    pct_rwind_total_hf = (rwind_hf_total / total_hf) * 100,
    pct_rflood_big_hlevel = (rflood_big_hlevel / rflood_hf_total ) * 100,
    pct_rflood_medium_hlevel = (rflood_medium_hlevel / rflood_hf_total ) * 100,
    pct_rflood_small_hlevel  = (rflood_small_hlevel / rflood_hf_total ) * 100,
    pct_rflood_total_hf = (rflood_hf_total / total_hf) * 100,
    pct_two_hazard_big_hlevel= (two_hazard_big_hlevel / two_hazard_hf_total) * 100,
    pct_two_hazard_medium_hlevel= (two_hazard_medium_hlevel / two_hazard_hf_total) * 100,
    pct_two_hazard_small_hlevel  = (two_hazard_small_hlevel / two_hazard_hf_total) * 100,
    pct_two_hazard_total_hf = (two_hazard_hf_total/ total_hf) * 100,
    pct_three_hazard_big_hlevel = (three_hazard_big_hlevel / three_hazard_hf_total) * 100,
    pct_three_hazard_medium_hlevel= (three_hazard_medium_hlevel / three_hazard_hf_total) * 100,
    pct_three_hazard_small_hlevel  = (three_hazard_small_hlevel / three_hazard_hf_total) * 100,
    pct_three_hazard_total_hf = (three_hazard_hf_total / total_hf) * 100,
    ) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

names(hf_in_hzones_hlevel)

# 4.5 Export the three tables into into an excel table by_surgery tab ----

# Rename variables so they make more sense in the tables and plots
names(hf_in_hzones_surgery)
ab_cnames <- ab %>% 
  as.data.frame() %>% 
  select(c(GID_0, NAME_0)) %>% 
  rename(c_name = NAME_0,
         iso3 = GID_0)
# merge the 3 datasets with ab_cnames to get country names

hf_in_hzones_surgery <- hf_in_hzones_surgery %>% 
  merge(.,ab_cnames, by = "iso3") %>% 
  relocate(c_name, .after = iso3)

hf_in_hzones_intensive <- hf_in_hzones_intensive %>% 
  merge(.,ab_cnames, by = "iso3") %>% 
  relocate(c_name, .after = iso3)

hf_in_hzones_hlevel <- hf_in_hzones_hlevel %>% 
  merge(.,ab_cnames, by = "iso3") %>% 
  relocate(c_name, .after = iso3)

# Export into a clean version of excel spreadsheet
# Cleaning variable names
names(hf_in_hzones_surgery)

new_headers_surgery <- c(
  "ISO Code",
  "Country",
  "Total Facilities",
  "Non-Surgical Facilities",
  "Surgical Facilities",
  "Total Facilities (Earthquake Risk)",
  "Non-Surgical (Earthquake Risk)",
  "Surgical (Earthquake Risk)",
  "Total Facilities (Hurricane Wind)",
  "Non-Surgical (Hurricane Wind)",
  "Surgical (Hurricane Wind)",
  "Total Facilities (Riverine Flood)",
  "Non-Surgical (Riverine Flood)",
  "Surgical (Riverine Flood)",
  "Total Facilities (2 Hazards)",
  "Surgical (2 Hazards)",
  "Non-Surgical (2 Hazards)",
  "Total Facilities (3 Hazards)",
  "Non-Surgical (3 Hazards)",
  "Surgical (3 Hazards)",
  "% Surgical (Earthquake Risk)",
  "% Non-Surgical (Earthquake Risk)",
  "% Total Facilities (Earthquake Risk)",
  "% Surgical (Hurricane Wind)",
  "% Non-Surgical (Hurricane Wind)",
  "% Total Facilities (Hurricane Wind)",
  "% Surgical (Riverine Flood)",
  "% Non-Surgical (Riverine Flood)",
  "% Total Facilities (Riverine Flood)",
  "% Surgical (2 Hazards)",
  "% Non-Surgical (2 Hazards)",
  "% Total Facilities (2 Hazards)",
  "% Surgical (3 Hazards)",
  "% Non-Surgical (3 Hazards)",
  "% Total Facilities (3 Hazards)"
)

hf_in_hzones_surgery_table <- hf_in_hzones_surgery %>% 
  set_names(new_headers_surgery)

# For intensive 
names(hf_in_hzones_intensive)

new_headers_intensive <- c(
  "ISO Code",
  "Country",
  "Total Facilities",
  "Facilities without ICU",
  "Facilities with ICU",
  "Total Facilities (Earthquake Risk)",
  "Without ICU (Earthquake Risk)",
  "With ICU (Earthquake Risk)",
  "Total Facilities (Hurricane Wind)",
  "Without ICU (Hurricane Wind)",
  "With ICU (Hurricane Wind)",
  "Total Facilities (Riverine Flood)",
  "Without ICU (Riverine Flood)",
  "With ICU (Riverine Flood)",
  "Without ICU (2 Hazards)",
  "Total Facilities (2 Hazards)",
  "With ICU (2 Hazards)",
  "Total Facilities (3 Hazards)",
  "Without ICU (3 Hazards)",
  "With ICU (3 Hazards)",
  "% With ICU (Earthquake Risk)",
  "% Without ICU (Earthquake Risk)",
  "% Total Facilities (Earthquake Risk)",
  "% With ICU (Hurricane Wind)",
  "% Without ICU (Hurricane Wind)",
  "% Total Facilities (Hurricane Wind)",
  "% With ICU (Riverine Flood)",
  "% Without ICU (Riverine Flood)",
  "% Total Facilities (Riverine Flood)",
  "% With ICU (2 Hazards)",
  "% Without ICU (2 Hazards)",
  "% Total Facilities (2 Hazards)",
  "% With ICU (3 Hazards)",
  "% Without ICU (3 Hazards)",
  "% Total Facilities (3 Hazards)"
)

hf_in_hzones_intensive_table <- hf_in_hzones_intensive %>% 
  set_names(new_headers_intensive )

# For Hospital level
names(hf_in_hzones_hlevel)

new_headers_hlevel <- c(
  "ISO Code",
  "Country",
  "Total Facilities",
  "Large Hospitals",
  "Medium Hospitals",
  "Small Hospitals",
  "Total Facilities (Earthquake Risk)",
  "Large (Earthquake Risk)",
  "Medium (Earthquake Risk)",
  "Small (Earthquake Risk)",
  "Total Facilities (Hurricane Wind)",
  "Large (Hurricane Wind)",
  "Medium (Hurricane Wind)",
  "Small (Hurricane Wind)",
  "Total Facilities (Riverine Flood)",
  "Large (Riverine Flood)",
  "Medium (Riverine Flood)",
  "Small (Riverine Flood)",
  "Total Facilities (2 Hazards)",
  "Large (2 Hazards)",
  "Medium (2 Hazards)",
  "Small (2 Hazards)",
  "Medium (3 Hazards)",
  "Total Facilities (3 Hazards)",
  "Large (3 Hazards)",
  "Small (3 Hazards)",
  "% Large (Earthquake Risk)",
  "% Medium (Earthquake Risk)",
  "% Small (Earthquake Risk)",
  "% Total Facilities (Earthquake Risk)",
  "% Large (Hurricane Wind)",
  "% Medium (Hurricane Wind)",
  "% Small (Hurricane Wind)",
  "% Total Facilities (Hurricane Wind)",
  "% Large (Riverine Flood)",
  "% Medium (Riverine Flood)",
  "% Small (Riverine Flood)",
  "% Total Facilities (Riverine Flood)",
  "% Large (2 Hazards)",
  "% Medium (2 Hazards)",
  "% Small (2 Hazards)",
  "% Total Facilities (2 Hazards)",
  "% Large (3 Hazards)",
  "% Medium (3 Hazards)",
  "% Small (3 Hazards)",
  "% Total Facilities (3 Hazards)"
)

hf_in_hzones_hlevel_table <- hf_in_hzones_hlevel %>% 
  set_names(new_headers_hlevel)

# Separate tables by subregion
# Separate country codes for Latin America Discuss classification this is from UNSD
#https://unstats.un.org/unsd/methodology/m49/

car_count_list <-  c(
  "AIA", "ATG", "ABW", "BHS", "BMU", "BRB", "BLZ", "BES", "VGB", "CYM", "CUW",
  "DMA", "GRD", "GLP", "HTI", "JAM", "MTQ", "MSR", "PRI", "BLM", "KNA",
  "LCA", "MAF", "VCT", "SXM", "SUR", "TTO", "TCA", "VIR", "GUF", "GUY"
)

hf_in_hzones_surgery_la_table <- hf_in_hzones_surgery_table %>% 
  filter(!(`ISO Code` %in% car_count_list))
hf_in_hzones_surgery_car_table <- hf_in_hzones_surgery_table %>% 
  filter(`ISO Code` %in% car_count_list)

hf_in_hzones_intensive_la_table <- hf_in_hzones_intensive_table %>% 
  filter(!(`ISO Code` %in% car_count_list))
hf_in_hzones_intensive_car_table <- hf_in_hzones_intensive_table %>% 
  filter(`ISO Code` %in% car_count_list)

hf_in_hzones_hlevel_la_table <- hf_in_hzones_hlevel_table %>% 
  filter(!(`ISO Code` %in% car_count_list))
hf_in_hzones_hlevel_car_table <- hf_in_hzones_hlevel_table %>% 
  filter(`ISO Code` %in% car_count_list)

# Export into Excel spreadsheet

wb <- createWorkbook()
# Your vector of sheet names
sheet_names <- c("by_surgery", "by_surgery_la","by_surgery_car", 
                 "by_intensive", "by_intensive_la", "by_intensive_car",
                 "by_hosp_level", "by_hosp_level_la", "by_hosp_level_car"
                 )

# Loop through the vector and add one sheet at a time
for(sheet in sheet_names){
  addWorksheet(wb, sheet)
}
writeData(wb, 
          sheet = "by_surgery", 
          x = hf_in_hzones_surgery_table)

writeData(wb, 
          sheet = "by_surgery_la", 
          x = hf_in_hzones_surgery_la_table)

writeData(wb, 
          sheet = "by_surgery_car", 
          x = hf_in_hzones_surgery_car_table)


writeData(wb, 
          sheet = "by_intensive", 
          x = hf_in_hzones_intensive_table)

writeData(wb, 
          sheet = "by_intensive_la", 
          x = hf_in_hzones_intensive_la_table)
writeData(wb, 
          sheet = "by_intensive_car", 
          x = hf_in_hzones_intensive_car_table)


writeData(wb, 
          sheet = "by_hosp_level", 
          x = hf_in_hzones_hlevel_table)

writeData(wb, 
          sheet = "by_hosp_level_la", 
          x = hf_in_hzones_hlevel_la_table)

writeData(wb, 
          sheet = "by_hosp_level_car", 
          x = hf_in_hzones_hlevel_car_table)

# Save workbook
saveWorkbook(wb, paste0(tables,"lac_hf_paho_in_hzones.xlsx"), overwrite = T)


# 
# 
# # 6. Plot some graphs to better explain the trends
# # Prepare tables too
# country_codes <- data.frame(
#   ISO3 = c("ABW", "AIA", "ARG", "ATG", "BHS", "BLZ", "BMU", "BOL", "BRA", "BRB",
#            "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GLP",
#            "GRD", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MEX",
#            "MSR", "MTQ", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "TCA",
#            "TTO", "URY", "VCT", "VEN", "VGB"),
#   Country = c("Aruba", "Anguilla", "Argentina", "Antigua and Barbuda", "Bahamas",
#               "Belize", "Bermuda", "Bolivia", "Brazil", "Barbados", "Chile", "Colombia",
#               "Costa Rica", "Cuba", "Curaçao", "Cayman Islands", "Dominica", "Dominican Republic",
#               "Ecuador", "Guadeloupe", "Grenada", "Guatemala", "French Guiana", "Guyana",
#               "Honduras", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia",
#               "Mexico", "Montserrat", "Martinique", "Nicaragua", "Panama", "Peru", "Puerto Rico",
#               "Paraguay", "El Salvador", "Suriname", "Turks and Caicos Islands", "Trinidad and Tobago",
#               "Uruguay", "Saint Vincent and the Grenadines", "Venezuela", "British Virgin Islands")
# )
# 
# # Separate country codes for Latin America Discuss classification this is from UNSD
# #https://unstats.un.org/unsd/methodology/m49/
# 
# car_count_list <-  c(
#   "AIA", "ATG", "ABW", "BHS", "BMU", "BRB", "BLZ", "BES", "VGB", "CYM", "CUW",
#   "DMA", "GRD", "GLP", "HTI", "JAM", "MTQ", "MSR", "PRI", "BLM", "KNA",
#   "LCA", "MAF", "VCT", "SXM", "SUR", "TTO", "TCA", "VIR", "GUF", "GUY"
# )
# 
# hf_long_pct <- hf_in_hzones %>%
#   select(iso3, starts_with("pct_")) %>%
#   select(iso3, ends_with("total_hf")) %>% 
#   pivot_longer(
#     cols = starts_with("pct_"),
#     names_to = "hazard_type",
#     values_to = "percentage"
#   ) %>%
#   mutate(hazard_type = gsub("pct_hf_", "", hazard_type)) %>%   # clean names
#   mutate(iso3 = toupper(iso3))
# 
# hf_long_pct_lat <- hf_long_pct %>% 
#   filter(!(iso3 %in% car_count_list))
# 
# hf_long_pct_car <- hf_long_pct %>% 
#   filter(iso3 %in% car_count_list)
# 
# plot_hf_per_lat <- ggplot(hf_long_pct_lat, aes(x = iso3, y = percentage, fill = hazard_type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(
#     title = "Percentage of Health Facilities by Hazard Prone Zone - Latin America Region (%)",
#     x = "Country (ISO3)",
#     y = "Percentage",
#     fill = "Hazard Type"
#   ) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   scale_fill_manual(
#     values = c(
#       pct_rec_total_hf = "#bf9000",
#       pct_rwind_total_hf  = "#34a853",
#       pct_rflood_total_hf = "#4a86e8",
#       pct_two_hazard_total_hf = "#9900ff",
#       pct_three_hazard_total_hf  = "#ff6d01"
#     ),
#     labels = c(
#       pct_rec_total_hf = "Health Facilities - Earthquake zone",
#       pct_rwind_total_hf = "Health Facilities - Hurricane winds zone",
#       pct_rflood_total_hf = "Health Facilities - Riverine floods zone",
#       pct_two_hazard_total_hf = "Health Facilities - Two Hazards zone",
#       pct_three_hazard_total_hf = "Health Facilities - Three Hazards zone"
#     )
#   )
# plot_hf_per_lat
# 
# ggsave(paste0(plots,"plot_hf_per_lat.png"), plot = plot_hf_per_lat, 
#        width = 10, height = 6, dpi = 300, bg = "white")
# 
# plot_hf_per_car <- ggplot(hf_long_pct_car, aes(x = iso3, y = percentage, fill = hazard_type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(
#     title = "Percentage of Health Facilities by Hazard Prone Zone - Caribbean Region (%)",
#     x = "Country (ISO3)",
#     y = "Percentage",
#     fill = "Hazard Type"
#   ) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   scale_fill_manual(
#     values = c(
#       pct_rec_total_hf = "#bf9000",
#       pct_rwind_total_hf  = "#34a853",
#       pct_rflood_total_hf = "#4a86e8",
#       pct_two_hazard_total_hf = "#9900ff",
#       pct_three_hazard_total_hf  = "#ff6d01"
#     ),
#     labels = c(
#       pct_rec_total_hf = "Health Facilities - Earthquake zone",
#       pct_rwind_total_hf = "Health Facilities - Hurricane winds zone",
#       pct_rflood_total_hf = "Health Facilities - Riverine floods zone",
#       pct_two_hazard_total_hf = "Health Facilities - Two Hazards zone",
#       pct_three_hazard_total_hf = "Health Facilities - Three Hazards zone"
#     )
#   )
# plot_hf_per_car
# 
# ggsave(paste0(plots,"plot_hf_per_car.png"), plot = plot_hf_per_car, 
#        width = 10, height = 6, dpi = 300, bg = "white")
# 
# 
# ### Include french Guyana as is it 0% for all the results
