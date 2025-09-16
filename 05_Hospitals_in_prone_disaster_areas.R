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

# Convert to spatial and reproject into World Cylindrical Equal Area
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

# 4.1 By Surgery - Loop through each hazard polygon and join  ----
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
    group_by(iso3, hazard_type, SurgeryRm) %>%
    summarise(n_facilities = n(), .groups = "drop")
})

# Reshape to get number of HF by hazard zone and by amentity
hf_summary_wide_surgery <- hf_counts_surgery %>%
  pivot_wider(
    names_from = c(hazard_type, SurgeryRm),  # multiple columns into name
    values_from = n_facilities,
    values_fill = 0 
  ) %>% 
  rename_with(~ paste0(.x, "_surgery"), !iso3)

# Calculate total of Health Facilities too
hf_summary_surgery <- hf_summary_wide_surgery %>% 
  mutate(
    total_hf_req = rowSums(select(., starts_with("req")), na.rm = T),
    total_hf_rwind = rowSums(select(., starts_with("rwind")), na.rm = T),
    total_hf_rflood = rowSums(select(., starts_with("rflood")), na.rm = T),
    total_hf_two_hazard = rowSums(select(., starts_with("two_hazard")), na.rm = T),
    total_hf_three_hazard = rowSums(select(., starts_with("three_hazard")), na.rm = T)
    ) %>% 
  relocate(total_hf_req, .before = req_No_surgery ) %>%
  relocate(rwind_Yes_surgery, .after = rwind_No_surgery) %>% 
  relocate(total_hf_rwind, .before = rwind_No_surgery ) %>%
  relocate(total_hf_rflood, .before = rflood_No_surgery ) %>%
  relocate(total_hf_two_hazard, .before = two_hazard_Yes_surgery) %>%
  relocate(total_hf_three_hazard, .before = three_hazard_No_surgery)

# Calculate all number of healthfacilities by country and amenity
hf_summary_all <- hf_spat %>% 
  as.data.frame() %>% 
  group_by(iso3, SurgeryRm) %>%
  summarize(n_hf = n()
            ) %>% 
  pivot_wider(
    names_from = SurgeryRm,
    values_from = n_hf,
    values_fill = 0
  ) %>% 
  mutate(total_hf  = Yes + No)

# Merge both tables
hf_in_hzones <- merge(hf_summary_all , hf_summary_surgery, by = "iso3")

# -------- hasta aqui

# Calculate the percentages
hf_in_hzones <- hf_in_hzones %>%
  mutate(
    pct_req_Yes_surgery = (req_Yes_surgery  / clinic) * 100,
    pct_req_No_surgery = (req_pol_hospital / hospital) * 100,
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
