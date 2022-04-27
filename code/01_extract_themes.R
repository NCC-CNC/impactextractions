library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Read-in NCC achievement boundaries -------------------------------------------

PMP <- read_sf(file.path("appdata", "achievements", "NCC_Accomplishments_April_2022.shp")) %>%
  filter(!is.na(NAME)) %>%
  # Set to rater projection (not sure about this coordinate system...)
  st_transform(crs = st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "))

# Read-in feature themes: ------------------------------------------------------

# Forest
frst <- rast(file.path("appdata", "themes", "albers",
                       "CA_forest_VLCE_2015_forest_only_ha_proj_scale.tif"))

# Grassland
gras <- rast(file.path("appdata", "themes", "albers",
                        "AAFC_LU2015_comb_masked_by_Prairie_grassland_comb.tif"))

# Wetlands
wetl <- rast(file.path("appdata", "themes", "albers",
                       "Wetland_comb_proj_diss_90m_Arc.tif"))

# Rivers
rivr <- rast(file.path("appdata", "themes", "albers",
                       "grid_1km_water_linear_flow_length_1km.tif"))

# Lakes
laks <- rast(file.path("appdata", "themes", "albers",
                       "Lakes_CanVec_50k_ha.tif"))

# Shoreline
shrl <- rast(file.path("appdata", "themes", "albers",
                       "Shoreline.tif"))

# Climate
cfor <- rast(file.path("appdata", "themes", "albers",
                       "fwdshortestpath.tif"))

cref <- rast(file.path("appdata", "themes", "albers",
                       "NA_combo_refugia_sum45.tif"))

# Carbon
csta <- rast(file.path("appdata", "themes", "albers",
                       "Carbon_Mitchell_2021_t.tif")) %>%
  terra::crop(cfor)

cseq <- rast(file.path("appdata", "themes", "albers",
                       "Carbon_Potential_NFI_2011_CO2e_t_year.tif")) %>%
  terra::crop(cfor)

# Freshwater
fwat <- rast(file.path("appdata", "themes", "albers", "water_provision_2a_norm.tif"))

# Recreation
recr <- rast(file.path("appdata", "themes", "albers", "rec_pro_1a_norm.tif"))

# Stack feature rasters --------------------------------------------------------

feat_stack <- c(frst, gras, wetl, rivr, laks, shrl, cfor, cref, csta, cseq, fwat, recr)
feat_stack <- terra::setMinMax(feat_stack)

names(feat_stack) <- c("Forest", "Grassland", "Wetland", "River", "Lakes",
                       "Shoreline", "Climate_velocity", "Climate_refugia",
                       "Carbon_current", "Carbon_potential","Freshwater",
                       "Recreation")

# Read-in species themes: ------------------------------------------------------

# Species at risk - ECCC
SAR <- rast(file.path("appdata", "themes", "albers", "SAR_sum.tif"))

# Amphibian - IUCN
amph <- rast(file.path("appdata", "themes", "albers", "amph_sum.tif"))

# Bird - IUCN
bird <- rast(file.path("appdata", "themes", "albers", "bird_sum.tif"))

# Mammal - IUCN
mamm <- rast(file.path("appdata", "themes", "albers", "bird_sum.tif"))

# Reptile - ICUN
rept <- rast(file.path("appdata", "themes", "albers", "rept_sum.tif"))

# Species at risk - NatureServe Canada
SAR_NSC <- rast(file.path("appdata", "themes", "albers", "NSC_SARsum.tif"))

# Endemic species - NatureServe Canada
END_NSC <- rast(file.path("appdata", "themes", "albers", "NSC_ENDsum.tif"))

# Several species - NatureServe Canada
SPP_NSC <- rast(file.path("appdata", "themes", "albers", "NSC_SPPsum.tif"))

# Stack species rasters --------------------------------------------------------

spp_stack <- c(SAR, amph, bird, mamm, rept, SAR_NSC, END_NSC, SPP_NSC)
spp_stack <- terra::setMinMax(spp_stack)
names(spp_stack) <- c("Species_at_Risk_ECCC", "Amphibians_IUCN", "Birds_IUCN",
                      "Mammals_IUCN","Reptiles_IUCN", "Species_at_Risk_NSC",
                      "Endemics_NSC", "Biodiversity_NSC")

# Extract raster variables to PMP ----------------------------------------------

# Environmental variables
PMP_feat_stack_mean <- exact_extract(feat_stack, PMP, fun = "sum", force_df = TRUE) %>%
  round(1)
names(PMP_feat_stack_mean) <- gsub("sum.", "", names(PMP_feat_stack_mean))
PMP_feat_stack_mean <- PMP_feat_stack_mean %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

# Species variables
PMP_spp_stack_mean <- exact_extract(spp_stack, PMP, fun = "max", force_df = TRUE) %>%
  round(1)
names(PMP_spp_stack_mean) <- gsub("max.", "", names(PMP_spp_stack_mean))
PMP_spp_stack_mean <- PMP_spp_stack_mean %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

# Combine all extractions into one sf object -----------------------------------
PMP_tmp <- cbind(PMP, PMP_feat_stack_mean, PMP_spp_stack_mean)

# Clean geometry, populate id 
PMP_tmp <- PMP_tmp %>% 
  st_make_valid() %>%
  mutate("id" = row_number())

# Remove 'Region' and 'Territory' from REGION field
PMP_tmp <- PMP_tmp %>% 
  mutate(REGION = str_remove_all(REGION, " Region")) %>%
  mutate(REGION = str_remove_all(REGION, " Territory")) 
  
# Calculate area ha 
PMP_tmp$Area_ha <- units::drop_units(units::set_units(st_area(PMP_tmp), value = ha))

# Project to WGS
PMP_tmp <- st_transform(PMP_tmp, crs = st_crs(4326))

# Save data for shiny app ------------------------------------------------------
save(PMP_tmp, PMP_feat_stack_mean, PMP_spp_stack_mean,
     file = file.path("appdata", "basedata.RData"))
