library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(raster)

# Set file names ----
INPUT_SHP_NAME <- "GIS_NCC_ACCOMPLISHMENTS_20221122.shp"
OUTPUT_SHP_PATH <- "C:/Data/National/NCC_Accomplishments/Extractions/NCC_ACCOMPLISHMENTS_EXTRACTED_20221122.shp"

# Read-in NCC achievements ----
PMP <- read_sf(file.path("appdata", "achievements", INPUT_SHP_NAME)) %>%
  filter(!is.na(NAME)) %>%
  # Project to WGS_1984_Canada_Albers (same as: Albers_Conic_Equal_Area)
  st_transform(crs = st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

# Read-in conservation features ----
## Forest
frst <- rast(file.path("appdata", "themes", "albers",
                       "CA_forest_VLCE_2015_forest_only_ha_proj_scale.tif"))
## Grassland
gras <- rast(file.path("appdata", "themes", "albers",
                        "AAFC_LU2015_comb_masked_by_Prairie_grassland_comb.tif"))
## Wetlands
wetl <- rast(file.path("appdata", "themes", "albers",
                       "Wetland_comb_proj_diss_90m_Arc.tif"))
## Rivers
rivr <- rast(file.path("appdata", "themes", "albers",
                       "grid_1km_water_linear_flow_length_1km.tif"))
## Lakes
laks <- rast(file.path("appdata", "themes", "albers",
                       "Lakes_CanVec_50k_ha.tif"))
## Shoreline
shrl <- rast(file.path("appdata", "themes", "albers",
                       "Shoreline.tif"))
## Climate Velocity
cfor <- rast(file.path("appdata", "themes", "albers",
                       "fwdshortestpath.tif"))
## Climate Refugia
cref <- rast(file.path("appdata", "themes", "albers",
                       "NA_combo_refugia_sum45.tif"))
# Carbon Current
csta <- rast(file.path("appdata", "themes", "albers",
                       "Carbon_Mitchell_2021_t.tif")) 
# Carbon Potential
cseq <- rast(file.path("appdata", "themes", "albers",
                       "Carbon_Potential_NFI_2011_CO2e_t_year.tif")) 
# Freshwater
fwat <- rast(file.path("appdata", "themes", "albers", 
                       "water_provision_2a_norm.tif"))
# Recreation
recr <- rast(file.path("appdata", "themes", "albers", 
                       "rec_pro_1a_norm.tif"))
# Pollination
epf <- rast(file.path("appdata", "themes", "albers", 
                      "epf_natural_habitat_sum.tif"))

marketv <- rast(file.path("appdata", "themes", "albers", 
                          "marketvalue_natural_habitat_sum.tif")) 

# Stack conservation feature rasters ----

feat_stack <- c(frst, gras, wetl, rivr, laks, shrl, cfor, 
                cref, csta, cseq, fwat, recr, epf, marketv)

feat_stack <- terra::setMinMax(feat_stack)

names(feat_stack) <- c("Forest", "Grassland", 
                       "Wetland", "River", 
                       "Lakes","Shore", 
                       "Climate_V", "Climate_R",
                       "Carbon_C", "Carbon_P",
                       "Freshwater","Rec", 
                       "EPF", "Market_V")

# Read-in species ----
## Species at risk - ECCC
ECCC_SAR <- rast(file.path("appdata", "themes", "albers", "SAR_sum.tif"))

## Amphibian - IUCN
IUCN_AMPH <- rast(file.path("appdata", "themes", "albers", "amph_sum.tif"))

## Bird - IUCN
IUCN_BIRD <- rast(file.path("appdata", "themes", "albers", "bird_sum.tif"))

## Mammal - IUCN
IUCN_MAMM <- rast(file.path("appdata", "themes", "albers", "mamm_sum.tif"))

## Reptile - ICUN
IUCN_REPT <- rast(file.path("appdata", "themes", "albers", "rept_sum.tif"))

## Endemic species - NatureServe Canada
NSC_END <- rast(file.path("appdata", "themes", "albers", "NSC_ENDsum.tif"))

## Species at risk - NatureServe Canada
NSC_SAR <- rast(file.path("appdata", "themes", "albers", "NSC_SARsum.tif"))

## Several species - NatureServe Canada
NSC_SPP <- rast(file.path("appdata", "themes", "albers", "NSC_SPPsum.tif"))

## Stack species rasters ----
spp_stack <- c(ECCC_SAR, IUCN_AMPH, IUCN_BIRD, 
               IUCN_MAMM, IUCN_REPT, NSC_END, 
               NSC_SAR, NSC_SPP)

spp_stack <- terra::setMinMax(spp_stack)

names(spp_stack) <- c("ECCC_SAR", "IUCN_AMPH", "IUCN_BIRD",
                      "IUCN_MAMM","IUCN_REPT", "NSC_END",
                      "NSC_SAR", "NSC_SPP")

# Extract (Zonal Statistics) ---------------------------------------------------

## Conservation themes (SUM) ----
PMP_feat_stack_mean <- exact_extract(feat_stack, PMP, fun = "sum", force_df = TRUE) %>%
  round(1) # round to 1 decimal
# Rename
names(PMP_feat_stack_mean) <- gsub("sum.", "", names(PMP_feat_stack_mean))
# Replace NA with 0
PMP_feat_stack_mean <- PMP_feat_stack_mean %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

## Species (MAX) ----
PMP_spp_stack_mean <- exact_extract(spp_stack, PMP, fun = "max", force_df = TRUE) %>%
  round(1) # round to 1 decimal
# Rename
names(PMP_spp_stack_mean) <- gsub("max.", "", names(PMP_spp_stack_mean))
# Replace NA with 0
PMP_spp_stack_mean <- PMP_spp_stack_mean %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

## Combine all extractions into one sf object ----
PMP_tmp <- cbind(PMP, PMP_feat_stack_mean, PMP_spp_stack_mean)

# Data Cleaning ----------------------------------------------------------------
## Fix geometry and populate new ID fields
PMP_tmp <- PMP_tmp %>% 
  st_make_valid() %>%
  mutate("id" = row_number()) %>%
  mutate("OBJECTID" = row_number())

## Remove 'Region' and 'Territory' from REGION field
PMP_tmp <- PMP_tmp %>% 
  mutate(REGION = str_remove_all(REG_NM_EN, " Region")) %>%
  mutate(REGION = str_remove_all(REGION, " Territory")) 
  
## Calculate area ha 
PMP_tmp$Area_ha <- units::drop_units(units::set_units(st_area(PMP_tmp), value = ha))

## Write extractions to disk as .shp
write_sf(PMP_tmp, OUTPUT_SHP_PATH)

## Remove confidential properties for impact app
PMP_tmp <- PMP_tmp %>% filter(CONF == 0)

# Project to WGS for impact app
PMP_tmp <- st_transform(PMP_tmp, crs = st_crs(4326))

# Save extractions for impact app ----
save(PMP_tmp, file = file.path("appdata", "basedata.RData"))
