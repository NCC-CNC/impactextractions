# impactextractions

The code in this repo is designed to extract national theme layers to
current project management plan (PMP) boundaries. 

Themes include: amphibians, biodiversity, birds, carbon (current), carbon (potential),
climate refugia, climate velocity, endemics, forest, freshwater, grassland, lakes,
mammals, recreation, reptiles, river shoreline, species at risk (ECCC), species at risk (NSC) and
wetland.

**Note:**
Data needed to execute `01_extract_themes.R` is not found in this repo. Please 
contact <daniel.wismer@natureconservancy.com> if you need to run this script. 

## Output
Extractions are saved to a `basedata.RData` file. This, along with other data 
found in the `impactextractions/appdata` folder is needed to run the `impact` 
shiny app for both dev and production environments.

`impact` is a golem shiny app, where `extdata` will sit outside of the 
root directory and in the `impactextractions` repo. This was decided so that local
development mirrors the production environment. In addition, this design will speed
up running `devtools::check()`. The `impact` shiny app will run inside a
docker container and access `extdata` outside of the container.
