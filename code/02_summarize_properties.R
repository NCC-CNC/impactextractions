library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmltools)

# Read-in extracted NCC parcel layer ----
ncc <- read_sf("C:/Data/National/NCC_Accomplishments/Extractions/NCC_Extracted_Nov1_2022.shp") %>%
  st_drop_geometry() 

# Summarize metrics across entire NCC portfolio (15+M ha) ----
metrics <- c("Area", "Forest", "Grassland", "Wetland", "Rivers", "Lakes", "Shoreline", 
             "Current Carbon", "Carbon Potential")

total <- c((prettyNum(round(sum(ncc$Area_ha),0), big.mark=",")),
           (prettyNum(round(sum(ncc$Forest),0), big.mark=",")), 
           (prettyNum(round(sum(ncc$Grassland),0), big.mark=",")),
           (prettyNum(round(sum(ncc$Wetland),0), big.mark=",")),
           (prettyNum(round(sum(ncc$River),0), big.mark=",")), 
           (prettyNum(round(sum(ncc$Lakes),0), big.mark=",")), 
           (prettyNum(round(sum(ncc$Shore),0), big.mark=",")),
           (prettyNum(round(sum(ncc$Carbon_C),0), big.mark=",")),
           (prettyNum(round(sum(ncc$Carbon_P),0), big.mark=",")))

units <- c("ha", "ha", "ha", "ha", "km", "ha", "km", "tonnes", "tonnes/yr")

ncc_tbl <- data.frame(
  Metric = metrics,
  Total = total,
  Unit = units
)

write.csv(ncc_tbl, "C:/Github/impactextractions/appdata/plots/impact_nov3_2022.csv", row.names = FALSE)
  
# Build year over year dataset ----
ncc_stats <- ncc %>%
  arrange(SE_DATE) %>%
  drop_na() %>%
  mutate(YEAR = as.numeric(format(SE_DATE, format = "%Y"))) %>%
  group_by(YEAR) %>%
  summarise(tot_ha =  round(sum(Area_ha, 0)),
            tot_forest = round(sum(Forest, 0)),
            tot_grass =  round(sum(Grassland, 0)),
            tot_wet =  round(sum(Wetland, 0)),
            tot_river =  round(sum(River, 0)),
            tot_lake =  round(sum(Lakes, 0)),
            tot_shore =  round(sum(Shore, 0)),
            tot_carbon_c = round(sum(Carbon_C, 0)),
            tot_carbon_p = round(sum(Carbon_P, 0))) %>%
  arrange(YEAR) %>%
  mutate(c_ha = cumsum(tot_ha)) %>%
  mutate(c_forest = cumsum(tot_forest)) %>%
  mutate(c_grass = cumsum(tot_grass)) %>%
  mutate(c_wet = cumsum(tot_wet)) %>%
  mutate(c_river = cumsum(tot_river)) %>%
  mutate(c_lake = cumsum(tot_lake)) %>%
  mutate(c_shore = cumsum(tot_shore)) %>%
  mutate(c_carbon_c = cumsum(tot_carbon_c)) %>%
  mutate(c_carbon_p = cumsum(tot_carbon_p))

# Yearly impact
ncc_stats_yearly <- data.frame(
  "Year" = ncc_stats$YEAR,
  "Area (ha)" = ncc_stats$tot_ha,
  "Forest (ha)" = ncc_stats$tot_forest,
  "Grassland (ha)" = ncc_stats$tot_grass,
  "Wetland (ha)" = ncc_stats$tot_wet,
  "Rivers (km)" = ncc_stats$tot_river,
  "Lakes (ha)" = ncc_stats$tot_lake,
  "Shore (km)" = ncc_stats$tot_shore,
  "Current Carbon (tonnes)" = ncc_stats$tot_carbon_c,
  "Carbon Potential (tonnes/yr)" = ncc_stats$tot_carbon_p,
  check.names = FALSE
)
write.csv(ncc_stats_yearly, "C:/Github/impactextractions/appdata/plots/impact_yearly_nov3_2022.csv", row.names = FALSE)

# Cumulative impact
ncc_stats_cumlative <- data.frame(
  "Year" = ncc_stats$YEAR,
  "Area (ha)" = ncc_stats$c_ha,
  "Forest (ha)" = ncc_stats$c_forest,
  "Grassland (ha)" = ncc_stats$c_grass,
  "Wetland (ha)" = ncc_stats$c_wet,
  "Rivers (km)" = ncc_stats$c_river,
  "Lakes (ha)" = ncc_stats$c_lake,
  "Shore (km)" = ncc_stats$c_shore,
  "Current Carbon (tonnes)" = ncc_stats$c_carbon_c,
  "Carbon Potential (tonnes/yr)" = ncc_stats$c_carbon_p,
  check.names = FALSE
)
write.csv(ncc_stats_cumlative, "C:/Github/impactextractions/appdata/plots/impact_cumulative_nov3_2022.csv", row.names = FALSE)

# Create year-over-year plots ----
## Area ----
ncc_area <- ggplot(ncc_stats, aes(YEAR, tot_cum_ha)) +
  geom_line(color = "#2D602E", size = 0.4) +
  geom_point(aes(text=paste0("Year: ", YEAR, "<br>", "Ha: ", prettyNum(tot_cum_ha,big.mark=","))),
             color = "#2D602E") +
  theme_light() +
  xlab("Year") +
  ylab("Area (ha)") +
  geom_hline(yintercept=13331334, linetype='dashed', col='#33862B', size = 0.2) +
  annotate("text", x = 2000, y = 13100000, label = "Tallurutiup Imanga National Marine Conservation Area (~9,980,945 Ha)",
           size = 3, color = "#33862B") +  
  scale_x_continuous(breaks=seq(1960,2022,5)) +
  scale_y_continuous(label=scales::comma, breaks = seq(0, 15750000, by=1000000), limits=c(0,15750000))

ncc_area_plotly <- ggplotly(ncc_area, tooltip = "text")
htmlwidgets::saveWidget(as_widget(ncc_area_plotly), "appdata/plots/ncc_area.html")

## Carbon Potential ----
ncc_carbon_p <- ggplot(ncc_stats, aes(YEAR, tot_cum_cabon_p)) +
  geom_line(color = "#2D602E", size = 0.4) +
  geom_point(aes(text=paste0("Year: ", YEAR, "<br>", "tonnes per year: ", prettyNum(tot_cum_cabon_p,big.mark=","))),
             color = "#2D602E") +
  theme_light() +
  xlab("Year") +
  ylab("Carbon Potential (tones per year)") +
  scale_x_continuous(breaks=seq(1960,2022,5)) +
  scale_y_continuous(label=scales::comma, breaks = seq(0, 3250000, by=250000), limits=c(0,3250000))

ncc_carbon_p_plotly <- ggplotly(ncc_carbon_p, tooltip = "text")
htmlwidgets::saveWidget(as_widget(ncc_carbon_p_plotly), "appdata/plots/ncc_carbon_potential.html")

## Carbon Current ----
ncc_carbon_c <- ggplot(ncc_stats, aes(YEAR, tot_cum_carbon_c)) +
  geom_line(color = "#2D602E", size = 0.4) +
  geom_point(aes(text=paste0("Year: ", YEAR, "<br>", "tones: ", prettyNum(tot_cum_carbon_c,big.mark=","))),
             color = "#2D602E") +
  theme_light() +
  xlab("Year") +
  ylab("Carbon Current (tones)") +
  scale_x_continuous(breaks=seq(1960,2022,5)) +
  scale_y_continuous(label=scales::comma, breaks = seq(0, 2000000000, by=200000000), limits=c(0,2000000000))

ncc_carbon_c_plotly <- ggplotly(ncc_carbon_c, tooltip = "text")
htmlwidgets::saveWidget(as_widget(ncc_carbon_c_plotly), "appdata/plots/ncc_carbon_current.html")

## River ----
ncc_river <- ggplot(ncc_stats, aes(YEAR, tot_cum_river)) +
  theme_light() +
  geom_line(color = "#a6bddb") +
  geom_point(aes(text=paste0("Year: ", YEAR, "<br>", "Km: ", prettyNum(tot_cum_river,big.mark=","))), color = "#2b8cbe") +
  xlab("Year") +
  ylab("Length (km)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  scale_x_continuous(breaks=seq(1970,2022,5), limits = c(1970, 2022)) +
  scale_y_continuous(label=scales::comma, breaks = seq(0, 20000, by=2000), limits=c(0,20000))

ncc_river_plotly <- ggplotly(ncc_river, tooltip = "text")
htmlwidgets::saveWidget(as_widget(ncc_river_plotly), "appdata/plots/ncc_river.html")

## ECCC SAR ----
ncc_eccc_sar <- ggplot(ncc_stats, aes(YEAR, tot_cum_eccc_sar)) +
  theme_light() +
  geom_line(color = "#fb8072", size = 0.4) +
  geom_point(aes(text=paste0("Year: ", YEAR, "<br>", "ECCC SAR Count: ", prettyNum(tot_cum_eccc_sar,big.mark=","))), color = "#fb8072") +
  xlab("Year") +
  ylab("ECCC SAR Count") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  scale_x_continuous(breaks=seq(1970,2022,5), limits = c(1970, 2022)) +  
  scale_y_continuous(label=scales::comma, breaks = seq(0, 180000, by=25000), limits=c(0,180000))

ncc_eccc_sar_plotly <- ggplotly(ncc_eccc_sar, tooltip = "text")
htmlwidgets::saveWidget(as_widget(ncc_eccc_sar_plotly), "appdata/plots/ncc_eccc_sar.html")
