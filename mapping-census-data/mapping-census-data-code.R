library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)
library(rdeck)

Sys.setenv("MAPBOX_ACCESS_TOKEN" = "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHRoYm12eDQwMzZ1MnNvN2JyMzZqYXBpIn0.Sdd16XvAh70IwBrqDD7MzQ")

wfh_tract_list <- read_rds("data/wfh_tract_list.rds")

## install.packages(c("tidycensus", "tidyverse", "mapview", "ggspatial", "leafsync"))

## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)

texas_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "TX",
  year = 2022
)

texas_income

texas_income_sf <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "TX",
  year = 2022,
  geometry = TRUE
)

plot(texas_income_sf['estimate'])

texas_income_sf

## library(mapview)
## 
## mapview(
##   texas_income_sf,
##   zcol = "estimate"
## )

library(mapview)

mapview(texas_income_sf, zcol = "estimate")

## vars <- load_variables(2022, "acs5")
## 
## View(vars)
## 

nyc_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = c("New York", "Kings", "Queens",
             "Bronx", "Richmond"),
  year = 2022,
  geometry = TRUE
)


mapview(nyc_income, zcol = "estimate")

san_diego_race <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0073P",
    White = "DP05_0079P",
    Black = "DP05_0080P",
    Asian = "DP05_0082P"
  ),
  state = "CA",
  county = "San Diego",
  year = 2022,
  geometry = TRUE
)


san_diego_race

san_diego_race_wide <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0073P",
    White = "DP05_0079P",
    Black = "DP05_0080P",
    Asian = "DP05_0082P"
  ),
  state = "CA",
  county = "San Diego",
  geometry = TRUE,
  output = "wide",
  year = 2022
)

san_diego_race_wide

nyc_income_tiger <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = c("New York", "Kings", "Queens",
             "Bronx", "Richmond"),
  year = 2022,
  cb = FALSE,
  geometry = TRUE
)

library(tigris)
library(sf)
sf_use_s2(FALSE)

nyc_erase <- erase_water(
  nyc_income_tiger,
  area_threshold = 0.5,
  year = 2022
)


mapview(nyc_erase, zcol = "estimate")

library(tidyverse)

san_diego_asian <- filter(san_diego_race, variable == "Asian")

ggplot(san_diego_asian, aes(fill = estimate)) + 
  geom_sf()

cont_choro <- ggplot(san_diego_asian, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_c(option = "rocket") + 
  labs(title = "Percent Asian by Census tract",
       subtitle = "San Diego County, CA",
       fill = "ACS estimate",
       caption = "2018-2022 ACS | tidycensus R package")

cont_choro

classed_choro <- ggplot(san_diego_asian, aes(fill = estimate)) + 
  geom_sf() + 
  theme_void() + 
  scale_fill_viridis_b(option = "rocket", n.breaks = 6) + 
  labs(title = "Percent Asian by Census tract",
       subtitle = "San Diego County, CA",
       fill = "ACS estimate",
       caption = "2018-2022 ACS | tidycensus R package")

classed_choro

faceted_choro <- ggplot(san_diego_race, aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  theme_void() + 
  scale_fill_viridis_c(option = "rocket") + 
  facet_wrap(~variable) + 
  labs(title = "Race / ethnicity by Census tract",
       subtitle = "San Diego County, California",
       fill = "ACS estimate (%)",
       caption = "2018-2022 ACS | tidycensus R package")

faceted_choro

san_diego_race_counts <- get_acs(
  geography = "tract",
  variables = c(
    Hispanic = "DP05_0073",
    White = "DP05_0079",
    Black = "DP05_0080",
    Asian = "DP05_0082"
  ),
  state = "CA",
  county = "San Diego",
  geometry = TRUE,
  year = 2022
)

library(sf)

san_diego_hispanic <- filter(
  san_diego_race_counts, 
  variable == "Hispanic"
)

centroids <- st_centroid(san_diego_hispanic)


grad_symbol <- ggplot() + 
  geom_sf(data = san_diego_hispanic, color = "black", fill = "lightgrey") + 
  geom_sf(data = centroids, aes(size = estimate),
          alpha = 0.7, color = "navy") + 
  theme_void() + 
  labs(title = "Hispanic population by Census tract",
       subtitle = "2018-2022 ACS, San Diego County, California",
       size = "ACS estimate") + 
  scale_size_area(max_size = 6) 

grad_symbol

san_diego_race_dots <- as_dot_density(
  san_diego_race_counts,
  value = "estimate",
  values_per_dot = 200,
  group = "variable"
)

san_diego_race_dots

dot_density_map <- ggplot() + 
  geom_sf(data = san_diego_hispanic, color = "lightgrey", fill = "white") + 
  geom_sf(data = san_diego_race_dots, aes(color = variable), size = 0.01) + 
  scale_color_brewer(palette = "Set1") + 
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  theme_void() + 
  labs(color = "Race / ethnicity",
       caption = "2018-2022 ACS | 1 dot = approximately 200 people")

dot_density_map

library(ggspatial)

dot_density_with_basemap <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoom = 9) + 
  geom_sf(data = san_diego_race_dots, aes(color = variable), size = 0.01) + 
  scale_color_brewer(palette = "Set1") + 
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  theme_void() + 
  labs(color = "Race / ethnicity",
       caption = "2018-2022 ACS | 1 dot = approximately 200 people")

dot_density_with_basemap

library(viridisLite)

colors <- rocket(n = 100)

mv1 <- mapview(san_diego_asian, zcol = "estimate", 
        layer.name = "Percent Asian<br>2018-2022 ACS",
        col.regions = colors)

mv1

library(leafsync)

san_diego_white <- filter(san_diego_race, variable == "White")

m1 <- mapview(san_diego_asian, zcol = "estimate", 
        layer.name = "Percent Asian<br/>2018-2022 ACS",
        col.regions = colors)

m2 <- mapview(san_diego_white, zcol = "estimate", 
        layer.name = "Percent White<br/>2018-2022 ACS",
        col.regions = colors)

mv2 <- sync(m1, m2)

mv2

## install.packages("remotes")
## library(remotes)
## install_github("qfes/rdeck")

library(tidyverse)

fulton_inflow <- get_flows(
  geography = "county",
  state = "GA",
  county = "Fulton",
  geometry = TRUE,
  year = 2020
) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit()

fulton_top_origins <- fulton_inflow %>%
  slice_max(estimate, n = 30) 

library(rdeck)

Sys.setenv("MAPBOX_ACCESS_TOKEN" = "pk.eyJ1Ijoia3dhbGtlcnRjdSIsImEiOiJjbHRoYm12eDQwMzZ1MnNvN2JyMzZqYXBpIn0.Sdd16XvAh70IwBrqDD7MzQ")

fulton_top_origins$centroid1 <- st_transform(fulton_top_origins$centroid1, 4326)
fulton_top_origins$centroid2 <- st_transform(fulton_top_origins$centroid2, 4326)

flow_map <- rdeck(map_style = mapbox_light(), 
      initial_view_state = view_state(center = c(-98.422, 38.606), zoom = 3, 
                                      pitch = 45)) %>%
  add_arc_layer(get_source_position = centroid2,
          get_target_position = centroid1,
          data = as_tibble(fulton_top_origins),
          get_source_color = "#274f8f",
          get_target_color = "#274f8f",
          get_height = 1,
          get_width = scale_linear(estimate, range = 1:5),
          great_circle = TRUE
          )

flow_map

library(tidycensus)
library(tidyverse)

top100counties <- get_acs(
  geography = "county",
  variables = "B01003_001",
  year = 2022,
  survey = "acs1"
) %>%
  slice_max(estimate, n = 100)

top100counties

## wfh_tract_list <- top100counties %>%
##   split(~NAME) %>%
##   map(function(county) {
##     state_fips <- str_sub(county$GEOID, 1, 2)
##     county_fips <- str_sub(county$GEOID, 3, 5)
## 
##     get_acs(
##       geography = "tract",
##       variables = "DP03_0024P",
##       state = state_fips,
##       county = county_fips,
##       year = 2022,
##       geometry = TRUE
##     )
##   })

library(mapview)

wfh_maps <- map(wfh_tract_list, function(county) {
  mapview(
    county, 
    zcol = "estimate",
    layer.name = "% working from home"
  ) 
})

wfh_maps$`San Mateo County, California`

utah_wfh_compare <- get_acs(
  geography = "county",
  variables = c(
    income17 = "CP03_2017_024",
    income22 = "CP03_2022_024"
  ),
  state = "UT",
  year = 2022
)

utah_wfh_compare

library(patchwork)

ts_maps <- purrr::map_dfr(c(2017, 2022), ~{
  dat <- get_acs(
    geography = "tract",
    variables = "B01001_001",
    state = "TX",
    county = "Collin County",
    geometry = TRUE,
    year = .x
  ) %>%
    mutate(year = .x)
})

ggplot(ts_maps, aes(fill = estimate)) + 
  geom_sf(lwd = 0.1) + 
  theme_void(base_size = 18) + 
  scale_fill_viridis_c() + 
  facet_wrap(~year)


library(sf)

wfh_17 <- get_acs(geography = "tract", variables = "B08006_017", year = 2017,
                  state = "UT", county = "Salt Lake", geometry = TRUE) %>%
  st_transform(6620)

wfh_22 <- get_acs(geography = "tract", variables = "B08006_017", year = 2022,
                  state = "UT", county = "Salt Lake", geometry = TRUE) %>%
  st_transform(6620)

library(sf)

wfh_22_to_17 <- wfh_22 %>%
  select(estimate) %>%
  st_interpolate_aw(to = wfh_17, extensive = TRUE)

library(mapview)
library(leafsync)

m22a <- mapview(wfh_22, zcol = "estimate", layer.name = "2020 geographies")
m17a <- mapview(wfh_22_to_17, zcol = "estimate", layer.name = "2015 geographies")

sync(m22a, m17a)

library(tigris)
options(tigris_use_cache = TRUE)

salt_lake_blocks <- blocks(
  "UT", 
  "Salt Lake", 
  year = 2020
)

wfh_17_to_22 <- interpolate_pw(
  from = wfh_17,
  to = wfh_22,
  to_id = "GEOID",
  weights = salt_lake_blocks,
  weight_column = "POP20",
  crs = 6620,
  extensive = TRUE
)

m17b <- mapview(wfh_17, zcol = "estimate", layer.name = "2017 geographies")
m22b <- mapview(wfh_17_to_22, zcol = "estimate", layer.name = "2022 geographies")

sync(m17b, m22b)

wfh_shift <- wfh_17_to_22 %>%
  select(GEOID, estimate17 = estimate) %>%
  left_join(
    select(st_drop_geometry(wfh_22), 
           GEOID, estimate22 = estimate), by = "GEOID"
  ) %>%
  mutate(
    shift = estimate22 - estimate17,
    pct_shift = 100 * (shift / estimate17)
  )

mapview(wfh_shift, zcol = "shift")
