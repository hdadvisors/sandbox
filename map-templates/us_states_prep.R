## SETUP ------------------------------

library(tidyverse)
library(tigris)
library(sf)

setwd("../sandbox/map-templates")


## STANDARD ---------------------------

# us_states <- states(cb = TRUE, resolution = "20m") |> 
#   shift_geometry() |> 
#   select(GEOID, ST = STUSPS, NAME, geometry) |> 
#   arrange(GEOID)
# 
# write_rds(us_states, "data/us_states.rds")

us_states <- read_rds("data/us_states.rds")

us_states |> 
  ggplot() +
  geom_sf() +
  theme_void()


## HEXBIN -----------------------------

# us_states_hex <- read_sf("data/us_states_hexgrid.geojson") |> 
#   mutate(NAME = str_remove_all(google_name, " \\(United States\\)")) |> 
#   select(ST = iso3166_2, NAME, geometry) |> 
#   left_join(st_drop_geometry(us_states)) |> 
#   select(GEOID, ST, NAME, geometry) |> 
#   arrange(GEOID) |> 
#   st_transform(3857)
# 
# write_rds(us_states_hex, "data/us_states_hex.rds")

us_states_hex <- read_rds("data/us_states_hex.rds")

us_states_hex |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = ST)) +
  theme_void()


