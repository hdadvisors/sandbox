## Load libraries -----------

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(mapview)
library(htmlwidgets)
library(htmltools)

## Import data --------------

# Download shapefile of localities (cities and counties)
localities <- counties("VA", cb = TRUE) |> 
  select(GEOID, NAMELSAD, LSAD)

# Download shapefile of places (towns and CDPs), filter out cities
places <- places("VA", cb = TRUE, year = 2019) |> 
  select(GEOID, NAMELSAD = NAME, LSAD) |> 
  filter(LSAD != 25) |> 
  mutate(
    NAMELSAD =
      case_when(
        GEOID == "5168880" ~ "Rose Hill CDP (Fairfax County)",
        LSAD == 43 ~ paste(NAMELSAD, "town"),
        LSAD == 57 ~ paste(NAMELSAD, "CDP"),
        .default = NAMELSAD
      )
  )

# Load locality-PDC crosswalk
pdc <- read_csv("va-pdc-locality.csv") |> 
  filter(type != "town") |> 
  select(1, 3)

# Load HUD PRO Housing priority geographies
hud_pro <- read_csv("hud-pro-priority-geos.csv") |> 
  filter(
    state == "Virginia",
    priority == "Yes"
  ) |> 
  group_by(jurisdiction) |> 
  summarise()

## Build output -------------

# Combine localities and places
geos <- bind_rows(localities, places)

# Dissolve localities into PDC polygons
pdc_geo <- localities |> 
  left_join(pdc, by = join_by("NAMELSAD" == "county")) |> 
  group_by(pdc) |> 
  summarise()

# Join geography centroids to PDC polygons
geos_pdc <- st_centroid(geos) |> 
  st_join(pdc_geo) |> 
  st_drop_geometry() |> 
  select(1, 4) |> 
  right_join(geos, by = "GEOID") |> 
  st_as_sf()

# Filter geographies to priority only
geos_priority <- geos_pdc |> 
  filter(NAMELSAD %in% hud_pro$jurisdiction) |> 
  select(1, 2, geography = 3, type = 4) |> 
  mutate(
    type = case_match(
      type,
      "06" ~ "County",
      "25" ~ "City",
      "43" ~ "Town",
      "57" ~ "CDP"
    )
  ) |> 
  st_transform(4326)

# Add html popup label
geos_priority$popup <- paste0(
  paste0("<b>", geos_priority$pdc, " PDC</b>"),
  "<br/>",
  geos_priority$geography
  ) 
  # lapply(htmltools::HTML)

# Calculate minimum and maximum x and y values for each feature
geos_priority$min_x <- sapply(st_geometry(geos_priority), function(geom) {
  st_bbox(geom)[1]
})
geos_priority$max_x <- sapply(st_geometry(geos_priority), function(geom) {
  st_bbox(geom)[3]
})
geos_priority$min_y <- sapply(st_geometry(geos_priority), function(geom) {
  st_bbox(geom)[2]
})
geos_priority$max_y <- sapply(st_geometry(geos_priority), function(geom) {
  st_bbox(geom)[4]
})

## Add population data ------

# Vector for GEOIDs of priority localities
localities_geoid <- geos_priority |> 
  filter(nchar(GEOID) == 5) |> 
  pull(GEOID)

# Vector for GEOIDs of priority places
places_geoid <- geos_priority |> 
  filter(nchar(GEOID) == 7) |> 
  #mutate(GEOID = str_extract(GEOID, ".{5}$")) |> 
  pull(GEOID)

# Combine vectors
priority_geoid <- c(localities_geoid, places_geoid)

# Get locality population data
localities_pop <- get_acs(
  geography = "county",
  variables = "B01001_001",
  state = "VA"
  #county = localities_geoid
) |> 
  select(GEOID, pop = estimate)

# Get places population data
places_pop <- get_acs(
  geography = "place",
  variables = "B01001_001",
  state = "VA"
) |> 
  select(GEOID, pop = estimate)

# Combine population data
geos_pop <- bind_rows(localities_pop, places_pop) |> 
  filter(GEOID %in% priority_geoid)
  
# Add population data to priority geographies
geos_priority <- geos_priority |> 
  left_join(geos_pop)

# Remove places fully contained by localities
places_centroids <- geos_priority |> 
  filter(nchar(GEOID) == 7) |> 
  st_centroid()

geos_overlap <- st_join(
  places_centroids,
  filter(geos_priority, nchar(GEOID) == 5),
  join = st_within
  ) |> 
  filter(!is.na(GEOID.y)) |> 
  pull(GEOID.x)

geos_priority_pop <- geos_priority |> 
  st_drop_geometry() |> 
  filter(!GEOID %in% geos_overlap)

# Total PDC populations
pdc_pop_priority <- geos_priority_pop |> 
  summarise(pop = sum(pop), .by = pdc)

pdc_pop <- geos_pdc |>
  st_drop_geometry() |>
  right_join(localities_pop) |> 
  summarise(total = sum(pop), .by = pdc) |> 
  left_join(pdc_pop_priority) |> 
  mutate(pct = pop/total)

## Export data --------------

write_rds(geos_priority, "geos_priority.rds")

write_rds(geos_priority_pop, "geos_priority_pop.rds")

write_rds(pdc_pop, "pdc_pop.rds")
