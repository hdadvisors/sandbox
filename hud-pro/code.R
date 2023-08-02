## Load libraries -----------

library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(shiny)
library(hdatools)

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
pdc <- read_csv("sandbox/hud-pro/va-pdc-locality.csv") |> 
  filter(type != "town") |> 
  select(1, 3)

# Load HUD PRO Housing priority geographies
hud_pro <- read_csv("sandbox/hud-pro/hud-pro-priority-geos.csv") |> 
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
  select(2, geography = 3, type = 4) |> 
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
) |> 
  lapply(htmltools::HTML)

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

## Export data --------------

write_rds(geos_priority, "sandbox/hud-pro/geos_priority.rds")
