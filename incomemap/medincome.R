library(tidycensus)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(scales)

b19013_defns <- load_variables(2021, "acs5") |> 
  filter(name == "B19013_001")

medincome <- get_acs(
  geography = "tract",
  state = "VA",
  table = "B19013",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) |> 
  filter(
    str_detect(NAME, "Richmond city") | 
      str_detect(NAME, "Henrico") | 
      str_detect(NAME, "Chesterfield")) |> 
  mutate(NAME = str_remove_all(NAME, "; Virginia"))



# mapview(medincome, zcol = "estimate")


pal <- colorNumeric(palette = "Blues", domain = medincome$estimate)


leaflet() |> 
  addTiles() |>
  addPolygons(data = medincome, 
              color = ~pal(estimate),
              fillOpacity = 0.6,
              opacity = 1,
              weight = 1,
              popup = scales::dollar_format(estimate))
  
