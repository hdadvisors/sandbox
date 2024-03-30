library(tidycensus)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(scales)



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
  mutate(NAME = str_remove_all(NAME, "; Virginia")) |> 
  mutate(AMI = percent(estimate/109400)) |> #FY 2023 Median Income
  st_transform(crs = "+proj=longlat +datum=WGS84")

write_rds(medincome, "incomemap/medincome.rds")



# mapview(medincome, zcol = "estimate")


pal <- colorNumeric(palette = "Blues", domain = medincome$estimate)


leaflet() %>%
  addTiles() |> 
  addPolygons(data = medincome,
              color = ~pal(estimate),
              fillOpacity = 0.6,
              opacity = 1,
              weight = 1,
              popup = ~paste("Location:", NAME, "<br>",
                             "2022 Median Household Income:", 
                             scales::dollar(estimate), "<br>",
                             "Percent AMI (using 2023 Median):", AMI)) |> 
  addLegend(data = medincome,
            pal = pal, 
            values = ~estimate,
            title = "Median Household Income",
            opacity = 0.6,
            position = "bottomright")
  
