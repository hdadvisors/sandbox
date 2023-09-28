library(tidyverse)
library(sf)
library(janitor)
library(knitr)
library()

# ph <- st_read("https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Public_Housing_Developments/FeatureServer/11/query?f=json&where=(FORMAL_PARTICIPANT_NAME%20IN%20('Roxboro%20Housing%20Authority'%2C%20'Oxford%20Housing%20Authority'%2C%20'Vance%20County%20Housing%20Authority'%2C%20'Roanoke-Chowan%20Regional%20Housing%20Authority'%2C%20'Roanoke%20Rapids%20Housing%20Authority'%2C%20'The%20Housing%20Authority%20of%20the%20City%20of%20Durham'%2C%20'Housing%20Authority%20of%20the%20County%20of%20Wake'%2C%20'Housing%20Authority%20of%20the%20City%20of%20Raleigh'))&outFields=*", quiet = TRUE)
# 
# write_rds(ph, "nc/data/ph.rds")

ph_units <- read_rds("nc/data/ph.rds") |> 
  st_drop_geometry() |> 
  clean_names() |> 
  group_by(formal_participant_name) |> 
  summarise(units = sum(total_units))

ph_developments <- read_rds("nc/data/ph.rds") |> 
  st_drop_geometry() |> 
  clean_names() |> 
  group_by(formal_participant_name, project_name) |> 
  summarise(units = sum(total_units))

ph_units |> 
  kable() |> 

