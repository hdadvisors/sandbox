library(tidyverse)
library(tigris)
library(mapview)

va_counties <- counties(state = "Virginia")

nc_counties <- counties(state = "North Carolina")

spdc <- va_counties |> 
  filter(NAME %in% c("Brunswick", "Halifax", "Mecklenburg"))

nc <- nc_counties |> 
  filter(NAME %in% c("Person", "Granville", "Warren",
                     "Halifax", "Northampton", "Vance",
                     "Franklin", "Wake", "Durham"))

spdc_nc <- spdc |> bind_rows(nc)

mapview(spdc_nc, label = "NAME")
