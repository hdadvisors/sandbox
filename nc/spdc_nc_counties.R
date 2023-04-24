library(tidyverse)
library(tigris)
library(mapview)
library(hdatools)

va_counties <- counties(state = "Virginia")

nc_counties <- counties(state = "North Carolina")

spdc <- va_counties |> 
  filter(NAME %in% c("Brunswick", "Halifax", "Mecklenburg"))

nc <- nc_counties |> 
  filter(NAME %in% c("Person", "Granville", "Warren",
                     "Halifax", "Northampton", "Vance",
                     "Franklin", "Wake", "Durham"))

spdc_nc <- spdc |> bind_rows(nc)

#mapview(spdc_nc, label = "NAME")

ggplot(spdc_nc, aes(fill = STATEFP, label = NAME)) +
  geom_sf(color = "white") +
  geom_sf_label(fill = "white", size = 3) +
  scale_fill_hfv() +
  labs(title = "SPDC and relevant North Carolina counties") +
  theme_hfv() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank())
  
