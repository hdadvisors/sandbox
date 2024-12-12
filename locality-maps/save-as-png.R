library(tidyverse)
library(tigris)

rva <- counties(state = "Virginia", cb = TRUE) |> 
  filter(NAMELSAD == "Richmond city")

ggplot(rva) +
  geom_sf(color = NA) +
  theme_void()

ggsave("rva.png" , device = "png")
