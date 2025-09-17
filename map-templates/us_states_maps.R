## SETUP ------------------------------

library(tidyverse)
library(tigris)
library(sf)

setwd("../sandbox/map-templates")

us_states_hex <- read_rds("data/us_states_hex.rds")

## DEFINE STATES ---------------------- 

# States with zoning reforms
selection_1 <- c(
  "AZ", "AR", "CA", "CT", "FL", "HI", "IA", "KY", 
  "ME", "MD", "MA", "MT", "NV", "NH", "NJ", "NC", 
  "OR", "RI", "TX", "UT", "VT", "WA", "WI"
)


## HEXBIN -----------------------------

us_states_hex |> 
  mutate(
    fill_color = if_else(ST %in% selection_1, "selection_1", "other")
  ) |> 
  ggplot() +
  geom_sf(
    aes(fill = fill_color),
    color = "white",
    linewidth = 1
  ) +
  geom_sf_text(
    data = . %>% filter(fill_color == "selection_1"),
    aes(label = ST),
    color = "white",
    fontface = "bold"
  ) +
  geom_sf_text(
    data = . %>% filter(fill_color == "other"),
    aes(label = ST),
    color = "#9d9d9d"
  ) +
  scale_fill_manual(
    values = c(
      "selection_1" = "#259591", 
      "other" = "#dcdbdb"),
    guide = NULL
  ) +
  theme_void()


