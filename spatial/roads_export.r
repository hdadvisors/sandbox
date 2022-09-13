library(tidyverse)
library(tigris)
library(sf)

setwd("C:/Users/yates/Documents/repos/sandbox/spatial")

rva_roads <- tigris::roads("VA", c("041", "087", "760"))

rva_roads_plot <- ggplot(rva_roads) +
  geom_sf(size = 0.1) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

ggsave("rva_roads_plot.pdf",
       width = 10,
       height = 10,
       units = "in")

ggsave("rva_roads_plot.eps",
       width = 10,
       height = 10,
       units = "in")
