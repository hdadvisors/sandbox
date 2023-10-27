library(tidyverse)
library(sf)
library(mapview)

props <- st_read("sandbox/wppdc/martinsville-developable-properties/Developable.shp") |> 
  st_zm()

mapview(props)
