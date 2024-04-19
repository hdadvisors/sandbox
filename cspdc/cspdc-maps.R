## Load libraries -----------

library(tidyverse)
library(tigris)
library(mapview)

## Download features --------

# Function to get CSPDC counties, cities, and towns at specified resolution

get_cspdc_features <- function(resolution = c("500k", "5m", "20m")){
  
  # Download all city/count features in Virginia
  va_counties <- counties(state = "Virginia", cb = TRUE, resolution = resolution)
  
  # Download all place features in Virginia (includes towns)
  va_places <- places(state = "Virginia", cb = TRUE)
  
  # Filter only CSPDC localities
  cspdc_counties <- va_counties |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Bath County",
        "Highland County",
        "Lexington city",
        "Buena Vista city",
        "Rockbridge County",
        "Staunton city",
        "Augusta County",
        "Waynesboro city",
        "Harrisonburg city",
        "Rockingham County"
      )
    )
  
  # Filter only CSPDC towns
  cspdc_places <- va_places |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Monterey town",
        "Glasgow town",
        "Goshen town",
        "Craigsville town",
        "Grottoes town",
        "Bridgewater town",
        "Broadway town",
        "Dayton town",
        "Elkton town",
        "Mount Crawford town",
        "Timberville town"
      )
    )
  
  # Join CSPDC localities and towns, create new column for jurisdiction type
  cspdc <- cspdc_counties |> 
    bind_rows(cspdc_places) |> 
    mutate(
      type = str_to_title(
        str_extract(NAMELSAD, "\\b\\w+$")
        )
      )
  
  cspdc
  
}

# Create CSPDC region at 500k resolution
cspdc_500k <- get_cspdc_features(resolution = "500k")

# Create CSPDC region at 5m resolution
cspdc_5m <- get_cspdc_features(resolution = "5m")

## Save and export ----------

# Function to render and save map at specified resolution for all file formats

save_cspdc_maps <- function(input) {
  
  # Define file formats
  file_type = c("png", "eps", "pdf")
  
  # Create character string from input feature name
  input_name <- deparse(substitute(input))
  
  # Assign colors by jurisdiction type
  cols = c(
    "County" = "grey90",
    "City" = "grey60",
    "Town" = "grey85"
  )
  
  # Render ggplot map
  map <- ggplot(input, aes(fill = type)) +
    geom_sf(color = "white") +
    scale_fill_manual(values = cols, guide = "none") +
    theme_void()
  
  # Save map as graphic object in each format
  for (type in file_type) {
    
    # Create file name for selected resolution and file format
    file_name <- paste0("sandbox/cspdc/", input_name, ".", type)
    
    # Save object
    ggsave(file_name, plot = map)
    
  }
  
}

# Save 500k resolution maps
save_cspdc_maps(cspdc_500k)

# Save 5m resolution maps
save_cspdc_maps(cspdc_5m)
