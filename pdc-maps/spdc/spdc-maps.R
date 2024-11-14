## Load libraries -----------

library(tidyverse)
library(tigris)
library(mapview)

## Download features --------

# Function to get SPDC counties, cities, and towns at specified resolution

get_spdc_features <- function(resolution = c("500k", "5m", "20m")){
  
  # Download all city/count features in Virginia
  va_counties <- counties(state = "Virginia", cb = TRUE, resolution = resolution)
  
  # Download all place features in Virginia (includes towns)
  va_places <- places(state = "Virginia", cb = TRUE)
  
  # Filter only SPDC localities
  spdc_counties <- va_counties |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Brunswick County",
        "Halifax County",
        "Mecklenburg County"
      )
    )
  
  # Filter only CSPDC towns
  spdc_places <- va_places |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Alberta town",
        "Boydton town",
        "Brodnax town",
        "Chase City town",
        "Clarksville town",
        "Halifax town",
        "La Crosse town",
        "Lawrenceville town",
        "Scottsburg town",
        "South Boston town",
        "South Hill town",
        "Virgilina town"
      )
    )
  
  # Join SPDC localities and towns, create new column for jurisdiction type
  spdc <- spdc_counties |> 
    bind_rows(spdc_places) |> 
    mutate(
      type = str_to_title(
        str_extract(NAMELSAD, "\\b\\w+$")
        )
      )
  
  spdc
  
}

# Create SPDC region at 500k resolution
spdc_500k <- get_spdc_features(resolution = "500k")

# Create SPDC region at 5m resolution
spdc_5m <- get_spdc_features(resolution = "5m")

## Save and export ----------

# Function to render and save map at specified resolution for all file formats

save_spdc_maps <- function(input) {
  
  # Define file formats
  file_type = c("png", "eps", "pdf")
  
  # Create character string from input feature name
  input_name <- deparse(substitute(input))
  
  # Assign colors by jurisdiction type
  cols = c(
    "County" = "grey90",
    "Town" = "grey60"
  )
  
  # Render ggplot map
  map <- ggplot(input, aes(fill = type)) +
    geom_sf(color = "white") +
    scale_fill_manual(values = cols, guide = "none") +
    theme_void()
  
  # Save map as graphic object in each format
  for (type in file_type) {
    
    # Create file name for selected resolution and file format
    file_name <- paste0("pdc-maps/spdc/", input_name, ".", type)
    
    # Save object
    ggsave(file_name, plot = map)
    
  }
  
}

# Save 500k resolution maps
save_spdc_maps(spdc_500k)

# Save 5m resolution maps
save_spdc_maps(spdc_5m)

