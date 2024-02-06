library(tidyverse)
library(tigris)
library(mapview)

# Function to get WPPDC counties, cities, and towns at specified resolution

get_wppdc_features <- function(resolution = c("500k", "5m", "20m")){
  
  va_counties <- counties(state = "Virginia", cb = TRUE, resolution = resolution)
  
  va_places <- places(state = "Virginia", cb = TRUE)
  
  wppdc_counties <- va_counties |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Danville city",
        "Pittsylvania County",
        "Martinsville city",
        "Henry County",
        "Franklin County",
        "Patrick County"
      )
    )
  
  wppdc_places <- va_places |> 
    select(GEOID, NAMELSAD) |> 
    filter(
      NAMELSAD %in% c(
        "Boones Mill town",
        "Rocky Mount town"
      )
    )
  
  wppdc <- wppdc_counties |> 
    bind_rows(wppdc_places) |> 
    mutate(
      type = str_to_title(
        str_extract(NAMELSAD, "\\b\\w+$")
        )
      )
  
  wppdc
  
}

# 

wppdc_500k <- get_wppdc_features(resolution = "500k")

wppdc_5m <- get_wppdc_features(resolution = "5m")

save_wppdc_map <- function(input, file_type = c("png", "eps", "pdf")) {
  
  input_name <- deparse(substitute(input))
  
  file_name <- paste0("wppdc/", input_name, ".", file_type)
  
  cols = c(
    "County" = "grey90",
    "City" = "grey70",
    "Town" = "grey80"
  )
  
  ggplot(input, aes(fill = type)) +
    geom_sf(color = "white") +
    scale_fill_manual(values = cols, guide = "none") +
    theme_void()
  
  ggsave(file_name)
  
}

save_wppdc_map(wppdc_5m, "png")
