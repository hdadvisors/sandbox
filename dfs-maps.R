# Load required libraries
library(tidyverse)
library(tigris)
library(sf)
library(hdatools)

# Set tigris options
options(tigris_use_cache = TRUE)

# 1. Retrieve Virginia counties and independent cities
va_localities <- counties(state = "VA", year = 2024, cb = TRUE)

va_counties <- va_localities |> 
  erase_water(area_threshold = 0.999)

# 2. Generate tibble with locality information
locality_data <- va_localities |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(
    fips_code = GEOID,
    full_name = NAMELSAD,
    short_name = NAME
  ) |>
  select(fips_code, full_name, short_name) |> 
  arrange(fips_code)

# 3. Create template for categorical tagging
# Copy and modify this section to assign categories to localities

logan <- c(
  "Albemarle County",
  "Augusta County",
  "Bedford County",
  "Bristol city",
  "Caroline County",
  "Chesapeake County",
  "Chesterfield County",
  "Colonial Heights city",
  "Fluvanna County",
  "Henrico County",
  "Isle of Wight County",
  "King George County",
  "Montgomery County",
  "Spotsylvania County",
  "Stafford County",
  "Suffolk city",
  "Virginia Beach city"
)

govos <- c(
  "Prince William County",
  "Manassas city",
  "Manassas Park city"
)

dts <- c("Loudoun County")

tyler <- c("Hanover County")

simplifile <- c(
  "Accomack County",
  "Albemarle County",
  "Alexandria city",
  "Arlington County",
  "Augusta County",
  "Bath County",
  "Bedford County",
  "Bland County",
  "Botetourt County",
  "Bristol city",
  "Brunswick County",
  "Campbell County",
  "Caroline County",
  "Carroll County",
  "Charles City County",
  "Charlotte County",
  "Chesapeake city",
  "Chesterfield County",
  "Craig County",
  "Culpeper County",
  "Cumberland County",
  "Danville city",
  "Dickenson County",
  "Dinwiddie County",
  "Fairfax County",
  "Fauquier County",
  "Floyd County",
  "Fluvanna County",
  "Franklin County",
  "Frederick County",
  "Fredericksburg city",
  "Giles County",
  "Goochland County",
  "Grayson County",
  "Greene County",
  "Greensville County",
  "Halifax County",
  "Hampton city",
  "Hanover County",
  "Henrico County",
  "Henry County",
  "Isle of Wight County",
  "James City County",
  "King and Queen County",
  "King George County",
  "King William County",
  "Lancaster County",
  "Lee County",
  "Loudoun County",
  "Lynchburg city",
  "Madison County",
  "Martinsville city",
  "Mecklenburg County",
  "Montgomery County",
  "Newport News city",
  "Norfolk city",
  "Northampton County",
  "Northumberland County",
  "Page County",
  "Patrick County",
  "Petersburg city",
  "Pittsylvania County",
  "Portsmouth city",
  "Powhatan County",
  "Prince Edward County",
  "Prince George County",
  "Prince William County",
  "Pulaski County",
  "Rappahannock County",
  "Richmond city",
  "Richmond County",
  "Roanoke city",
  "Roanoke County",
  "Rockingham County",
  "Russell County",
  "Salem city",
  "Scott County",
  "Shenandoah County",
  "Smyth County",
  "Southampton County",
  "Spotsylvania County",
  "Stafford County",
  "Staunton city",
  "Suffolk city",
  "Surry County",
  "Tazewell County",
  "Virginia Beach city",
  "Warren County",
  "Washington County",
  "Waynesboro city",
  "Williamsburg city",
  "Winchester city",
  "Wise County",
  "Wythe County",
  "York County"
)

csc <- c(
  "Accomack County",
  "Albemarle County",
  "Arlington County",
  "Augusta County",
  "Bedford County",
  "Bland County",
  "Botetourt County",
  "Brunswick County",
  "Campbell County",
  "Caroline County",
  "Carroll County",
  "Chesterfield County",
  "Craig County",
  "Culpeper County",
  "Dickenson County",
  "Dinwiddie County",
  "Fairfax County",
  "Floyd County",
  "Fluvanna County",
  "Franklin County",
  "Frederick County",
  "Giles County",
  "Goochland County",
  "Greene County",
  "Greensville County",
  "Halifax County",
  "Hanover County",
  "Henrico County",
  "Henry County",
  "Isle of Wight County",
  "James City County",
  "King and Queen County",
  "King George County",
  "King William County",
  "Lancaster County",
  "Lee County",
  "Loudoun County",
  "Madison County",
  "Mecklenburg County",
  "Montgomery County",
  "Northampton County",
  "Northumberland County",
  "Page County",
  "Patrick County",
  "Pittsylvania County",
  "Powhatan County",
  "Prince Edward County",
  "Prince George County",
  "Prince William County",
  "Pulaski County",
  "Richmond County",
  "Roanoke County",
  "Rockingham County",
  "Russell County",
  "Scott County",
  "Shenandoah County",
  "Smyth County",
  "Southampton County",
  "Spotsylvania County",
  "Stafford County",
  "Surry County",
  "Tazewell County",
  "Washington County",
  "Wise County",
  "Wythe County",
  "York County",
  "Alexandria city",
  "Bristol city",
  "Chesapeake city",
  "Fredericksburg city",
  "Hampton city",
  "Lynchburg city",
  "Manassas city",
  "Manassas Park city",
  "Martinsville city",
  "Newport News city",
  "Norfolk city",
  "Portsmouth city",
  "Richmond city",
  "Roanoke city",
  "Salem city",
  "Staunton city",
  "Suffolk city",
  "Virginia Beach city",
  "Waynesboro city",
  "Winchester city"
)

locality_categories <- locality_data |>
  mutate(
    category = case_when(
      # Example categorization - modify as needed
      full_name %in% logan ~ "Logan Systems, Inc.",
      full_name %in% govos ~ "GovOS", 
      full_name %in% dts ~ "DTS AlertMe",
      full_name %in% tyler ~ "Tyler Technologies", 
      TRUE ~ "None known"
    )
  ) |> 
  mutate(
    category = fct_relevel(
      category,
      "Logan Systems, Inc.",
      "GovOS",
      "DTS AlertMe",
      "Tyler Technologies",
      "None known"
    )
  ) |> 
  mutate(
    efile = case_when(
      full_name %in% simplifile & full_name %in% csc ~ "Simplifile and CSC",
      full_name %in% simplifile ~ "Simplifile only",
      full_name %in% csc ~ "CSC only",
      TRUE ~ "No e-file option"
    )
  ) |> 
  mutate(
    efile = fct_relevel(
      efile,
      "Simplifile and CSC",
      "Simplifile only",
      "CSC only",
      "No e-file option"
    )
  ) 

# 4. Join categories back to shapefile and create map
va_map_data <- va_localities |>
  left_join(locality_categories, by = c("GEOID" = "fips_code"))

# Define manual colors for each category
category_colors <- c(
  "Logan Systems, Inc." = "#4d617a",
  "GovOS" = "#79d2d2",
  "DTS AlertMe" = "#ada9d9",
  "Tyler Technologies" = "#c84c8f",
  "None known" = "#e3e3e3"
)

efile_colors <- c(
  "Simplifile and CSC" = "#4d617a",
  "Simplifile only" = "#79d2d2",
  "CSC only" = "#ada9d9",
  "No e-file option" = "#e3e3e3"
)

# Generate the map
ggplot(va_map_data) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  scale_fill_manual(values = category_colors, name = "Vendor") +
  theme_hfv() +
  labs(
    title = "Active local land records notification systems in Virginia",
    subtitle = "Current as of July 2025",
    caption = "**Source:** Preliminary inventory of local government and circuit court websites."
  ) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "inside",
    legend.justification.inside = "left",
    legend.position.inside = c(0,0.75),
    legend.title = element_text(),
    #legend.justification = "top",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

# Generate the map
ggplot(va_map_data) +
  geom_sf(aes(fill = efile), color = "white", size = 0.2) +
  scale_fill_manual(values = efile_colors, name = "Vendor") +
  theme_hfv() +
  labs(
    title = "Active eRecording platforms in Virginia",
    subtitle = "Current as of July 2025",
    caption = "**Source:** cscglobal.com and mortgagetech.ice.com."
  ) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "inside",
    legend.justification.inside = "left",
    legend.position.inside = c(0,0.75),
    legend.title = element_text(),
    #legend.justification = "top",
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
