library(tidyverse)
library(tigris)
library(hdatools)

va_pdc_locality <- read_csv("sandbox/hud-pro/va-pdc-locality.csv") |> 
  filter(type %in% c("city", "county"))

va_counties <- counties(state = 51, cb = TRUE) |> 
  select(name = NAMELSAD)

hda_pdcs <- c(
  "Northern Shenandoah",
  "West Piedmont",
  "PlanRVA",
  "Southside",
  "Northern Virginia",
  "New River Valley",
  "George Washington",
  "Central Shenandoah",
  "Central Virginia"
)

hda_localities <- c(
  "Petersburg city",
  "Winchester city",
  "Loudoun County",
  "Fairfax County",
  "Buckingham County",
  "Charlottesville city",
  "Virginia Beach city",
  "Alexandria city",
  "James City County"
)

va_counties_pdc <- va_counties |> 
  left_join(va_pdc_locality, by = join_by(name == county))

va_counties_pdc$hda_pdc <- case_match(
  va_counties_pdc$pdc,
  hda_pdcs ~ TRUE,
  .default = FALSE
)

va_counties_pdc$hda_local <- case_match(
  va_counties_pdc$name,
  hda_localities ~ TRUE,
  .default = FALSE
)

va_counties_pdc$code <- case_when(
  va_counties_pdc$hda_local == TRUE & va_counties_pdc$hda_pdc == FALSE ~ "2",
  va_counties_pdc$hda_local == TRUE & va_counties_pdc$hda_pdc == TRUE ~ "2",
  va_counties_pdc$hda_local == FALSE & va_counties_pdc$hda_pdc == TRUE ~ "1",
  .default = "0"
)

ggplot(va_counties_pdc, aes(fill = code)) +
  geom_sf(color = "#e3e3e3") +
  scale_fill_manual(values = c("2" = "#445ca9", "1" = "#8baeaa", "0" = "#e3e3e3")) +
  theme_void() +
  theme(legend.position = "none")
