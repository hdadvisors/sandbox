library(tidyverse)
library(geojsonsf)
library(sf)

lookup <- read_csv("local_lookup.csv") |> 
  mutate(county = name_long)


cb7 <- read_rds("cb_7.rds") |> 
  filter(year == 2021,
         tenure == "Renter",
         household_income == "31 to 50% AMI" | household_income == "30% AMI or less"
         ) |> 
  mutate(county = str_to_title(county)) |> 
  left_join(lookup, by = "county") |> 
  group_by(county, fips_locality) |> 
  summarise(estimate = sum(estimate))


hcv <- st_make_valid(st_read("va_hcv.geojson")) |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  group_by(COUNTY) |> 
  summarise(HCV_PUBLIC = sum(HCV_PUBLIC)) |> 
  mutate(fips_locality = COUNTY) |> 
  left_join(cb7, by = "fips_locality") |> 
  mutate(ratio = (HCV_PUBLIC/estimate)*100)

write_rds(hcv, "hcv_clean.rds")

hcv <- read_rds("hcv_clean.rds") |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  mutate(county = case_when(
    fips_locality == "093" ~ "Isle of Wight County",
    fips_locality == "097" ~ "King and Queen County",
    TRUE ~ county
  ))
  
  
library(leaflet)

hcv_pal <- colorBin(
  palette = c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"),
  domain = c(0, 40),
  bins = c(0, 5, 10, 15, 20, 30, 35, 40)
)

# Create popup content with styling applied directly to elements
popups <- lapply(seq(nrow(hcv)), function(i) {
  paste0(
    "<div style='font-family: Arial, sans-serif;'>",
    "<div style='font-size: 16px; font-weight: bold; color: #2B4162; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #eee;'>",
    hcv$county[i],
    "</div>",
    "<div style='margin: 8px 0; color: #333;'>",
    "<div style='font-size: 13px; color: #666;'>Housing Choice Vouchers</div>",
    "<div style='font-size: 15px; font-weight: 600; color: #2B4162; margin-top: 2px;'>",
    format(hcv$HCV_PUBLIC[i], big.mark = ","),
    "</div>",
    "</div>",
    "<div style='margin: 8px 0; color: #333;'>",
    "<div style='font-size: 13px; color: #666;'>Renters at 50% AMI or Less</div>",
    "<div style='font-size: 15px; font-weight: 600; color: #2B4162; margin-top: 2px;'>",
    sprintf("%.1f", hcv$estimate[i]),
    "</div>",
    "</div>",
    "</div>",
    "<div style='margin: 8px 0; color: #333;'>",
    "<div style='font-size: 13px; color: #666;'>Vouchers per 100 Renters at 50% AMI or Less</div>",
    "<div style='font-size: 15px; font-weight: 600; color: #2B4162; margin-top: 2px;'>",
    sprintf("%.1f", hcv$ratio[i]),
    "</div>",
    "</div>",
    "</div>"
  )
})

# Create the map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -79.5, lat = 37.5, zoom = 7) %>%
  addPolygons(
    data = hcv,
    fillColor = ~hcv_pal(ratio),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = popups,
    popupOptions = popupOptions(
      closeButton = TRUE,
      closeOnClick = FALSE,
      maxWidth = 300
    )
  ) %>%
  # Add a legend matching the screenshot
  addLegend(
    position = "bottomright",
    pal = hcv_pal,
    values = hcv$ratio,
    title = "HCVs per 100 Renters",
    opacity = 0.7,
    labFormat = labelFormat(suffix = "")
  )

hcv_filter <- hcv |> 
  subset