library(tidyverse)
library(tigris)

hcv_tract_va <- read_csv("hcv-tract-va.csv")

data(fips_codes)
  
fips <- fips_codes |> 
  filter(state == "VA")

hcv_local <- hcv_tract_va |> 
  summarise(n = sum(HCV_PUBLIC, na.rm = TRUE), .by = COUNTY) |> 
  left_join(fips, by = join_by(COUNTY == county_code)) |> 
  mutate(county = str_remove_all(county, " County"))

hcv_r10 <- hcv_local |> 
  filter(county %in% c("Albemarle", "Charlottesville city", "Fluvanna", "Greene", "Louisa", "Nelson")) |> 
  summarise(n = sum(n))

# Region Ten CSB
# 1349 - (602 + 322) = 425

hcv_nrv <- hcv_local |> 
  filter(county %in% c("Craig", "Giles", "Montgomery", "Floyd", "Pulaski")) |> 
  summarise(n = sum(n))

# Housing Connections
# 524

hcv_rr <- hcv_local |> 
  filter(county %in% c("Culpeper", "Fauquier", "Madison", "Orange", "Rappahannock")) |> 
  summarise(n = sum(n))

# RRCSB / Encompass
# 325  