# Collect building permits data from Census BPS

library(tidyverse)
library(glue)

years <- 2000:2021

# Generate the appropriate names for the data

header_rows <- read_csv("https://www2.census.gov/econ/bps/County/co2021a.txt", 
                        col_names = FALSE,
                        n_max = 2)

# Parse column names

column_names <- header_rows |> 
  select(X1:X18) |> 
  t() |> 
  as_tibble() |> 
  mutate(group = rep(1:6, each = 3)) |> 
  group_by(group) |> 
  fill(V1, .direction = "updown") |> 
  mutate(names = paste0(V1, ": ", V2)) |> 
  pull(names)

# Scrape annual county data and apply column names

cbps_annual <- map_df(years, ~{
  raw <- read_csv(glue("https://www2.census.gov/econ/bps/County/co{.x}a.txt"), skip = 2, 
                  col_names = FALSE) |> 
    select(X1:X18) |> 
    set_names(column_names)
  
  raw
  
})

# Scrape 2022 YTD and apply column names

cbps_2022 <- read_csv("https://www2.census.gov/econ/bps/County/co2212y.txt", skip = 2,
                      col_names = FALSE) |> 
  select(X1:X18) |> 
  set_names(column_names) |> 
  mutate('Survey: Date' = 2022)

# Append annual data with 2022 YTD

cbps_raw <- cbps_annual |> 
  bind_rows(cbps_2022)

# Filter data for Virginia and put into tidy format

cbps_data <- cbps_raw  |>  
  mutate(year = `Survey: Date`,
         GEOID = paste0(`FIPS: State`, `FIPS: County`)) |>
  select(`County: Name`,  `1-unit: Bldgs`:GEOID) |>
  filter(str_sub(GEOID, 1, 2) == "51") |>
  pivot_longer(`1-unit: Bldgs`:`5+ units: Value`,
               names_to = "type",
               values_to = "value") |>
  separate(type, into = c("Type", "col"), sep = ": ") |>
  pivot_wider(names_from = col,
              values_from = value) |>
  rename_with(tolower, Type:Value) |> 
  select(GEOID, NAME = `County: Name`, year, type:value) |> 
  mutate(GEOID = str_replace_all(GEOID, "51515", "51019"), # Merge 'Bedford city' values to 'Bedford county'
         GEOID = str_replace_all(GEOID, "51560", "51005")) # Merge 'Clifton Forge city' values to 'Alleghany county'

# Save data

write_csv(cbps_data, "building-permits.csv")
