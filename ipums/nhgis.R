library(tidyverse)
library(ipumsr)

ipumsr::set_ipums_api_key("59cba10d8a5da536fc06b59d33e9a1f3da6e4c3e9a7d47368eda2a0b")

meta_tst <- get_metadata_nhgis("time_series_tables")

meta_dt <- get_metadata_nhgis("data_tables")

samples <- get_sample_info("usa")

get_nhgis_tst <- function(tst_name, geography, description, dir = "sandbox/ipums/") {
  
  tst <- tst_spec(
    tst_name,
    geog_levels = geography
  )
  
  ext <- define_extract_nhgis(
    description = description,
    time_series_tables = tst,
    tst_layout = "time_by_row_layout"
  )
  
  data <- submit_extract(ext)
  
  info <- get_extract_info(data)
  
  while (info$status != "completed") {
    Sys.sleep(5)
    info <- get_extract_info(data)
  }
  
  path <- download_extract(data, download_dir = dir)
  
  dl <- read_nhgis(path)
  
  dl
  
}

## -------

dec_samples <- samples |> 
  filter(str_detect(description, "0 1%$")) |> 
  pull(name)

dec_samples <- c(
  "us1850a", "us1860a", "us1870a", "us1880a",
  "us1900k", "us1910k", "us1920a", "us1930a",
  "us1940a", "us1950a", "us1960b", "us1970d",
  "us1980b", "us1990b", "us2000g", "us2010a",
  "us2022a" 
)

ext_dec <- define_extract_usa(
  "RACE / LABFORCE / COUNTYICP [1850-2022]",
  samples = dec_samples,
  variables = c("RACE", "LABFORCE", "STATEICP", "COUNTYICP")
)

data_dec <- submit_extract(ext_dec)

download_extract(data_dec, download_dir = "sandbox/ipums/", overwrite = TRUE)

microdata_dec <- read_ipums_micro("sandbox/ipums/usa_00003.xml")

## Somerset County ---------------

# Time series: Total population 1790-2020

county_pop <- get_nhgis_tst("A00", "county", "A00: Total population [county] [1790-2020]")

somerset_pop <- county_pop |> 
  filter(COUNTY == "Somerset County" & STATE == "Maryland") |> 
  select(-9)

write_csv(somerset_pop, "sandbox/ipums/somerset_pop.csv")

# Time series: Population by race 1970 - 2022

county_race <- get_nhgis_tst("B18", "county", "B18: Population by race [county] [1970-2022]")

somerset_race <- county_race |> 
  filter(COUNTY == "Somerset County" & STATE == "Maryland")

write_csv(somerset_race, "sandbox/ipums/somerset_race.csv")

## Crisfield ---------------------

# Time series: Total population by place 1970-2022

place_pop <- get_nhgis_tst("AV0", "place", "AV0: Total population [place] [1970-2022]")

crisfield_pop <- place_pop |> 
  filter(STATE == "Maryland" & PLACE == "Crisfield city")

write_csv(crisfield_pop, "sandbox/ipums/crisfield_pop.csv")

# Time series: Population by race 1970 - 2022

place_race <- get_nhgis_tst("B18", "place", "B18: Population by race [place] [1970-2022]")

crisfield_race <- place_race |> 
  filter(STATE == "Maryland" & PLACE == "Crisfield city")

write_csv(crisfield_race, "sandbox/ipums/crisfield_race.csv")


#...

meta_race <- meta_dt |> 
  filter(description == "Race" & universe == "Persons")

ds <- ds_spec(
  "race_county",
  data_tables = "B18",
  geog_levels = "county"
)