library(tidyverse)
library(readxl)
library(janitor)


nc_counties <- c("Person", "Granville", "Warren", "Halifax", 
                 "Northampton", "Vance","Franklin", "Wake", "Durham")

zips <- c(27503,27572,27701,27702,27703,27704,
          27705,27706,27707,27708,27709,27710,
          27711,27712,27713,27715,27717,27722,
          27508,27525,27549,27596,27507,27509,
          27522,27565,27581,27582,27823,27839,
          27843,27844,27850,27870,27874,27887,
          27890,27820,27831,27832,27842,27845,
          27853,27862,27866,27867,27869,27876,
          27877,27897,27343,27541,27573,27574,
          27583,27536,27537,27544,27553,27556,
          27584,27502,27511,27512,27513,27518,
          27519,27523,27526,27529,27539,27540,
          27545,27560,27562,27571,27587,27588,
          27591,27592,27597,27601,27602,27603,
          27604,27605,27606,27607,27608,27609,
          27610,27611,27612,27613,27614,27615,
          27616,27617,27619,27620,27622,27623,
          27624,27625,27626,27627,27628,27629,
          27634,27635,27636,27640,27650,27656,
          27658,27661,27668,27675,27676,27690,
          27695,27697,27698,27699)

nc_prop <- read_excel("nc/data/nc_nhpd_properties.xlsx") |> 
  clean_names() |> 
  mutate(county = str_to_sentence(county)) |> 
  subset(county %in% c("Person", "Granville", "Warren", "Halifax", 
                       "Northampton", "Vance","Franklin", "Wake", "Durham")) |> 
  filter(property_status != "Inactive")



nc_subsidies <- read_excel("nc/data/nc_nhpd_subsidies.xlsx") |> 
  clean_names() |> 
  mutate(zip = as.numeric(zip_code)) |> 
  subset(zip %in% zips) |> 
  filter(subsidy_status != "Inactive") |> 
  filter(subsidy_name == "LIHTC")

write_csv(nc_subsidies, "nc/data/nc_lihtc.csv")  
