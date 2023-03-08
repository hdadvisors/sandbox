# Load libraries

library(tidyverse)
library(httr)
library(jsonlite)
library(httr)
library(glue)

# Return only the columns needed.

col_spec <- cols_only(
  "activity_year" = col_double(),
  "lei" = col_character(),
  "county_code" = col_character(),
  "census_tract" = col_character(),
  "derived_loan_product_type" = col_character(),
  "derived_dwelling_category" = col_character(),
  "derived_ethnicity" = col_character(),
  "derived_race" = col_character(),
  "derived_sex" = col_character(),
  "action_taken" = col_integer(),
  "purchaser_type" = col_integer(),
  "loan_type" = col_integer(),
  "loan_purpose" = col_integer(),
  "reverse_mortgage" = col_integer(),
  "loan_amount" = col_character(),
  "loan_to_value_ratio" = col_character(),
  "interest_rate" = col_character(),
  "total_loan_costs" = col_character(),
  "loan_term" = col_character(),
  "property_value" = col_character(),
  "construction_method" = col_integer(),
  "occupancy_type" = col_integer(),
  "manufactured_home_secured_property_type" = col_integer(),
  "manufactured_home_land_property_interest" = col_integer(),
  "total_units" = col_character(),
  "applicant_age" = col_character(),
  "income" = col_double(),
  "debt_to_income_ratio" = col_character(),
  "denial_reason-1" = col_integer(),
  "denial_reason-2" = col_integer(),
  "denial_reason-3" = col_integer(),
  "denial_reason-4" = col_integer(),
  "tract_minority_population_percent" = col_double(),
  "ffiec_msa_md_median_family_income" = col_double(),
  "tract_to_msa_income_percentage" = col_double(),
  "tract_owner_occupied_units" = col_double()
)

hmda_pull <- map_dfr(2018:2021, ~{
  GET("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv", 
      query = list(
        states = "VA",
        years = .x
      ), 
      progress()) %>%
    content(as = "text") %>%
    read_csv(col_types = col_spec) 
})

hmda_pburg <- hmda_pull %>% 
  filter(county_code == 51730) %>% 
  filter(census_tract == 51730810700)

write_csv(hmda_pburg, "hmda/pnr_hmda.csv")

# library(fs)
# 
# file_list <- dir_ls("hmda/hmda_data")
# 
# 
# hmda_data <- map(file_list, read_csv)
