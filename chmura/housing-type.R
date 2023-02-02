# Collect housing type data from ACS

library(tidycensus)
library(tidyverse)

years <- 2010:2021

b25032 <- "B25032"

# Get variables for Table B25032: Tenure by Units in Structure

b25032_vars <- load_variables(2021, "acs5") |> 
  filter(str_detect(name, b25032)) |> 
  filter(str_length(name) == 10)

# Clean B25032 variable names

b25032_cleaned <-  b25032_vars |> 
  separate(label, into = c("est", "tot", "tenure", "type"),
           sep = "!!") |> 
  select(variable = name, tenure, type) |>
  filter(across(c(tenure, type), ~!is.na(.x))) |> 
  mutate(across(.fns = ~str_remove_all(.x, ":")),
         tenure = str_remove_all(tenure, "-occupied housing units")) |> 
  mutate(type = case_when(
    type == "1, detached" ~ "Single-family (detached)",
    type == "1, attached" ~ "Single-family (attached)",
    type == "2" ~ "2 units",
    type == "3 or 4" ~ "3 or 4 units",
    type == "5 to 9" ~ "5 to 9 units",
    type == "10 to 19" ~ "10 to 19 units",
    type == "20 to 49" ~ "20 to 49 units",
    type == "50 or more" ~ "50 or more units",
    type == "Mobile home" ~ "Mobile (manufactured) home",
    type == "Boat, RV, van, etc." ~ "Other")
  )

# Get B25032 data for every locality in Virginia

output_b25032 <- map_dfr(b25032, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) %>%
      left_join(b25032_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, tenure,
             type, estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})

# Clean up data

b25032_prep <- output_b25032 |> 
  drop_na() |> 
  mutate(locality = str_remove_all(locality, ", Virginia"),
         locality = str_replace_all(locality, "Bedford city", "Bedford County"),
         fips = str_replace_all(fips, "51515", "51019")) |> 
  select(fips, locality, year, tenure, type, estimate, moe) |> 
  group_by(fips, locality, year, tenure, type) |> 
  summarize(estimate = sum(estimate), 
            moe = moe_sum(moe, estimate)) 

# Save data

write_csv(b25032_prep, "housing-type.csv")
