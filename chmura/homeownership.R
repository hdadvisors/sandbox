# Collect households by tenure data from ACS, calculate homeownership rate

library(tidycensus)
library(tidyverse)

years <- 2010:2021

b25003 <- "B25003"

# Get variables for Table B25003: Tenure

b25003_vars <- load_variables(2021, "acs5") |> 
  filter(str_detect(name, b25003)) |> 
  filter(str_length(name) == 10)

b25003_cleaned <- b25003_vars |>
  separate(label, into = c("est", "total", "tenure"), sep = "!!") |> 
  select(variable = name, tenure) |> 
  mutate(tenure = replace_na(tenure, "All"))

# Get B25032 data for every locality in Virginia

output_b25003 <- map_dfr(b25003, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "county",
      table = tb,
      year = yr,
      state = "VA"
    ) %>%
      left_join(b25003_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, tenure,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})

# Clean up and calculate homeownership rates

b25003_prep <- output_b25003 |> 
  mutate(locality = str_remove_all(locality, ", Virginia"),
         locality = str_replace_all(locality, "Bedford city", "Bedford County"),
         fips = str_replace_all(fips, "51515", "51019")) |> 
  select(fips, locality, year, tenure, estimate, moe) |> 
  group_by(fips, locality, year, tenure) |> 
  summarize(estimate = sum(estimate), 
            moe = moe_sum(moe, estimate)) |> 
  filter(tenure != "Renter occupied") |>
  mutate(tenure = str_remove_all(tenure, " occupied"),
         tenure = str_to_lower(tenure)) |> 
  pivot_wider(names_from = tenure,
              values_from = c(estimate, moe)) |> 
  mutate(rate = estimate_owner / estimate_all,
         moe_rate = moe_prop(estimate_owner, estimate_all,
                             moe_owner, moe_all))

# Save data

write_csv(b25003_prep, "homeownership.csv")
