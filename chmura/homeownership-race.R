# Collect households by tenure by race data from ACS, calculate homeownership rate

library(tidycensus)
library(tidyverse)

years <- 2010:2021

b25003 <- c(paste0("B25003", LETTERS[2:9]))

# Create a function to convert variable to race or ethnicity variable

concept_to_race <- function(x) {
  out <- x %>%
    str_remove_all("HOUSEHOLDER") %>%
    str_remove_all("TENURE \\(|\\)") %>%
    str_to_title() %>%
    str_replace_all("And", "and") %>%
    str_replace_all("Or", "or") %>%
    str_remove_all(" Alone")
  
  out
}

# Get variables for Table B25003: Tenure

b25003_vars <- load_variables(2021, "acs5") |> 
  filter(str_sub(name, end = 7) %in% b25003) |> 
  filter(str_detect(name, "PR") == FALSE)

# Clean up variables

b25003_cleaned <- b25003_vars %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, into = c("est", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, race, tenure) %>% 
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")),
         across(.fns = ~str_remove_all(.x, " --")),
         across(.fns = ~str_replace_all(.x, "total", "All")),
         across(.fns = ~str_replace_all(.x, "Tenure", "All")))

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
      select(variable, year, locality = NAME, fips = GEOID, race, tenure,
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
  select(fips, locality, year, race, tenure, estimate, moe) |> 
  group_by(fips, locality, year, race, tenure) |> 
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

write_csv(b25003_prep, "homeownership-race.csv")
