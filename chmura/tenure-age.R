# Collect households by tenure by age data from ACS

library(tidycensus)
library(tidyverse)

years <- 2010:2021

b25007 <- "B25007"

# Get variables for Table B25007: Tenure by Age of Householder

b25007_vars <- load_variables(2021, "acs5") |> 
  filter(str_detect(name, b25007))

b25007_cleaned <- b25007_vars |> 
  separate(label, into = c("est", "total", "tenure", "age"), sep = "!!") |> 
  select(variable = name, tenure, age) |> 
  drop_na() |> 
  mutate(tenure = str_remove_all(tenure, " occupied:"),
         age = str_remove_all(age, "Householder | years"))

# Get B25007 data for every locality in Virginia

output_b25007 <- map_dfr(b25007_cleaned$variable, function(vars) {
  
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "county",
      variables = vars,
      year = yr,
      state = "VA"
    ) |> 
      left_join(b25007_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull |> 
      mutate(year = yr) |> 
      select(variable, year, locality = NAME, fips = GEOID, tenure, age,
             estimate, moe)
    
    acs_rearranged
    
  })
  
  yearly_data
  
})

# Clean up

b25007_prep <- output_b25007 |> 
  mutate(locality = str_remove_all(locality, ", Virginia"),
         locality = str_replace_all(locality, "Bedford city", "Bedford County"),
         fips = str_replace_all(fips, "51515", "51019")) |> 
  select(fips, locality, year, tenure, age, estimate, moe) |> 
  group_by(fips, locality, year, tenure, age) |> 
  summarize(estimate = sum(estimate), 
            moe = moe_sum(moe, estimate))

# Save data

write_csv(b25007_prep, "tenure-age.csv")
