library(tidyverse)
library(tidycensus)

hope_cos <- c("Galax city", "Bland", "Carroll", "Grayson", "Smyth", "Wythe")

years <- c(2010, 2021)

vars <- load_variables(2021, "acs5")

raw <- map_dfr(years, function(yr){
  pull <- get_acs(
    geography = "county",
    county = hope_cos,
    state = "VA",
    variables = c("B25064_001", "B19013_001"),
    year = yr,
    survey = "acs5",
    cache_table = TRUE
  ) |>
    mutate(year = yr)
})

data <- raw |> 
  mutate(locality = str_remove_all(NAME, ", Virginia")) |> 
  mutate(variable = case_when(
    variable == "B19013_001" ~ "Income",
    TRUE ~ "Rent"
  )) |> 
  mutate(cv = ((moe/1.645)/estimate)*100) |> 
  mutate(reliability = case_when(
    cv < 15 ~ "High reliability",
    cv >= 15 & cv <= 30 ~ "Medium reliability",
    cv > 30 ~ "Low reliability")
  )

table <- data |> 
  select(locality, year, variable, estimate) |> 
  pivot_wider(names_from = year, values_from = estimate) |> 
  mutate(pct_chg = (`2021`-`2010`)/`2010`) |> 
  pivot_wider(names_from = variable, values_from = 3:5)

write_csv(table, "~/repos/sandbox/hope-inc/rent-inc-change.csv")
