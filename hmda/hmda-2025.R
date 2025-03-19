library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

hmda <- GET(
  "https://ffiec.cfpb.gov/v2/data-browser-api/view/csv",
  query = list(
    states = "VA",
    years = 2020
  ),
  progress()
) |> 
  content(as = "text") |> 
  read_csv()

hmda |> 
  filter(
    loan_purpose == 1,
    action_taken == 1,
    occupancy_type == 1
  ) |> 
  summarise(
    n = n(),
    .by = total_units
  ) |> 
  mutate(pct = n/sum(n)*100) |> 
  arrange(desc(pct))
