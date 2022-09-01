library(tidyverse)
library(readxl)
library(janitor)

setwd("G:/My Drive/repos/rrh-framework")

# Load OEWS files already filtered to Richmond, VA MSA

oews_2019 <- read_xlsx("data/oews_rva_2019.xlsx",
                       sheet = "m2019",
                       na = c("*", "**", "#"))

oews_2021 <- read_xlsx("data/oews_rva_2021.xlsx",
                       sheet = "m2021",
                       na = c("*", "**", "#")) |> 
  janitor::clean_names(case = "snake")

# Pull out annual wage percentiles for each year

percentiles_2019 <- oews_2019 |> 
  filter(occ_code == "00-0000") |> 
  select(9, 24:28) |> 
  mutate(year = "y2019", .before = 1)

percentiles_2021 <- oews_2021 |> 
  filter(occ_code == "00-0000") |> 
  select(10, 26:30) |> 
  mutate(year = "y2021", .before = 1)

# Combine years, rearrange, and calculate percent change over time

percentiles <- bind_rows(percentiles_2019, percentiles_2021) |> 
  pivot_longer(3:7) |> 
  pivot_wider(names_from = year,
              values_from = value) |> 
  mutate(pct_change = (y2021 - y2019)/y2019,
         wage = case_when(
           name == "a_pct10" ~ "10th percentile",
           name == "a_pct25" ~ "25th percentile",
           name == "a_median" ~ "Median",
           name == "a_pct75" ~ "75th percentile",
           name == "a_pct90" ~ "90th percentile"
         ))

# Plot percent change in annual wage 

percentiles |> 
  mutate(wage = fct_relevel(wage,
                            "10th percentile",
                            "25th percentile",
                            "Median",
                            "75th percentile",
                            "90th percentile")) |> 
  ggplot(aes(x = pct_change, y = wage)) +
  geom_col() +
  geom_text(aes(label = scales::label_percent(accuracy = 0.1)(pct_change)),
            nudge_x = 0.01) +
  labs(title = "Percent change in annual wage in Richmond, VA MSA",
       subtitle = "May 2019 to May 2021") +
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank())


#
         