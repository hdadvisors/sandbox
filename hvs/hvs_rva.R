library(tidyverse)
library(readxl)
library(janitor)

setwd("G://My Drive/repos")

# Load in homeowner vacancy rate (for metros) xlsx file downloaded from Census website

hvs <- read_xlsx("sandbox/hvs/tab5_msa_15_22_hvr.xlsx",
                   col_names = c("msa",
                                 "q1v", "q1m",
                                 "q2v", "q2m",
                                 "q3v", "q3m",
                                 "q4v", "q4m"),
                   col_types = c("text", "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric"),
                   range = "B9:J675") |>
  
  # Filter and clean data
  
  filter(str_detect(msa, "Richmond")) |> 
  mutate(msa = str_remove_all(msa, "\\."),
         year = 2022:2015, .before = 2) |>
  
  # Rearrange data into tidy format
  
  pivot_longer(3:10) |> 
  mutate(type = case_when(
    name = str_detect(name, "v") ~ "rate",
    TRUE ~ "moe")) |> 
  mutate(name = str_sub(name, end = -2)) |> 
  pivot_wider(names_from = type,
              values_from = value) |>
  
  # Translate quarters into dates
  
  mutate(date = as.Date(case_when(
    name == "q1" ~ paste0(year, "-03-31"),
    name == "q2" ~ paste0(year, "-06-30"),
    name == "q3" ~ paste0(year, "-09-30"),
    name == "q4" ~ paste0(year, "-12-31")
  )), .before = 4) |> 
  
  # Add lower and upper bounds of 90% confidence interval
  
  mutate(rate_lo = rate - moe,
         rate_hi = rate + moe)
  

# Plot data

hvs |> 
  ggplot(aes(x = date, y = rate, ymin = rate_lo, ymax = rate_hi)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(
    title = "Homeowner vacancy rate for Richmond, VA MSA",
    subtitle = "2015 Q1 to 2022 Q2",
    caption = "Shaded area represents 90 percent confidence interval.\nSource: U.S. Census Bureau, Current Population Survey/Housing Vacancy Survey, August 2, 2022.") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())
