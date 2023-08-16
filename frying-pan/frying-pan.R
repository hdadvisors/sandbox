## Setup --------------------

library(tidyverse)
library(hdatools)
library(fredr)
library(janitor)
library(scales)

setwd("C:/Users/Jonathan/Documents/repos/sandbox/frying-pan/")

## Data import --------------

# List of csv files from FRED

files <- c(
  "GDP",        # GDP (Q, $bn)
  "PCE",        # Personal Consumption Expenditures (M, $bn)
  "PCEDG",      # PCE Durable Goods (M, $bn)
  "PCES",       # PCE Services (M, $bn)
  "SLEXPND",    # State and Local Government Current Expenditures (Q, $bn)
  "TLRESCONS",  # Total Construction Spending: Residential in the United States ($mm)
  "VABPPRIVSA", # New Private Housing Units Authorized by Building Permits for Virginia (Q, units)
  "VACONS",     # All Employees: Construction in Virginia (thousands)
  "VAECON",     # Construction Earnings in Virginia ($k)
  "VANA",       # All Employees: Total Nonfarm in Virginia (thousands)
  "VANGSP",     # Gross Domestic Product: All Industry Total in Virginia ($m)
  "VASTHPI"     # All-Transactions House Price Index for Virginia (Index 1980-Q1 = 100)
)

# Function to load and prep FRED data

fred_import <- function(files) {

  # Load csv files and apply standard column names
  raw <- map(
    files,
    ~ read.csv(
        paste0(.x, ".csv"),
        col.names = c("date", "value")
      )
  )
  
  # Add column for measure name
  named <- map2(
    raw, files,
    ~ mutate(.x, measure = .y, .before = 1)
  )

  # Combine data and calculate index values
  data <- bind_rows(named) |> 
    mutate(date = as.Date(date)) |> 
    group_by(measure) |> 
    arrange(date) |> 
    mutate(pct_chg = (value - first(value))/first(value),
           index = (pct_chg*100)+100) |> 
    ungroup()
 
  data
  
}

# Import FRED csv files

fred_data <- fred_import(files)

# Import wealth inequality csv and create date column

wealth <- read_csv("wealth-inequality.csv") |> 
  clean_names() |> 
  mutate(
    date = make_date(year, (quarter - 1) * 3 + 1),
    .before = 1
  )

## Recession periods --------

# Load daily recession designator from FRED API 

fred_recession <- fredr(
  series_id = "USRECD",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-06-01")
  )

# Filter only recession start/end days

recession <- fred_recession |> 
  mutate(recession = case_when(
    value == 1 & (lag(value, default = 0) == 0) ~ "start",
    value == 1 & (lead(value, default = 0) ==0) ~ "end",
    .default = NA
  )) |>
  filter(!is.na(recession)) |> 
  select(date, recession) |>
  pivot_wider(names_from = recession, values_from = date) |> 
  unnest()

# Function to add shaded recession bars to plots

add_recession <- function() {
  
  geom_rect(
    data = recession,
    inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "#696969", alpha = 0.1
  )
  
}

## Export function ----------

# Function to save plots with presets

my_ggsave <- function(
    plot,
    width = 1200,
    height = 800,
    units = "px",
    bg = "white",
    format = "png"
) {
  
  filename <- paste0(deparse(substitute(plot)), ".png")
  
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    bg = bg,
    device = format
  )
}

## Plots --------------------

# Gross Domestic Product

plot_gdp <- fred_data |> 
  filter(measure == "GDP") |> 
  mutate(trend = 95 + 1.65*row_number()) |> 
  ggplot(aes(x = date, y = index)) +
  geom_line(aes(y = trend), color = "#808ea0", linewidth = 0.4, linetype = 2) +
  geom_line(color = "#011e41", linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  labs(
    title = "U.S. Gross Domestic Product",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

my_ggsave(plot_gdp)

# Personal Consumption Expenditures (Durable Goods and Services)

plot_pce <- fred_data |> 
  filter(measure %in% c("PCEDG", "PCES")) |> 
  mutate(trend = 95 + 1.65*row_number()) |> 
  ggplot(aes(x = date, y = index, color = measure)) +
  geom_line(linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  labs(
    title = "Personal Consumption Expenditures",
    subtitle = "Index 2000 Q1 = 100 | <span style='color:#011e41;'>**Durable Goods**</span> and <span style='color:#40c0c0;'>**Services**</span>",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

my_ggsave(plot_pce)

# State and Local Government Expenditures

plot_slg <- fred_data |> 
  filter(measure == "SLEXPND") |> 
  ggplot(aes(x = date, y = index)) +
  geom_line(color = "#b1005f", linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  labs(
    title = "State and Local Government Expenditures",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

my_ggsave(plot_slg)

# Total Residential Construction Spending

plot_tlrescons <- fred_data |> 
  filter(measure == "TLRESCONS") |> 
  ggplot(aes(x = date, y = index)) +
  geom_line(color = "#b1005f", linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  labs(
    title = "Construction Spending",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

my_ggsave(plot_tlrescons)

# Real Wealth Growth

plot_wealth <- wealth |> 
  ggplot(aes(x = date, y = real_wealth_per_unit_growth, color = group)) +
  geom_hline(yintercept = 0, color = "#4b4f50", 
            linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  scale_y_continuous(
    limits = c(-1, 1.52),
    breaks = c(-1, -0.5, 0, 0.5, 1, 1.5),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_percent()
    ) +
  theme_hfv(base_size = 26) +
  labs(
    title = "Real Wealth Growth",
    subtitle = "<span style='color:#8b85ca;'>**Top 10%**</span>, <span style='color:#40c0c0;'>**Middle 40%**</span>, and <span style='color:#011e41;'>**Bottom 50%**</span> of adult earners (20+)",
    caption = "**Source:** realtimeinequality.org"
  )

my_ggsave(plot_wealth)

# Employment Level in Virginia (Nonfarm and Construction Employees)

plot_va_emp <- fred_data |> 
  filter(measure %in% c("VANA", "VACONS")) |> 
  ggplot(aes(x = date, y = index, color = measure)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 100, color = "#4b4f50", 
             linewidth = 0.3) +
  add_recession() +
  scale_color_hfv(-1) +
  theme_hfv(base_size = 26) +
  labs(
    title = "Employment Level in Virginia",
    subtitle = "Index 2000 Q1 = 100 | <span style='color:#011e41;'>**Total Nonfarm**</span> and <span style='color:#40c0c0;'>**Construction**</span> employees",
    caption = "**Source:** U.S. Bureau of Labor Statistics"
  )

my_ggsave(plot_va_emp)

# Residential Building Permits and House Price Index

housing.labs <- c("Residential Building Permits", "House Price Index")
names(housing.labs) <- c("VABPPRIVSA", "VASTHPI")

plot_va_housing <- fred_data |> 
  filter(measure %in% c("VABPPRIVSA", "VASTHPI")) |> 
  ggplot(aes(x = date, y = index, color = measure)) +
  geom_hline(yintercept = 100, color = "#4b4f50", 
             linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  add_recession() +
  facet_wrap(
    ~measure,
    scales = "free_y",
    labeller = labeller(measure = housing.labs) 
    ) +
  scale_color_hfv(-1) +
  theme_hfv(base_size = 26) +
  labs(
    title = "Building Permits and Home Prices in Virginia",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Census Bureau and  U.S. Federal Housing Finance Agency"
  )

my_ggsave(plot_va_housing)
