library(tidyverse)
library(hdatools)
library(fredr)

setwd("C:/Users/Jonathan/Documents/repos/sandbox/frying-pan/")

## Data import --------------

fred_import <- function(files) {

  raw <- map(
    files,
    ~ read.csv(
        paste0(.x, ".csv"),
        col.names = c("date", "value")
      )
  )
  
  named <- map2(
    raw, files,
    ~ mutate(.x, measure = .y, .before = 1)
  )

  data <- bind_rows(named) |> 
    mutate(date = as.Date(date)) |> 
    group_by(measure) |> 
    arrange(date) |> 
    mutate(pct_chg = (value - first(value))/first(value),
           index = (pct_chg*100)+100) |> 
    ungroup()
 
  data
  
}

files <- c(
  "GDP",        # GDP (Q, $bn)
  "PCE",        # Personal Consumption Expenditures (M, $bn)
  "PCEDG",      # PCE Durable Goods (M, $bn)
  "PCES",       # PCE Services (M, $bn)
  "SLEXPND",    # State and Local Government Current Expenditures (Q, $bn)
  "TLRESCONS",  # Total Construction Spending: Residential in the United States ($mm)
  "VABPPRIVSA", # New Private Housing Units Authorized by Building Permits for Virginia (Q, units)
  "VACONS",     # All Employees: Construction in Virginia (thousands)
  "VANA",       # All Employees: Total Nonfarm in Virginia (thousands)
  "VANGSP",     # Gross Domestic Product: All Industry Total in Virginia ($m)
  "VASTHPI"     # All-Transactions House Price Index for Virginia (Index 1980-Q1 = 100)
  )

fred_data <- fred_import(files)

fred_recession <- fredr(
  series_id = "USRECD",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-06-01")
  )

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

add_recession <- function() {
  
  geom_rect(
    data = recession,
    inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "#696969", alpha = 0.1
  )
  
}

## Plots --------------------

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

ggsave(
  "plot_gdp.png",
  width = 800, height = 800,
  units = "px",
  bg = "white"
  )

plot_pce <- fred_data |> 
  filter(measure %in% c("PCEDG", "PCES")) |> 
  mutate(trend = 95 + 1.65*row_number()) |> 
  ggplot(aes(x = date, y = index, color = measure)) +
  #geom_line(aes(y = trend), color = "#808ea0", linewidth = 0.4, linetype = 2) +
  geom_line(linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  theme(
    plot.title = ggtext::element_textbox_simple(
      size = 28
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 22,
      # color = "#383c3d", 
      #hjust = 0L,
      #vjust = 0L,
      margin = ggplot2::margin(t = 5, b = 10, unit = "pt"),
      # face = "plain",
      # family = "Open Sans"
      )
  ) +
  labs(
    title = "Personal Consumption Expenditures",
    subtitle = "Index 2000 Q1 = 100 | <span style='color:#011e41;'>**Durable Goods**</span> and <span style='color:#40c0c0;'>**Services**</span>",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

ggsave(
  "plot_pce.png",
  width = 800, height = 800,
  units = "px",
  bg = "white"
  )

plot_slg <- fred_data |> 
  filter(measure == "SLEXPND") |> 
  ggplot(aes(x = date, y = index)) +
  geom_line(color = "#b1005f", linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  theme(
    plot.title = ggtext::element_textbox_simple(
      size = 26
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(t = 5, b = 10, unit = "pt"),
    )
  ) +
  labs(
    title = "State and Local Government Expenditures",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

ggsave(
  "plot_slg.png",
  width = 800, height = 800,
  units = "px",
  bg = "white"
)

plot_tlrescons <- fred_data |> 
  filter(measure == "TLRESCONS") |> 
  ggplot(aes(x = date, y = index)) +
  geom_line(color = "#b1005f", linewidth = 0.6) +
  add_recession() +
  scale_color_hfv() +
  theme_hfv(base_size = 26) +
  theme(
    plot.title = ggtext::element_textbox_simple(
      size = 26
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      margin = ggplot2::margin(t = 5, b = 10, unit = "pt"),
    )
  ) +
  labs(
    title = "Construction Spending",
    subtitle = "Index 2000 Q1 = 100",
    caption = "**Source:** U.S. Bureau of Economic Analysis"
  )

ggsave(
  "plot_tlrescons.png",
  width = 800, height = 800,
  units = "px",
  bg = "white"
)
