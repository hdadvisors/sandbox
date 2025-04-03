library(tidyverse)
library(readxl)
library(hdatools)
library(gt)

# Federal claims

fed_claims <- read_csv("microblog/fed-workforce/all_weekly_claims.csv") |> 
  filter(st == "VA")

fed_claims_processed <- fed_claims |>
  # Convert week_filed to date
  mutate(
    week_filed = as.Date(rptdate, "%m/%d/%Y"),
    # Extract year
    year = year(week_filed),
    # Calculate day of year (1-366)
    day_of_year = yday(week_filed)
  ) |> 
  group_by(year) |>
  arrange(year, week_filed) |>
  mutate(week_num = row_number()) |>
  ungroup()

fed_claims_table_data <- fed_claims_processed |>
  filter(year >= 2022, year <= 2025, week_num <= 9) |> 
  select(year, week_num, c4) |>
  pivot_wider(
    names_from = week_num,
    values_from = c4
  )

fed_claims_table <- fed_claims_table_data |>
  gt() |>
  tab_header(
    title = "Initial weekly federal worker unemployment claims in Virginia",
    subtitle = "First 9 weeks of 2022–2025"
  ) |>
  cols_label(
    year = "Year"
  ) |>
  fmt_number(
    columns = 2:10,
    decimals = 0,
    use_seps = TRUE
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = year == 2025
    )
  ) |>
  tab_options(
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) |> 
  tab_source_note(source_note = md(
    "**Note:** Includes all Unemployment Compensation for Federal Employees (UCFE) claims. UCFE claims lag one week behind regular initial claims data.")) |> 
  tab_source_note(source_note = md(
    "**Source:** U.S. Department of Labor, Employment & Training Administration. *Unemployment Insurance Weekly Claims Data - Report r539cy*. Accessed March 19, 2025."
  )) |> 
  cols_width(everything() ~ pct(10))

fed_claims_table

# Unemployment claims

va_claims <- read_csv("microblog/fed-workforce/va_weekly_claims.csv")

va_claims_processed <- va_claims |>
  # Convert week_filed to date
  mutate(
    week_filed = as.Date(week_filed, "%m/%d/%Y"),
    # Extract year
    year = year(week_filed),
    # Calculate day of year (1-366)
    day_of_year = yday(week_filed)
  ) |> 
  group_by(year) |>
  arrange(year, week_filed) |>
  mutate(week_num = row_number()) |>
  ungroup()

# Create the plot
ggplot(
  filter(va_claims_processed, day_of_year < 70 & year > 2021),
  aes(
    x = day_of_year,
    y = claims_initial,
    group = year,
    color = as.factor(year)
    )
  ) +
  geom_line(color = "lightgrey") +
  geom_line(data = filter(va_claims_processed, year == 2025), 
            color = "firebrick", linewidth = 1) +
  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(
    title = "Initial Claims by Day of Year",
    x = "Month",
    y = "Initial Claims",
    color = "Year"
  ) +
  theme_hfv()

claims_table_data <- va_claims_processed |>
  filter(year >= 2022, year <= 2025, week_num <= 9) |> 
  select(year, week_num, claims_initial) |>
  pivot_wider(
    names_from = week_num,
    values_from = claims_initial
  )
  
claims_table <- claims_table_data |>
  gt() |>
  tab_header(
    title = "Initial weekly unemployment claims in Virginia",
    subtitle = "First 9 weeks of 2022–2025"
  ) |>
  cols_label(
    year = "Year"
  ) |>
  fmt_number(
    columns = 2:10,
    decimals = 0,
    use_seps = TRUE
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = year == 2025
    )
  ) |>
  tab_options(
    heading.align = "left",
    column_labels.font.weight = "bold"
  ) |> 
  tab_source_note(source_note = md(
    "**Source:** U.S. Department of Labor, Employment & Training Administration. *Unemployment Insurance Weekly Claims Data - Report r539cy*. Accessed March 19, 2025."
  ))

claims_table
