## Collect PUMS Data for WFH Microblog --------------------

# 1. Setup ------------------------------------------------

library(tidyverse)
library(tidycensus)
library(srvyr)
library(scales)
library(ggalluvial)
library(ggpattern)
library(hdatools)

# Load PUMS variables to explore
pums_vars_2023 <- pums_variables |>  
  filter(year == 2023, survey == "acs1")

# Years
years <- c(2019, 2021, 2022, 2023)

# Function to get PUMS variables without weights
get_pums_var <- function(year, vars, wgts = NULL) {
  
  data <- get_pums(
    variables = vars,
    state = "VA",
    survey = "acs1",
    year = year,
    rep_weights = wgts
  )
  
  data
  
}

# 2. Variables --------------------------------------------

make_pums_vars <- function() {
  
  # HOUSING RECORD - BASIC VARIABLES
  vars_hr_basic <- c(
    "PUMA"
    # "PUMA10",      # PUMA (2010 definition)
    # "PUMA20"       # PUMA (2020 definition)
  )
  
  # HOUSING RECORD - HOUSING UNIT VARIABLES
  vars_hr_hu <- c(
    "BDSP",        # Number of bedrooms
    "TEN",         # Tenure
    "HISPEED"      # Broadband
  )
  
  # HOUSING RECORD - HOUSEHOLD VARIABLES
  vars_hr_hh <- c(
    "GRPIP",       # Gross rent as percent of income
    "HINCP",       # Household income
    "MV",          # When moved into home
    "NOC",         # Number of own children
    "OCPIP",       # Selected monthly owner costs as percent of income
    "WIF"          # Workers in family
  )
  
  # PERSON RECORD - PERSON VARIABLES
  vars_pr_p <- c(
    "AGEP",        # Age
    "COW",         # Class of worker
    "JWMNP",       # Travel time to work
    "JWRIP",       # Carpool status
    "JWTRNS",      # Means of transportation to work
    "MIGPUMA",     # Migration PUMA
    "WAGP",        # Wages or salary income
    "WKHP"         # Hours worked per week
  )
  
  # PERSON RECORD - RECODED PERSON VARIABLES
  vars_pr_rcp <- c(
    "ESR",         # Employment status recode
    "NAICSP",      # NAICS recode (based on 2017 NAICS codes)
    "POWPUMA",
    # "POWPUMA10",   # Place of work PUMA (2010 definition)
    # "POWPUMA20",   # Place of work PUMA (2020 definition)
    "SOCP"         # SOC codes (based on 2018 SOC codes)
  )
  
  vars_wfh <- c(
    vars_hr_basic, vars_hr_hu, vars_hr_hh, vars_pr_p, vars_pr_rcp
  )
  
  vars_wfh
  
}

vars_wfh <- make_pums_vars()


# 3. Collect PUMS weights ---------------------------------

# Get PUMS with weights only
pums_wgt <- years |> 
  set_names() |> 
  map(\(year) get_pums_var(year, vars = c("SERIALNO", "SPORDER"), wgts = "both")) |> 
  list_rbind(names_to = "year")


# 4. Collect PUMS variables -------------------------------

# Get PUMS data for multiple years
pums_var <- years |> 
  set_names() |> 
  map(\(year) get_pums_var(year, vars_wfh)) |> 
  list_rbind(names_to = "year")

# Save or load as needed
# write_rds(pums_wfh, "data/pums_var.rds")
# write_rds(pums_wgt, "data/pums_wgt.rds")

pums_wgt <- read_rds("data/pums_wgt.rds")
pums_var <- read_rds("data/pums_var.rds")

# Recode PUMS data

pums_vars_2023 |> 
  filter(var_code %in% vars_wfh) |> 
  write_csv("pums_vars_recode.csv")

pums_recode <- pums_var |> 
  mutate(
    TEN = case_when(
      TEN %in% c("1", "2") ~ "Owner",
      TEN %in% c("3", "4") ~ "Renter",
      TRUE ~ "Vacant / GQ"
    ),
    MV = case_when(
      MV == "b" ~ "Vacant / GQ",
      MV == "1" ~ "Within last year",
      MV == "2" ~ "Previous year",
      TRUE ~ "2 or more years ago"
    ),
    WIF = case_when(
      WIF == "b" ~ "N/A",
      WIF == "1" ~ "Sole worker",
      TRUE ~ "Multiple workers"
    ),
    COW = case_when(
      COW == "b" ~ "N/A",
      COW == "1" ~ "For-profit",
      COW == "2" ~ "Non-profit",
      COW == "3" ~ "Local government",
      COW == "4" ~ "State government",
      COW == "5" ~ "Federal government",
      COW %in% c("6", "7") ~ "Self-employed",
      TRUE ~ "Other"
    ),
    JWRIP = case_when(
      JWRIP == "0" ~ "N/A",
      JWRIP == "1" ~ "Drove alone",
      TRUE ~ "Carpool"
    ),
    JWTRNS = case_when(
      JWTRNS == "bb" ~ "N/A",
      JWTRNS %in% c("01", "07", "08") ~ JWRIP,
      JWTRNS %in% c("02", "03", "04", "05", "06") ~ "Public transit",
      JWTRNS %in% c("09", "10") ~ "Biked or walked",
      JWTRNS == "11" ~ "Worked from home",
      JWTRNS == "12" ~ "Other"
    ),
    ESR = case_when(
      ESR %in% c("1", "2") ~ "Civilian",
      ESR %in% c("4", "5") ~ "Military",
      TRUE ~ "N/A"
    )
  )


# 5. 

pums_wfh <- pums_recode |> 
  filter(ESR != "N/A") |> 
  left_join(pums_wgt)

jwtrns <- pums_wfh |> 
  to_survey(type = "person", design = "rep_weights") |> 
  filter(JWTRNS != "N/A") |> 
  group_by(year, JWTRNS) |> 
  summarise(
    n = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  ungroup()

jwtrns_plot <- jwtrns |> 
  bind_rows(
    jwtrns |> 
      filter(year == "2019") |> 
      mutate(year = "2020")
  )

jwtrns_col <- setNames(
  ifelse(unique(jwtrns$JWTRNS) == "Worked from home", "#c0327e", "#d2d2d2"),
  unique(jwtrns$JWTRNS)
)

p <- jwtrns_plot |> 
  ggplot(
    aes(x = year, y = pct, fill = JWTRNS, alluvium = JWTRNS)
  ) +
  geom_alluvium(
    color = "white",
    linewidth = 0.5,
    width = 0.6, alpha = 0.95, decreasing = FALSE,
    curve_type = "sigmoid"
  ) +
  #geom_vline(xintercept = 1) +
  #geom_vline(xintercept = 2) +
  geom_rect(
    aes(xmin = 1.6, xmax = 2.4, ymin = 0, ymax = 1.0),
    alpha = 0.01,
    fill = "white"
  ) +
  geom_rect_pattern(
    aes(xmin = 1.6, xmax = 2.4, ymin = 0, ymax = 1.0),
    alpha = 0,
    fill = "white",
    #pattern_colour = "white",
    pattern_fill = "#ececec",
    pattern_density = 0.25,
    #pattern_alpha = 0.5,
    pattern_linetype = 0,
    pattern_spacing = 0.02,
    pattern_size = 0
  ) +
  annotate("text", x = 5.35, y = 0.6, label = "Drove alone", size = 6, hjust = 0) +
  annotate("text", x = 5.35, y = 0.22, label = "Worked from home", size = 6, hjust = 0, color = "#c0327e", fontface = "bold") +
  annotate("text", x = 5.35, y = 0.12, label = "Carpooled", size = 5, hjust = 0) +
  annotate("text", x = 5.35, y = 0.07, label = "Public transit", size = 5, hjust = 0) +
  annotate("text", x = 5.35, y = 0.03, label = "Walked or biked", size = 5, hjust = 0) +
  annotate("text", x = 5.35, y = -0.01, label = "Other", size = 5, hjust = 0) +
  annotate("text", x = 2, y = 0.5, label = "2020 1-year data\nnot released", size = 6, hjust = 0.5, lineheight = 0.3, fontface = "italic") +
  geom_text(
    data = . %>% filter(JWTRNS == "Worked from home"),
    aes(
      x = c(1:5),
      y = c(0.11, 0.15, 0.22, 0.22, 0.22),
      label = c("6.0%", " ", "22.3%", "18.3%", "17.0%")
    ),
    size = 6.5,
    color = "white"
  ) +
  scale_x_discrete(
    limits = c("2019", "2020", "2021", "2022", "2023", " "),
    labels = c("2019", "2020", "2021", "2022", "2023", " "),
    expand = c(0.05, 0.05)
  ) +
  scale_y_continuous(limits = c(-0.05, 1.01), expand = c(0, 0)) +
  scale_fill_manual(values = jwtrns_col) +
  labs(
    title = "Remote work takes hold in Virginia",
    subtitle = "Commuting methods for all employed civilian workers",
    caption = "**Source:** American Community Survey Public Use Microdata Sample 1-year estimates"
  ) +
  theme_hfv(
    base_size = 20,
    axis.text.y = element_blank(),
    axis.text.x = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    title = element_markdown(size = 5)
  ) + 
  ggview::canvas(
    1000, 500, units = "px", scale = 1.5
  )


ggview::save_ggplot(
   p,  file = "jwtrns.png"
  )

