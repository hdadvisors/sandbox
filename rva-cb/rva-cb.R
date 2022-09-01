library(tidycensus)
library(tidyverse)

# Set years for for ACS pull

years <- 2015:2020

# Define PHA localities

pha <- c("51085", "51760", "51087", "51041")

# Table B25070: Gross Rent as a Percentage of Household Income

b25070_raw <- map_dfr(years, function(yr){
  b25070_pull <- get_acs(
    geography = "county",
    table = "B25070",
    state = "VA",
    county = "Richmond city",
    year = yr
  ) |> 
    mutate(year = yr)
})

# Table B25074: Household Income by Gross Rent as a Percentage of Household Income

b25074_raw <- map_dfr(years, function(yr){
  b25074_pull <- get_acs(
    geography = "county",
    table = "B25074",
    state = "VA",
    year = yr
  ) |> 
    mutate(year = yr)
})

vars_b25074 <- load_variables(2020, dataset = "acs5") |> 
  filter(str_detect(name, "B25074"))

vars_cb <- vars_b25074 |> 
  separate(label, into = c("est", "tot", "inc", "cb"), sep = "!!") |> 
  select(variable = name, inc, cb) |> 
  mutate(across(.fns = ~str_remove_all(.x, ":"))) |> 
  drop_na() |> 
  mutate(cb_group = case_when(
    str_detect(cb, "30.0|35.0|40.0|50.0") ~ "Cost-burdened",
    str_detect(cb, "20.0|25.0") ~ "Not cost-burdened",
    TRUE ~ "Not computed"
  ))

b25074_data <- b25074_raw |> 
  filter(GEOID %in% pha) |> 
  right_join(vars_cb, by = "variable")

cb_renter_inc_1620 <- b25074_data |> 
  filter(!year == 2015) |> 
  group_by(GEOID, year, inc, cb_group) |> 
  summarise(estimate = sum(estimate)) |>
  group_by(GEOID, inc, cb_group) |>
  mutate(change = estimate - lag(estimate)) |> 
  ungroup() |> 
  mutate(inc = fct_relevel(inc,
                           "Less than $10,000",
                           "$10,000 to $19,999",
                           "$20,000 to $34,999",
                           "$35,000 to $49,999",
                           "$50,000 to $74,999",
                           "$75,000 to $99,999",
                           "$100,000 or more"),
         cb_group = fct_relevel(cb_group,
                                "Cost-burdened",
                                "Not cost-burdened",
                                "Not computed"))

cb_renter_inc_1620 |> 
  ggplot(aes(x = as.character(year), y = estimate, fill = cb_group)) +
  geom_col(position = "dodge") +
  facet_wrap(~inc, nrow = 1, labeller = label_wrap_gen(width = 12, multi_line = TRUE)) +
  labs(title = "Renter households by income and cost burden type",
       subtitle = "2016 to 2020") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#d8b365", "#018571", "#80cdc1"))








cb_renter_inc <- b25074_data |> 
  group_by(year, inc, cb_group) |> 
  summarise(estimate = sum(estimate)) |>
  group_by(inc, cb_group) |>
  mutate(change = estimate - lag(estimate)) |> 
  drop_na() |> 
  mutate(run_diff = cumsum(change)) |> 
  ungroup() |> 
  mutate(inc = fct_relevel(inc,
                           "Less than $10,000",
                           "$10,000 to $19,999",
                           "$20,000 to $34,999",
                           "$35,000 to $49,999",
                           "$50,000 to $74,999",
                           "$75,000 to $99,999",
                           "$100,000 or more"),
         cb_group = fct_relevel(cb_group,
                                "Cost-burdened",
                                "Not cost-burdened",
                                "Not computed"))





ggplot(cb_renter,
       aes(x = year, y = change)) +
  geom_col() +
  labs(title = "Year-over-year change in cost-burdened renter households",
       subtitle = "2011-2015 through 2016-2020 American Community Survey 5-year estimates")

# CHAS cost burden data

chas_raw <- read_rds("data/cb_7.rds")

chas_data <- cb_7 |> 
  filter(county == "Richmond city",
         tenure == "Renter",
         cost_burden != "No or negative income",
         cb_group == "Cost-burdened") |> 
  group_by(year) |>
  summarise(estimate = sum(estimate)) |> 
  mutate(change = estimate - lag(estimate),
         source = "chas")

cb_compare <- cb_renter |> 
  bind_rows(chas_data) |> 
  filter(year %in% 2016:2018)

test <- cb_7 |> 
  filter(county == "Richmond city",
         tenure == "Renter") |> 
  mutate(cost_burden = str_replace(cost_burden, "Severely cost-burdened", "Cost-burdened")) |> 
  group_by(year, cost_burden) |> 
  summarise(estimate = sum(estimate)) |>
  group_by(cost_burden) |> 
  mutate(change = estimate - lag(estimate)) |> 
  drop_na() |> 
  mutate(run_diff = cumsum(change))

ggplot(test,
       aes(x = year,
           y = change,
           fill = cost_burden)) +
  geom_col() +
  facet_wrap(vars(cost_burden), nrow = 1) +
  theme(legend.position = "none")

