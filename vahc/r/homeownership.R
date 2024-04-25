library(tidyverse)
library(tidycensus)


# Data collection

# Create an object for all years.
years <- 2010:2023

# Create an object for all tables needed. This includes a table that aggregates 
# to total housing units.
b25003 <- c("B25003", paste0("B25003", LETTERS[2:9]))

# Create a function to convert variable to race or ethnicity variable.
concept_to_race <- function(x) {
  out <- x %>%
    str_remove_all("HOUSEHOLDER") %>%
    str_remove_all("TENURE \\(|\\)") %>%
    str_to_title() %>%
    str_replace_all("And", "and") %>%
    str_replace_all("Or", "or") %>%
    str_remove_all(" Alone")
  
  out
}

# Pull the table variables, excluding Puerto Rico.
b25003_defns <- load_variables(2021, "acs5") %>%
  filter(str_sub(name, end = 6) %in% b25003) %>% 
  filter(str_detect(name, "PR") == FALSE)


# Clean the variables provided by Census API and separate needed variables into
# appropriate columns.
b25003_cleaned <- b25003_defns %>%
  mutate(race = concept_to_race(concept)) %>%
  separate(label, into = c("est", "total", "tenure"), sep = "!!") %>% 
  select(variable = name, race, tenure) %>% 
  mutate(across(.fns = ~replace_na(.x, "All")),
         across(.fns = ~str_remove_all(.x, ":")),
         across(.fns = ~str_remove_all(.x, " --")),
         across(.fns = ~str_replace_all(.x, "total", "All")),
         across(.fns = ~str_replace_all(.x, "Tenure", "All")))



output_b25003_state <- map_dfr(b25003, function(tb) {
  yearly_data <- map_dfr(years, function(yr) {
    
    acs_pull <- get_acs(
      geography = "state",
      state = "VA",
      table = tb,
      year = yr
    ) %>%
      left_join(b25003_cleaned, by = "variable")
    
    acs_rearranged <- acs_pull %>%
      mutate(year = yr) %>%
      select(variable, year, locality = NAME, fips = GEOID, race, tenure,
             estimate, moe)
    
    acs_rearranged
  })
  yearly_data
})



# Data Prep
# The previous data output makes it difficult to easily calculate dynamic 
# homeownership rates if wanting to combine multiple localities or races. In
# order to create these calculations more easily, unpivot the columns to rows
# utilizing the pivot_wider function.
# 


output_b25003_wide_state <- output_b25003_state %>% 
  select( -c(variable)) %>% 
  pivot_wider(names_from = tenure,
              values_from = c(estimate, moe)) %>% 
  select(year:race, est_all = estimate_All, est_owner = "estimate_Owner occupied",
         est_renter = "estimate_Renter occupied", moe_all = moe_All, 
         moe_owner = "moe_Owner occupied",
         moe_renter = "moe_Renter occupied")

# Clean the locality column to remove comma and Virginia from data, as well as
# converting Bedford city (51515) to Bedford County (51019).




# Data export
# 
# Write to rds.

write_rds(output_b25003_wide_state, "vahc/data/b25003_state.rds")


# Data viz 

library(hdatools)
library(scales)
library(ggrepel)

# Pre-Pandemic Plot

homeownership <- read_rds("vahc/data/b25003_state.rds") |> 
  filter(year <= 2019) |> 
  filter(race != "All") |> 
  mutate(race = case_when(
    race == "American Indian and Alaska Native " ~ "Another Race",
    race == "Native Hawaiian and Other Pacific Islander " ~ "Another Race",
    race == "Some Other Race " ~ "Another Race",
    race == "Two or More Races " ~ "Multiracial",
    TRUE ~ race)) |> 
  group_by(year, race) |> 
  summarise(est_all = sum(est_all),
            est_owner = sum(est_owner)) |> 
  mutate(rate = est_owner/est_all) |> 
  group_by(race) |> 
  mutate(pct_change = (rate/lag(rate) -1) * 100) |> 
  mutate(year = as.character(year))

data_ends <- homeownership |> 
  filter(year == 2019)

pre_plot <- ggplot(homeownership,
       aes(x = year, 
           y = rate,
           color = race)) +
  geom_line(group = 1, linewidth = 1) +
  geom_point(size = 4) +
  theme_hfv(base_size = 15) +
  facet_wrap(~reorder(race, -rate), 
             nrow = 1, 
             labeller = label_wrap_gen(10)) +
  scale_color_hfv() +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 90))


pre_plot +
  geom_text_repel(
    aes(label = percent(rate)), data = data_ends,
    fontface ="plain", color = "black", size = 5,
    nudge_y = 0.2
  )

# Post-Pandemic Plot


homeownership <- read_rds("vahc/data/b25003_state.rds") |> 
  filter(race != "All") |> 
  mutate(race = case_when(
    race == "American Indian and Alaska Native " ~ "Another Race",
    race == "Native Hawaiian and Other Pacific Islander " ~ "Another Race",
    race == "Some Other Race " ~ "Another Race",
    race == "Two or More Races " ~ "Multiracial",
    TRUE ~ race)) |> 
  group_by(year, race) |> 
  summarise(est_all = sum(est_all),
            est_owner = sum(est_owner)) |> 
  mutate(rate = est_owner/est_all) |> 
  group_by(race) |> 
  mutate(pct_change = (rate/lag(rate) -1) * 100) |> 
  mutate(year = as.character(year))


data_ends <- homeownership |> 
  filter(year == 2023)

ggplot(homeownership,
       aes(x = year, 
           y = pct_change,
           fill = race)) +
  geom_col() +
  facet_wrap(~race, nrow = 1) +
  theme_hfv() +
  scale_fill_hfv() +
  theme(legend.position = "none")
         