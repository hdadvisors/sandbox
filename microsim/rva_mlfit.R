library(tidycensus)
library(tigris)
library(mlfit)
library(tidyverse)
library(sf)
library(qs)
options(tigris_use_cache = TRUE)




# Get seed population for Richmond region's surrounding counties
# Goochland, Powhatan, Western Hanover - 14501
# King William, New Kent, Charles City, Eastern Hanover - 08501


rva_counties_pums <- get_pums(
  variables = c("AGEP", "HINCP", "OCCP", "SEX", "JWTRNS", "PUMA"),
  state = "VA",
  year = 2023,
  survey = "acs5",
  variables_filter = list(TYPEHUGQ = "1"),
  recode = TRUE
) %>%
  filter(PUMA %in% c("14501", "08501"))

# Constraints: household income, age, general occupation
# Bring through all those characteristics along with sex, place of work
# Do it for Arlington (PUMAs 1301 and 1302)

# Get the seed population - omit GQ
arlington_pums <- get_pums(
  variables = c("AGEP", "HINCP", "OCCP", "SEX", "JWTRNS", "PUMA"),
  state = "VA",
  year = 2023,
  survey = "acs5",
  variables_filter = list(TYPEHUGQ = "1"),
  recode = TRUE
) %>%
  filter(PUMA %in% c("01301", "01302"))

# Get the constraint data
# First: age
age_vars <- paste0("DP05_00", str_pad(5:17, 2, "left", "0"))
names(age_vars) <- c(rep("1", 4), rep("2", 3), rep("3", 3), rep("4", 3))

arlington_age <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  variables = age_vars
) 

# Household income
income_vars <- paste0("DP03_00", 52:61)
names(income_vars) <- c(rep("1", 3), rep("2", 2), rep("3", 2), rep("4", 3))

arlington_income <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  variables = income_vars
) 

# Sex
arlington_sex <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  variables = c(male = "DP05_0002",
                female = "DP05_0003")
) 

# Occupation
occ_vars <- c(
  management = "DP03_0027",
  service = "DP03_0028",
  sales = "DP03_0029",
  natural_resources = "DP03_0030",
  production = "DP03_0031"
)

arlington_occupation <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  variables = occ_vars
) %>%
  select(-moe)

# Add in separate logic for not in labor force - it should be everyone else
arlington_occ_sum <- arlington_occupation %>%
  group_by(GEOID) %>%
  summarize(in_labor = sum(estimate, na.rm = TRUE)) %>%
  select(GEOID, in_labor)

arlington_no_labor <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  variables = "DP05_0001"
) %>%
  select(GEOID, NAME, variable, pop = estimate) %>%
  inner_join(arlington_occ_sum, by = "GEOID") %>%
  mutate(estimate = pop - in_labor, variable = "no_labor") %>%
  select(GEOID, NAME, variable, estimate)

arlington_occupation <- bind_rows(arlington_occupation, arlington_no_labor)

# Align seed categories with constraints
seed <- arlington_pums %>%
  transmute(
    hh_id = SERIALNO,
    person_id = paste0(hh_id, SPORDER),
    hh_wt = WGTP,
    age = case_when(
      AGEP < 20 ~ "1",
      AGEP < 45 ~ "2",
      AGEP < 65 ~ "3",
      TRUE ~ "4"
    ),
    income = case_when(
      HINCP < 25000 ~ "1",
      HINCP < 50000 ~ "2",
      HINCP < 100000 ~ "3",
      TRUE ~ "4"
    ),
    occupation = case_when(
      OCCP %in% c("0009", "9920") ~ "no_labor",
      OCCP < "3600" ~ "management",
      OCCP < "4700" ~ "service",
      OCCP < "6000" ~ "sales",
      OCCP < "7700" ~ "natural_resources",
      OCCP < "9800" ~ "production",
      TRUE ~ "no_labor"
    ),
    puma = PUMA,
    age_detailed = AGEP,
    sex = case_when(
      SEX == "1" ~ "male",
      TRUE ~ "female"
    ),
    hh_income = HINCP,
    occ_label = OCCP_label,
    workplace = JWTRNS_label
  )

# Use this code to filter out 0-population tracts, which will
# cause synthetic population generator to fail
arlington_tracts <- get_acs(
  geography = "tract",
  state = "VA",
  county = "Arlington",
  year = 2023,
  geometry = TRUE,
  variables = "B01001_001"
) %>%
  filter(estimate > 0)

# Map tracts to PUMAs
arlington_pumas1 <- pumas(state = "VA", cb = TRUE, year = 2020) %>%
  select(puma = PUMACE20) %>%
  filter(puma %in% c("01301", "01302"))

arlington_tract_geom <- select(arlington_tracts, GEOID, geometry)

geo_hierarchy <- arlington_tract_geom %>%
  select(tract = GEOID) %>%
  st_centroid() %>%
  st_join(arlington_pumas1) %>%
  st_drop_geometry() %>%
  na.omit()

# Set up the constraints with some sense checks
age_cons <- arlington_age %>%
  group_by(GEOID, variable) %>%
  summarize(n = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(age = variable, tract = GEOID) %>%
  filter(tract %in% geo_hierarchy$tract)

assertthat::assert_that(length(unique(age_cons$tract)) == nrow(geo_hierarchy))

income_cons <- arlington_income %>%
  group_by(GEOID, variable) %>%
  summarize(n = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(income = variable, tract = GEOID) %>%
  filter(tract %in% geo_hierarchy$tract)

assertthat::assert_that(length(unique(income_cons$tract)) == nrow(geo_hierarchy))

occp_cons <- arlington_occupation %>%
  group_by(GEOID, variable) %>%
  summarize(n = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(occupation = variable, tract = GEOID) %>%
  filter(tract %in% geo_hierarchy$tract)

assertthat::assert_that(length(unique(occp_cons$tract)) == nrow(geo_hierarchy))

assertthat::are_equal(sum(occp_cons$n, na.rm = TRUE), sum(age_cons$n, na.rm = TRUE))


sex_cons <- arlington_sex %>%
  select(sex = variable, tract = GEOID, n = estimate) %>%
  filter(tract %in% geo_hierarchy$tract)

assertthat::are_equal(sum(sex_cons$n, na.rm = TRUE), sum(age_cons$n, na.rm = TRUE))



# Set up the fitting problem
# arlington_simulation <- ml_problem(
#   ref_sample = seed,
#   controls = list(
#     individual = list(age_cons, occp_cons),
#     group = list(income_cons)
#   ),
#   field_names = special_field_names(
#     groupId = "hh_id",
#     individualId = "person_id",
#     count = "n",
#     zone = "tract",
#     region = "puma"
#   ), 
#   geo_hierarchy = geo_hierarchy
# )

# dallas_fits <- arlington_simulation %>%
#   map(ml_fit, algorithm = "ipu", maxiter = 2000, .progress = TRUE)

# # Convergence?
# table(map_lgl(arlington_fits, function(x) x$success))

# With household weights
arlington_simulation <- ml_problem(
  ref_sample = seed,
  controls = list(
    individual = list(age_cons, occp_cons),
    group = list(income_cons)
  ),
  field_names = special_field_names(
    groupId = "hh_id",
    individualId = "person_id",
    count = "n",
    zone = "tract",
    region = "puma",
    prior_weight = "hh_wt"
  ), 
  geo_hierarchy = geo_hierarchy
)

arlington_fits <- arlington_simulation %>%
  map(ml_fit, algorithm = "ipu", maxiter = 2000, .progress = TRUE)

table(map_lgl(arlington_fits, function(x) x$success))


# write_rds(dallas_fits, "data/dallas_sim_dec22.rds")


# dallas_fits <- read_rds("data/dallas_sim_dec22.rds")
# We need to determine which have weights that can be replicated
synthetic_pop <- map_dfr(arlington_fits, ~{
  sum_weights <- sum(.x$flat_weights, na.rm = TRUE)
  
  if (sum_weights > 0) {
    ml_replicate(.x, algorithm = "trs")
  }
  
}, .id = "tract")

# Check professions in a given neighborhood
professions <- synthetic_pop %>%
  filter(tract == "51013100300") %>%
  group_by(occ_label) %>%
  summarize(n = n())



# If rolling up to the tract level, it makes more sense to sum over the fractional weights
# Integerization is necessary (with `ml_replicate()` to create synthetic agents); these 
# sums won't perfectly reflect the population due to rounding



