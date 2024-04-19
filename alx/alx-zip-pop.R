library(tidyverse)
library(tidycensus)
library(srvyr)
library(hdatools)
library(scales)

# Population by ZIP code

zips <- c(22206, 22301, 22302, 22304, 22305, 22311, 22314)

pop <- get_acs(
  geography = "zcta",
  variables = "B01003_001",
  #state = "VA",
  zcta = zips,
  year = 2021,
  survey = "acs5"
)

pop_pct <- pop |> 
  mutate(pct = estimate/sum(estimate))

ggplot(pop_pct, aes(x = pct, y = reorder(GEOID, pct))) +
  geom_col(fill = "#445ca9") +
  geom_text(
    aes(label = label_percent(accuracy = 1)(pct)),
    color = "white",
    nudge_x = -0.02
    ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.05))
    ) +
  theme_hda() +
  flip_gridlines() +
  labs(
    title = "Percent of population by ZIP code",
    subtitle = "City of Alexandria",
    caption = "**Source:** U.S. Census Bureau, 2017-2021 American Community Survey, 5-year estimates."
  ) +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Household size by bedrooms in unit

alx_pums <- get_pums(
  variables = c("NP", "BDSP", "HUPAC", "HINCP"),
  state = "VA",
  puma = "51255",
  year = 2021,
  survey = "acs5",
  variables_filter = list(SPORDER = 1, BDSP = (0:6), TEN = 3),
  rep_weights = "housing"
)

va_pums <- get_pums(
  variables = c("PUMA20", "NP", "BDSP", "HINCP"),
  state = "VA",
  #puma = "51000",
  year = 2022,
  survey = "acs5",
  variables_filter = list(SPORDER = 1, BDSP = (0:6), TEN = 3),
  rep_weights = "housing"
)

alx_pums <- filter(alx_pums, HINCP < 100000)

alx_srv <- to_survey(alx_pums, type = "housing")

alx_np_bdsp <- alx_srv |> 
  group_by(NP, BDSP) |> 
  summarise(
    n = survey_total(vartype = "cv")
  )

alx_dist <- alx_np_bdsp |> 
  filter(
    BDSP < 4
    ) |> 
  group_by(BDSP) |> 
  mutate(pct = n/sum(n)) |> 
  ungroup() |> 
  mutate(
    NP = case_when(
      NP == 1 ~ "1 person",
      NP == 2 ~ "2 persons",
      NP == 3 ~ "3 persons",
      NP == 4 ~ "4 persons",
      NP == 5 ~ "5 persons",
      .default = "6+ persons"
    )
  ) |> 
  mutate(
    BDSP = case_when(
      BDSP == 0 ~ "Studio",
      BDSP == 1 ~ "1 bedroom",
      BDSP == 2 ~ "2 bedroom",
      BDSP == 3 ~ "3 bedroom"
    )
  )
  
ggplot() +
  geom_col(
    data = alx_dist,
    aes(word(NP, 1), pct, fill = BDSP),
    position = "stack"
    ) +
  geom_text(
    data = filter(alx_dist, pct > 0.05),
    aes(word(NP, 1), pct, label = label_percent(accuracy = 1)(pct)),
    nudge_y = -0.02,
    color = "white"
    ) +
  facet_grid(. ~ fct_relevel(BDSP, "Studio"), scales = "free", space = "free") +
  scale_y_continuous(
    labels = label_percent(),
    expand = expansion(mult = c(0.01, 0.02)),
    limits = c(0, 0.85)
    ) +
  scale_fill_hda() +
  theme_hda() +
  add_zero_line() +
  labs(
    title = "Household size by number of bedrooms in unit",
    subtitle = "All renters in the City of Alexandria with incomes below $100,000",
    x = "Number of persons in household",
    caption = "**Source:** U.S. Census Bureau, 2017-2021 American Community Survey Public Use Microdata Sample, 5-year estimates."
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)))

ggplot() +
  geom_col(
    data = alx_dist,
    aes(word(NP, 1), n, fill = BDSP),
    position = "stack"
  ) +
  #geom_text(
  #  data = filter(alx_dist, pct > 0.05),
  #  aes(word(NP, 1), pct, label = label_percent(accuracy = 1)(pct)),
  #  nudge_y = -0.02,
  #  color = "white"
  #) +
  facet_grid(. ~ fct_relevel(BDSP, "Studio"), scales = "free", space = "free") +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.01, 0.02)),
    limits = c(0, 8200)
  ) +
  scale_fill_hda() +
  theme_hda() +
  add_zero_line() +
  labs(
    title = "Household size by number of bedrooms in unit",
    subtitle = "All renters in the City of Alexandria with incomes below $100,000",
    x = "Number of persons in household",
    y = "Households",
    caption = "**Source:** U.S. Census Bureau, 2017-2021 American Community Survey Public Use Microdata Sample, 5-year estimates."
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
    )

# For HHs with at least one child

alx_srv <- to_survey(alx_pums, type = "housing")

alx_child_srv <- alx_srv |> 
  group_by(NP, BDSP, HUPAC) |> 
  summarise(
    n = survey_total()
  ) |> 
  filter(!HUPAC == "4")

alx_child_dist <- alx_child_srv |> 
  filter(BDSP < 4) |> 
  group_by(BDSP) |> 
  mutate(pct = n/sum(n)) |> 
  ungroup() |> 
  mutate(
    NP = case_when(
      NP == 2 ~ "2 persons",
      NP == 3 ~ "3 persons",
      NP == 4 ~ "4 persons",
      NP == 5 ~ "5 persons",
      .default = "6+ persons"
    )
  ) |> 
  mutate(
    BDSP = case_when(
      BDSP == 0 ~ "Studio",
      BDSP == 1 ~ "1 bedroom",
      BDSP == 2 ~ "2 bedroom",
      BDSP == 3 ~ "3 bedroom",
      BDSP == 4 ~ "4 bedroom"
    )
  )

ggplot() +
  geom_col(
    data = alx_child_dist,
    aes(word(NP, 1), n, fill = BDSP),
    position = "stack"
  ) +
  #geom_text(
  #  data = filter(alx_dist, pct > 0.05),
  #  aes(word(NP, 1), pct, label = label_percent(accuracy = 1)(pct)),
  #  nudge_y = -0.02,
  #  color = "white"
  #) +
  facet_grid(. ~ fct_relevel(BDSP, "Studio"), scales = "free", space = "free") +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.01, 0.02)),
    limits = c(0, 1900)
  ) +
  scale_fill_hda() +
  theme_hda() +
  add_zero_line() +
  labs(
    title = "Household size by number of bedrooms in unit",
    subtitle = "All renters **with children** in the City of Alexandria with incomes below $100,000",
    x = "Number of persons in household",
    y = "Households",
    caption = "**Source:** U.S. Census Bureau, 2017-2021 American Community Survey Public Use Microdata Sample, 5-year estimates."
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
