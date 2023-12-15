library(tidyverse)
library(tidycensus)
library(hdatools)
library(scales)

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
