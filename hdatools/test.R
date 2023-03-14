library(tidyverse)
library(hdatools)
library(scales)
library(ggiraph)

set.seed(1983)
a <- rnorm(1000)
b <- rnorm(1000)
category <- sample(letters[1:4], 1000, replace = TRUE)

x <- tibble(a, b, category)

ggplot(x, aes(a, b, color = category)) +
  geom_point() +
  theme_pha() +
  scale_color_pha() +
  labs(title = "Here is the title",
       subtitle = "Here is the subtitle",
       caption = "Source: Here is the source.")

pha_names <- c("Richmond city", "Hanover County", "Henrico County", "Chesterfield County")

pha_comp_change <- read_rds("C:/Users/yates/Documents/repos/rrh-framework/data/rr_comp_change.rds") |>
  filter(NAMELSAD %in% pha_names)

comp_chg_summary <- pha_comp_change |>
  group_by(year, component) |>
  summarise(value = sum(value)) |>
  mutate(year = as.numeric(year),
         percent = value/sum(value)) |>
  filter(year >= 2016 & year != 2020) |>
  mutate(year = as.character(year)) |>
  mutate(component = fct_relevel(component,
                                 levels = c("Domestic migration",
                                            "International migration",
                                            "Natural increase")))

g <- ggplot(comp_chg_summary,
       aes(x=year,
           y=percent,
           fill=component,
           data_id = percent,
           tooltip = percent(percent))) +
  geom_col(position = "stack") +
  labs(title="Components of population change",
       subtitle = "Share of regional population growth by source",
       y = "Percent",
       caption = "**Note:** Data not available for 2020.<br>Source: U.S. Census Bureau, Population Estimates Program.") +
  scale_fill_pha() +
  scale_y_continuous(labels = percent_format()) +
  theme_pha() +
  theme(axis.title.y = element_text()) +
  geom_col_interactive(position = "stack",
                       size = 2)

girafe(ggobj = g,
       height_svg = 4)
