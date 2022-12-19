library(tidyverse)
library(hdatools)
library(ggtext)

set.seed(1983)
a <- rnorm(1000)
b <- rnorm(1000)
category <- sample(letters[1:8], 1000, replace = TRUE)

x <- tibble(a, b, category)

gg <- x |> filter(b > 0) |>
ggplot(aes(b, category, fill = category)) +
  geom_col() +
  labs(
    title = "Here is a longer title that tells a story",
    subtitle = "Here is a longer subtitle that describes the data",
    caption = "**Source:** Here is a source.<br>**Note:** Here is a note."
  ) +
  theme_hfv(base_size = 12) +
  scale_fill_hfv() +
  flip_gridlines() +
  add_zero_line("x")

gg

