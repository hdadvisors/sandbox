library(tidyverse)
library(hdatools)
library(ggtext)

set.seed(1983)
a <- rnorm(1000)
b <- rnorm(1000)
category <- sample(letters[1:8], 1000, replace = TRUE)

x <- tibble(a, b, category)

scale_fill_gradient_pha <- function(...,
                                    colors = c("#5bab8e","#a6cccc","#f39152","#be451c"),
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "#e2e4e3",
                                    guide = "colorbar") {
  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "pha",
    palette = scales::gradient_n_pal(colors, values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


gg <- x |> filter(b > 0) |>
ggplot(aes(b, category, fill = b)) +
  geom_col() +
  labs(
    title = "Here is the title",
    subtitle = "Here is the subtitle",
    caption = "**Source:** Here is a source.<br>**Note:** Here is a note."
  ) +
  theme_pha() +
  scale_fill_gradient_pha(colors = c("#5bab8e", "#a6cccc")) +
  flip_gridlines() +
  add_axis_line(axis = "x")

gg +
  ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggplot2::theme(
    axis.line.y = element_line(color = "#383c3d", size = 0.5)
  )


  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    axis.line.y = element_line(color = "#383c3d", size = 0.5)
    ) +
  flip_gridlines()

add_axis_line <- function(axis = c("x", "y")) {

  if(axis == "x") {

    list(
      ggplot2::scale_x_continuous(expand = expansion(mult = c(0, 0.05))),
      ggplot2::theme(
        axis.line.y = element_line(color = "#383c3d", size = 0.5)
      )
    )

    } else {

    list(
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.05))),
      ggplot2::theme(
        axis.line.x = element_line(color = "#383c3d", size = 0.5)
      )
    )
  }

}

call_top <- read_csv("C:/Users/Jonathan/Documents/repos/rrh-framework/data/hrl_need.csv") |>
  mutate(need = case_when(
    need == "Other - specify in \"Issues\" tab" ~ "Other",
    TRUE ~ need
  )) |>
  transform(percent = ave(vol,FUN = prop.table)) |>
  filter(percent > 0.01)

ggplot(call_top,
                   aes(y = reorder(need, vol),
                       x = percent,
                       data_id = percent,
                       tooltip = percent_format()(percent))) +
  geom_col(fill = "#5bab8e") +
  geom_col_interactive(fill = "#5bab8e", size = 2) +
  add_axis_line(axis = "x") +
  scale_x_continuous(labels = label_percent()) +
  theme_pha() + 
  labs(title = "Housing Resource Line volume by call topic",
       subtitle = "September 2020 through October 2022",
       caption = "**Note:** Does not include topics below 1 percent of call volume.<br>**Source:** Partnership for Housing Affordability.") +
  flip_gridlines()
