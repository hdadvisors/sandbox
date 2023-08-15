library(tidyverse)
library(scales)
library(hdatools)
library(readxl)

adu_raw <- read_xlsx("sandbox/jcc/va-adu-ratios.xlsx")

## Data 1 -------------------

adu_1 <- adu_raw |> 
  pivot_wider(
    names_from = limit,
    values_from = value
  ) |> 
  mutate(
    code = str_replace_all(
      code,
      ": ",
      ":\n"
      )
  )

## Data 2 -------------------

adu_2 <- adu_raw |> 
  pivot_wider(
    names_from = ratio,
    values_from = value,
  ) |> 
  select(
    1, 2, density = 3, affordable = 4
  ) |> 
  mutate(
    code = str_replace_all(
      code,
      ": ",
      ":\n"
    )
  )
    
## Plot 1 -------------------

ggplot(adu_1,
       aes(x = Min,
           xend = Max,
           y = ratio,
           yend = ratio,
           color = ratio,
           )
       ) +
  geom_segment(
    size = 8
  ) +
  geom_text(
    aes(
      x = Min + 0.01,
      label = label_percent()(Min)
    ),
    size = 3,
    color = "white",
    fontface = "bold",
    hjust = 0
  ) +
  geom_text(
    aes(
      x = Max - 0.01,
      label = paste0(
        as.character(Max*100), "%"
        )
    ),
    size = 3,
    color = "white",
    fontface = "bold",
    hjust = 1
  ) +
  facet_wrap(
    ~fct_rev(code),
    ncol = 1,
    strip.position = "left"
    ) +
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_color_hfv(-1) +
  theme_hfv() +
  flip_gridlines() +
  labs(
    #title = "Allowable ADU program design limits in Virginia state code",
    subtitle = "Minimum and maximum <span style='color:#011e41;'>**density bonus**</span> and <span style='color:#40c0c0;'>**affordable unit**</span> percentages for ADU programs permitted by Virginia state code" 
  ) +
  theme(
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 0
      )
  )

## Plot 2 -------------------

ggplot(adu_2,
       aes(
         x = affordable,
         y = density,
         color = code,
         fill = code
       )) +
  geom_line(linewidth = 1.5) +
  geom_area(alpha = 0.2) +
  geom_ribbon(
    aes(
      xmin = 0,
      xmax = affordable
    ),
    alpha = 0.2,
    linewidth = 0
  ) +
  facet_wrap(
    ~fct_reorder(
      code,
      density
      )
    ) +
  scale_color_hfv(-1) +
  scale_fill_hfv(-1) +
  scale_x_continuous(
    labels = label_percent(accuracy = 0.1),
    breaks = c(0.05, 0.1, 0.17, 0.35),
    limits = c(0,0.4),
    expand = expansion(mult = c(0,0))
    ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    breaks = c(0.2, 0.3, 0.575, 0.95),
    limits = c(0,1),
    expand = expansion(mult = c(0,0))
  ) +
  add_zero_line("x") +
  add_zero_line("y") +
  coord_fixed(ratio = 0.5) +
  labs(
    title = "Allowable ADU program design limits in Virginia state code",
    subtitle = "Minimum and maximum density bonus and affordable unit percentages",
    x = "\nAffordable units",
    y = "Density bonus\n",
    caption = "**Source:** Va. Code Ann. § 15.2-2305 and § 15.2-2305.1."
  ) +
  theme_hfv() +
  theme(
    axis.title = element_text(),
    #axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_line(color = "#cbcdcc", size = 0.05)
  )
