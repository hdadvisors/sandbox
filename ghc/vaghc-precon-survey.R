library(tidyverse)
library(hdatools)
library(ggtext)
library(scales)

survey_raw <- read_csv("ghc/vaghc-precon-survey.csv") |> 
  select(-6)

qs <- str_remove(colnames(survey_raw), "Question \\d+: ")

q_labs <- data.frame(
  q = paste0("q", 2:6),
  lab = qs
)

survey <- survey_raw |> 
  select(
    q2 = 1,
    q3 = 2,
    q4 = 3,
    q5 = 4, 
    q6 = 5
  ) |> 
  pivot_longer(
    cols = everything(),
    names_to = "q",
    values_to = "score"
  ) |> 
  mutate(score = as.character(score)) |> 
  summarise(
    n = n(),
    .by = c(q, score)
  ) |> 
  drop_na() |> 
  mutate(
    pct = n/sum(n),
    .by = q
  ) |> 
  left_join(q_labs)

score_cols <- c(
  "5" = "#3f5179",
  "4" = "#8c92a9",
  "3" = "#d2d2d2",
  "2" = "#e1987c",
  "1" = "#d35223"
)

score_labs <- c(
  "Highly likely",
  "Likely",
  "Unsure",
  "Unlikely",
  "Highly unlikely"
)

survey |> 
  ggplot(
    aes(
      x = pct,
      y = lab,
      fill = fct_rev(score),
      label = label_percent(1)(pct),
      group = score
    )
  ) +
  geom_col(width = 1) +
  geom_text(
    data = . %>% filter(pct > 0.05),
    position = position_stack(vjust = 0.5),
    size = 6,
    color = "white"
  ) +
  facet_wrap(~fct_rev(str_wrap(lab, 80)), ncol = 1, scales = "free_y") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = score_cols, labels = score_labs) +
  labs(
    title = "2024 VAGHC Pre-Conference Session",
    subtitle = "Attendee survey results",
    caption = "Total responses collected: 63"
  ) +
  theme_minimal(
    base_size = 16,
    base_family = "Open Sans"
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.margin = margin(t = 5, b = -10),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 15, hjust = 0, margin = margin(t = 3, b = 2)),
    plot.caption = element_text(face = "italic", hjust = 0)
  )
