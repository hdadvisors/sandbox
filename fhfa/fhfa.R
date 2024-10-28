library(tidyverse)
library(janitor)

nmdb <- read_csv("fhfa/nmdb-outstanding-mortgage-statistics-states-quarterly.csv")

dd <- read_csv("fhfa/nmdb-data-dictionary.csv")

nmdb_va <- nmdb |> 
  filter(GEOID == "VA") |> 
  left_join(dd) |> 
  select(
    PERIOD,
    MARKET,
    SERIESID,
    CATEGORY,
    LABEL,
    DETAILS,
    val_loan = VALUE1,
    val_volume = VALUE2
  ) |> 
  clean_names() |> 
  mutate(
    date = parse_date_time(period, "Yq"),
    .before = 1
  )
  

nmdb_va |> 
  filter(
    market == "All Mortgages",
    seriesid %in% c("TOT_LOANS", "AVE_INTRATE")
  ) |> 
  pivot_wider(
    name_from = seriesid,
    values_from = val_loan
  )
  ggplot(aes(x = date, color = seriesid)) +
  geom_line(aes(y = val_loan)) +
  scale_y_continuous(sec.axis = (~.*1000))

  
