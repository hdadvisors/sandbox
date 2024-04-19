library(tidyverse)
library(tidycensus)

va_co_pop <- get_acs(
  geography = "county",
  variables = "B01001_001",
  state = "VA"
)

nova <- c(
  "51510",
  "51013",
  "51600",
  "51059",
  "51610",
  "51107",
  "51683",
  "51685",
  "51153"
)

hr <- c(
  "51550",
  "51620",
  "51073",
  "51650",
  "51093",
  "51095",
  "51700",
  "51710",
  "51735",
  "51740",
  "51175",
  "51800",
  "51181",
  "51810",
  "51830",
  "51199"
)

region_pop <- va_co_pop |> 
  mutate(
    region = case_when(
      GEOID %in% nova ~ "NOVA",
      GEOID %in% hr ~ "HR",
      .default = "Rest of state"
    )
  ) |> 
  summarise(
    pop = sum(estimate),
    .by = region
  ) |> 
  mutate(
    pct = pop/sum(pop)
  )
