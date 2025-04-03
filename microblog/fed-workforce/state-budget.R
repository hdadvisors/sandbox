library(tidyverse)
library(readxl)
library(gt)

top5 <- c(
  "51013",
  "51059",
  "51153",
  "51107",
  "51810"
)

tax_inc <-  read_xlsx(
  "microblog/fed-workforce/va_budget.xlsx",
  sheet = "table 1-7"
  ) |> 
  mutate(
    top = case_when(
      fips %in% top5 ~ "top5",
      .default = "not top5"
    )
  )

tax_inc_sum <- tax_inc |> 
  summarise(
    tax = sum(total_tax_liability),
    .by = top
  ) |> 
  mutate(pct = tax/sum(tax), type = "inc")

tax_sales <-  read_xlsx(
  "microblog/fed-workforce/va_budget.xlsx",
  sheet = "table 4-3"
) |> 
  mutate(
    top = case_when(
      fips %in% top5 ~ "top5",
      .default = "not top5"
    )
  )

tax_sales_sum <- tax_sales |> 
  summarise(
    tax = sum(share_state_tax),
    .by = top
  ) |> 
  mutate(pct = tax/sum(tax), type = "sales")

tax_reco <-  read_xlsx(
  "microblog/fed-workforce/va_budget.xlsx",
  sheet = "table 5-5"
) |> 
  mutate(
    top = case_when(
      fips %in% top5 ~ "top5",
      .default = "not top5"
    )
  )

tax_reco_sum <- tax_reco |> 
  summarise(
    tax = sum(recordation_tax),
    .by = top
  ) |> 
  mutate(pct = tax/sum(tax), type = "reco")

tax_sum <- bind_rows(tax_inc_sum, tax_sales_sum, tax_reco_sum) |> 
  summarise(
    tax = sum(tax),
    .by = top
  ) |> 
  mutate(pct = tax/sum(tax))


