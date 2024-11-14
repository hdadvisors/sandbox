library(tidyverse)
library(readxl)

nhpd <- read_xlsx("wppdc/nhpd-active-inconclusive-va-2024-02-06.xlsx")

nhpd_pcs <- nhpd |> 
  filter(str_detect(ManagerName, "(?i)piedmont community services"))

cols <- c("Owner", "ManagerName")

nhpd_csb <- nhpd |> 
  filter(if_any(cols, ~ str_detect(.x, "(?i)community services board|csb")))
