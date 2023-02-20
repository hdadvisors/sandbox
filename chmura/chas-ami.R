# Get household by income data from CHAS

library(tidyverse)
library(httr)
library(glue)
library(readxl)
library(janitor)

years <- 2012:2019

sumlev <- "050"

dir.create(glue("{sumlev}"))

walk(years, ~{
  
  url <- glue("https://www.huduser.gov/PORTAL/datasets/cp/{.x - 4}thru{.x}-{sumlev}-csv.zip")
  
  file <- basename(url)
  
  path <- file.path(sumlev, file)
  
  if (!file.exists(path)) {
    GET(url, write_disk(path, overwrite = TRUE), progress(type = "down"))
  }
  
  print(glue("Unzipping {.x}..."))
  unzip(path, exdir = file.path(sumlev, .x))
  
})

tables <- "8"

chas_data <- map_df(tables, function(table) {
  
  mytable <- map_df(years, function(year) {
    
    # Identify the year-specific folder
    path <- file.path("050", year)
    
    # Find the file - it may be buried so use recursive = TRUE
    file <- list.files(path, pattern = glue("Table{table}.csv"), recursive = TRUE)
    
    # Read in the file quietly
    raw <- read_csv(file.path(path, file), col_types = cols())
    
    # Clean it up
    cleaned <- raw %>%
      clean_names() %>%
      mutate(fips = substr(geoid, 8, 12)) %>%
      separate(name, into = c("county", "state"), sep = ",") %>%
      filter(st == "51") %>%
      pivot_longer(starts_with("T"), 
                   names_to = "code",
                   values_to = "value") %>%
      mutate(id = str_extract(code, "\\d+$"),
             type = str_extract(code, "est|moe")) %>%
      select(-code) %>%
      pivot_wider(names_from = type, values_from = value) %>%
      rename(Estimate = est, MOE = moe) %>%
      mutate(Code := glue("T{table}_est{id}"),
             Year = year) %>%
      select(Code, Year, Estimate, MOE, everything(), -id)%>%      
      mutate(fips = case_when(
        fips == "51515" ~ "51019",
        TRUE ~ fips
      )) %>%
      mutate(county = case_when(
        county == "Bedford city" ~ "Bedford County",
        TRUE ~ county
      ))
    
    # Account for different data dictionaries
    # Find the data dictionary in the appropriate folder
    dict_path <- list.files(path, pattern = "dictionary", recursive = TRUE, full.names = TRUE) 
    
    # Read in the data dictionary and merge
    dict <- read_excel("050/2019/050/CHAS data dictionary 15-19.xlsx", 
                       sheet = glue("Table {table}"))
    
    cleaned_with_dict <- cleaned %>%
      left_join(dict, by = c("Code" = "Column Name"))
    
    cleaned_with_dict
    
  }) 
  
  mytable
  
})

chas_clean <- chas_data |> 
  select(year = Year, period = source, county, fips, tenure = Tenure,
         income = 'Household income', cost_burden = 'Cost burden',
         facilities = Facilities, estimate = Estimate) |> 
  filter(facilities == "All",
         income != "All",
         cost_burden != "All") |> 
  select(-facilities)

write_csv(chas_clean, "chas-ami-cb.csv")
