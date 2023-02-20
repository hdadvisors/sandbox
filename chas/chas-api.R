# Collect households by tenure and AMI data from HUD CHAS

library(tidyverse)
library(hudr)

# Set HUD API key

api_token <- Sys.getenv("HUD_API_KEY")

# Set years

years <- c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014",
           "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")

years <- c("2014-2018")

# Load in CHAS data dictionary

chas_vars <- read_csv("chas-api-vars.csv")

# Import FIPS codes for localities

locality_ids <- get_hud_chas_entityid_list(
    stateid = "51",
    geo_lvl = "county",
    hud_key = api_token) |> 
  filter(countyname != "Bedford city")

chas_output <- map(locality_ids$entity_id, function(ids) {
  
  yearly_data <- map(years, function(yr) {
    
    chas_pull <- get_hud_chas_data(
      entityid = ids,
      stateid = "51",
      type = "3",
      yr = yr,
      hud_key = api_token
    )
    
    chas_pull 
    
  }, .progress = TRUE)
  
  yearly_data
  
}, .progress = TRUE)

chas_bind <- chas_output |> list_rbind()


chas_output <- map(c("1", "3"), function(ids) {
  
  chas_pull <- get_hud_chas_data(
    entityid = ids,
    stateid = "51",
    type = "3",
    yr = "2015-2019",
    hud_key = api_token
  )
  
  chas_pull
  
}) |> list_rbind()

chas_test <- get_hud_chas_data(
  entityid = "89",
  stateid = "51",
  type = "3",
  yr = "2014-2018",
  hud_key = api_token
)

chas_join <- chas_test |> 
  left_join(chas_vars, by = "variable")

chas_rearranged <- chas_join |> 
  select(year, locality = geoname, variable, topic, tenure, ami, detail, value) |> 
  mutate(locality = str_remove_all(locality, ", Virginia"))
