library(tidyverse)
library(sf)
library(data.table)
library(vroom)

parcels <- vroom("regrid/regrid_va_all/va_statewide.csv")

filter_chunk <- function(chunk) {
  church_keywords <- c("church", "chapel", "parish", "diocese", "monastery", "convent", 
                       "temple", "synagogue", "mosque", "religious", "episcopal", "catholic", 
                       "lutheran", "baptist", "methodist", "presbyterian", "evangelical",
                       "congregation", "ministry", "spiritual", "faith")
  
  pattern <- paste(church_keywords, collapse = "|")
  
  chunk %>% filter(grepl(pattern, owner, ignore.case = TRUE))
}

library(future.apply)
library(progressr)

# Set up parallel processing
plan(multisession)

# Define chunk size
chunk_size <- 100000  # Adjust based on your system's capabilities

# Calculate number of chunks
n_chunks <- ceiling(nrow(parcels) / chunk_size)

# Process chunks in parallel with progress bar
with_progress({
  p <- progressor(steps = n_chunks)
  
  filtered_chunks <- future_lapply(1:n_chunks, function(i) {
    start <- (i - 1) * chunk_size + 1
    end <- min(i * chunk_size, nrow(df))
    chunk <- parcels[start:end, ]
    result <- filter_chunk(chunk)
    p()  # Update progress
    result
  })
})

# Combine results
filtered_df <- bind_rows(filtered_chunks)