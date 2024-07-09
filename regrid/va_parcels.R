# Load required packages
library(dplyr)
library(purrr)
library(readr)
library(fs)

folder <- "regrid/regrid_va_csv"

# Get a list of all zip files in the directory
zip_files <- dir_ls(path = folder, regexp = "\\.zip$")


# Function to extract csv from a zip file and read it
extract_and_read_csv <- function(zip_file) {
  temp_dir <- tempdir()
  unzip(zip_file, exdir = temp_dir)
  csv_file <- dir_ls(temp_dir, regexp = "\\.csv$")
  
  # Read the CSV file with all columns as character type
  df <- read_csv(csv_file, col_types = cols(.default = "c"))
  
  # Add the source file name as a column
  df$source_file <- basename(zip_file)
  
  unlink(temp_dir, recursive = TRUE)
  return(df)
}

# Extract and read all csv files
all_data <- map(zip_files, safely(extract_and_read_csv))

# Combine the successfully read data frames
valid_data <- all_data %>%
  map(~.$result)

# Get all unique column names
all_columns <- valid_data %>%
  map(names) %>%
  unlist() %>%
  unique()

# Function to add missing columns and select all columns
standardize_columns <- function(df) {
  df %>%
    add_column(!!!setNames(rep(list(NA_character_), length(all_columns) - ncol(df)), 
                           setdiff(all_columns, names(df)))) %>%
    select(all_of(all_columns))
}

# Standardize and combine all data frames
combined_data <- valid_data %>%
  map(standardize_columns) %>%
  bind_rows()

# Check for any errors
errors <- all_data %>%
  keep(~!is.null(.$error)) %>%
  map(~.$error)

if (length(errors) > 0) {
  warning("Some files could not be read. Check the 'errors' variable for details.")
}