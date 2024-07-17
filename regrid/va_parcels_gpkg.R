## Setup --------------------------------------------------

# Load required packages
library(tidyverse)
library(fs)
library(sf)

# Define directories
dir_source <- "regrid/regrid_va_gpkg"
dir_output <- "regrid/regrid_va_geo"
dir_all <- "regrid/regrid_va_all"

## Unzip files --------------------------------------------

# Get a list of all zip files in the directory
zip_files <- dir_ls(path = dir_source, glob = "*.zip")

# Sample subset of zip files for testing
# zip_sample <- dir_ls(path = dir_source, glob = "*o.csv.zip")

# Function to unzip a single file
unzip_file <- function(zip_file, dir_output) {
  unzip(zip_file, exdir = dir_output)
}

# Use purrr to apply the unzip function to all ZIP files
zip_files |> 
  walk(~unzip_file(.x, dir_output))


# ## Identify universal columns -----------------------------
# 
# # Get a list of all Geopackage files in the directory
gpkg_files <- list.files(path = dir_output, pattern = "\\.gpkg$", full.names = TRUE)
# 
# write_rds(csv_files, "regrid_va_csv/csv_files.rds")
# 
# # Function to read CSV headers and create a presence vector
# read_csv_headers <- function(file_path) {
#   # Read only the first row to get column names
#   headers <- names(read_csv(file_path, n_max = 0, show_col_types = FALSE))
#   
#   # Create a named vector of 1 values for each column
#   setNames(rep(1, length(headers)), headers)
# }
# 
# # Apply the function to all CSV files
# all_headers <- map(csv_files, read_csv_headers)
# 
# # Get unique column names across all files
# all_columns <- unique(unlist(map(all_headers, names)))
# 
# # Create a dataframe with file names and column presence
# result_df <- map(
#   csv_files,
#   function(file) {
#     headers <- all_headers[[which(csv_files == file)]]
#     values <- c(
#       file_name = basename(file),
#       setNames(
#         map_dbl(all_columns, ~ if (.x %in% names(headers)) {1} else {0}),
#         all_columns
#       )
#     )
#   }
# ) |> 
#   bind_rows()
#   
# # Summarize dataframe to count column presence by locality
# result_sum <- result_df |> 
#   select(-1) |> 
#   mutate(across(everything(), ~as.numeric(.x))) |> 
#   summarise(across(everything(), sum)) |> 
#   pivot_longer(
#     everything(),
#     names_to = "variable",
#     values_to = "localities"
#   )
# 
# # Create vector of column names found in every file
# universal_vars <- result_sum$variable[which(result_sum$localities == 133)]


## Load and standardize data schema -----------------------

regrid_parcel_schema <- read_csv("regrid/regrid_docs/regrid_parcel_schema.csv") |> 
  mutate(
    type = case_when(
      str_detect(type, "text|boolean|uuid") ~ "character",
      str_detect(type, "integer|double") ~ "numeric",
      str_detect(type, "date|timestamptz") ~ "date",
      .default = type
    )
  )

# Vectors of column names by data type
vars_char <- regrid_parcel_schema$column_name[which(regrid_parcel_schema$type == "character")]
vars_num <- regrid_parcel_schema$column_name[which(regrid_parcel_schema$type == "numeric")]
vars_date <- regrid_parcel_schema$column_name[which(regrid_parcel_schema$type == "date")]


## Prepare to read and combine Geopackage files ------------------

# Function to read Geopackages and standardize output
assemble_gpkg <- function(file_names) {
  
  load_gpkg <- function(file_names) {
    
    df <- st_read(file_names) |> 
      select(any_of(universal_vars)) |> 
      mutate(file_name = str_remove_all(file_names, paste0(dir_output, "/")), .before = 1) |> 
      mutate(across(all_of(vars_char), ~as.character(.x))) |> 
      mutate(across(all_of(vars_num), ~as.numeric(.x))) |> 
      mutate(across(all_of(vars_date), ~as.Date(.x)))
    
    df
    
  }
  
  map(file_names, load_gpkg, .progress = TRUE) |> 
    list_rbind()
  
}

# Vector to use to split 133 gpkg_files into 4 groups
groups <- rep(1:4, c(34, 34, 34, 31))

# Split gpkg_files
split_list <- split(gpkg_files, groups)

# Assign the split vectors to new variables
gpkg_files_1 <- split_list[[1]]
gpkg_files_2 <- split_list[[2]]
gpkg_files_3 <- split_list[[3]]
gpkg_files_4 <- split_list[[4]]


## Read and combine Geopackage files by group --------------------

# Divide assembly into 4 groups
va_statewide_1 <- assemble_gpkg(gpkg_files_1)
va_statewide_2 <- assemble_gpkg(gpkg_files_2)
va_statewide_3 <- assemble_gpkg(gpkg_files_3)
va_statewide_4 <- assemble_gpkg(gpkg_files_4)


# Combine all Geopackages into statewide dataset
va_statewide <- bind_rows(
  va_statewide_1, va_statewide_2, va_statewide_3, va_statewide_4
)


st_write(va_statewide, "regrid/regrid_va_all/va_statewide.gpkg")



