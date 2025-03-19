library(tidyverse)
library(readxl)
library(hdatools)
library(janitor)

nhpd <- read_excel("nhpd/nhpd_va_subsidies.xlsx")


lihtc <- nhpd |> 
  clean_names() |> 
  filter(subsidy_name == "LIHTC") |> 
  filter(subsidy_status != "Inactive") 


# Assuming your dataframe is called 'df'
# First, let's ensure the date is in the correct format
lihtc$end_date <- as.Date(lihtc$end_date)

# Create cumulative sum of units lost over time
df_cumulative <- lihtc %>%
  arrange(end_date) %>%
  mutate(cumulative_units = cumsum(assisted_units))

# Create the visualization
ggplot(df_cumulative, aes(x = end_date, y = cumulative_units)) +
  # Add step line to show cumulative loss
  geom_step(color = "red", size = 1) +
  # Add points at each date where units are lost
  geom_point(size = 3) +
  # Customize theme and labels
  theme_minimal() +
  labs(
    title = "Cumulative Loss of Affordable Housing Units Over Time",
    x = "Date",
    y = "Number of Units Lost",
    caption = "Source: Housing Data"
  ) +
  # Rotate x-axis labels for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  # Format y-axis with comma separator for thousands
  scale_y_continuous(labels = scales::comma)

# If you want to see both cumulative and individual loss events
# Create a second visualization
ggplot() +
  # Bar chart showing individual loss events
  geom_col(data = lihtc, aes(x = end_date, y = assisted_units), 
           fill = "#011E41", alpha = 0.6) +
  # Line showing cumulative loss
  geom_line(data = df_cumulative, 
            aes(x = end_date, y = cumulative_units),
            color = "#259591", size = 1) +
  theme_minimal() +
  labs(
    title = "LIHTC Units At-Risk of Loss Over Time in Virginia",
    subtitle = "Individual Events and Cumulative Loss",
    x = "Date",
    y = "Number of Units",
    caption = "Source: National Housing Preservation Database."
  ) +
  theme_hfv() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma)
