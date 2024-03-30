library(tidycensus)
library(tidyverse)
library(sf)
library(leaflet)
library(scales)
library(leaflet.extras)


medincome <- read_rds("medincome.rds")
# medincome <- get_acs(
#   geography = "tract",
#   state = "VA",
#   table = "B19013",
#   year = 2022,
#   survey = "acs5",
#   geometry = TRUE
# ) |> 
#   filter(
#     str_detect(NAME, "Richmond city") | 
#       str_detect(NAME, "Henrico") | 
#       str_detect(NAME, "Chesterfield")) |> 
#   mutate(NAME = str_remove(NAME, "; Virginia")) |> 
  # mutate(AMI = percent(estimate/109400)) |> #FY 2023 Median Income
  # st_transform(crs = "+proj=longlat +datum=WGS84")


# Define UI

tags$style(HTML("
  .shiny-output {
    width: 100vw !important;
    height: 100vh !important;
  }
"))

ui <- fluidPage(
  tags$style(HTML("
    .shiny-output {
      width: 100vw !important;
      height: 100vh !important;
    }
  ")),
  titlePanel("Median Household Income by Census Tract"),
  leafletOutput("map")
)

# Define server logic
server <- function(input, output, session) {
  
  pal <- colorNumeric(palette = "Reds", domain = medincome$estimate)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() |> 
      addPolygons(data = medincome,
                  color = ~pal(estimate),
                  fillOpacity = 0.6,
                  opacity = 1,
                  weight = 1,
                 popup = ~paste("Location:", NAME, "<br>",
                                "2022 Median Household Income:", 
                                scales::dollar(estimate), "<br>",
                                "Percent AMI (using 2023 Median):", AMI)) |> 
      addLegend(data = medincome,
                pal = pal, 
                values = ~estimate,
                title = "Median Household Income",
                opacity = 0.6,
                position = "bottomright")
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)