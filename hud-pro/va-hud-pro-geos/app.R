## Setup --------------------

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(DT)
library(sf)

# setwd("C:/Users/Jonathan/Documents/repos/sandbox/hud-pro/")

geos_priority <- read_rds("geos_priority.rds")

pdc_pal <- colorFactor(rainbow(23), unique(geos_priority$pdc))

## User interface -----------

ui <- fluidPage(
  
  # Drop down PDC filter
  selectInput(
    inputId = "filter",
    label = "Select PDC:",
    choices = c("All", geos_priority$pdc)
  ),
  
  # Map and table vertically stacked
  mainPanel(
    leafletOutput("map"),
    br(),
    dataTableOutput("table")
  )
  
)

## Server -------------------

server <- function(input, output){

  # Apply PDC filter to data
  filtered_data <- reactive({
    if (input$filter == "All") {
      geos_priority
    } else {
      subset(geos_priority, pdc == input$filter)
    }
  })
  
  # Default Leaflet map of geographies
  output$map <- renderLeaflet({
    leaflet(geos_priority) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      addPolygons(
        fillColor = ~pdc_pal(pdc),
        fillOpacity = 0.5,
        color = ~pdc_pal(pdc),
        weight = 2,
        label = ~popup
      ) |>  
      fitBounds(~min(min_x), ~min(min_y), ~max(max_x), ~max(max_y))
  })
  
  # Leaflet map when data is filtered
  observe({
    leafletProxy("map", data = filtered_data()) |> 
      clearShapes() |> 
      addPolygons(
        fillColor = ~pdc_pal(pdc),
        fillOpacity = 0.5,
        color = ~pdc_pal(pdc),
        weight = 2,
        label = ~popup
      ) |>   
      fitBounds(~min(min_x), ~min(min_y), ~max(max_x), ~max(max_y))
  })
  
  # Table
  output$table <- renderDT({
    DT::datatable(
      filtered_data() %>%
        as.data.frame() %>%
        select(1:3),
      options = list(
        order = list(1, 'asc')
      ),
      rownames = FALSE,
      colnames = c("PDC", "Name", "Type")
    )
  })

}

## Run app ------------------ 

shinyApp(ui = ui, server = server)
