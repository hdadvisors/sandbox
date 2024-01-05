## Setup --------------------

setwd("~/repos/sandbox/vha/vhtf")

source("vhtf.R")

library(shiny)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(DT)
library(sf)
library(shinyjs)

vhtf <- read_rds("vhtf-districts.rds") |> 
  st_transform(4326)

districts <- read_rds("districts.rds")

vha_pal <- colorFactor(c("#0C4D4F", "#19787B"), c("Senate", "House"))

## Map ----------------------

leaflet(data = filter(districts, district == "HD 1")) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    fillColor = ~vha_pal(chamber),
    fillOpacity = 0.5,
    color = ~vha_pal(chamber),
    weight = 1.5,
    #label = ~district,
    #labelOptions = labelOptions(permanent = TRUE, textOnly = TRUE)
  ) |> 
  addMarkers(
    data = vhtf,
    popup = ~popup
  ) |> 
  fitBounds(~min(min_x), ~min(min_y), ~max(max_x), ~max(max_y))

## User interface -----------

ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("VHTF Competitive Loan Pool Projects"),
  
  wellPanel(
  
    fluidRow(
      column(2,
        # Chamber filter
        radioButtons(
          inputId = "filter_chamber",
          label = "Select chamber:",
          choices = unique(districts$chamber),
          selected = districts$chamber[1]
        )
      ),
      column(3,
        selectInput(
          "filter_district",
          "Choose district:",
          choices = NULL
        )
      ),
      column(2,
        actionButton("reset", "Reset selection"),
        offset = 5
      )
    )
  
  ),
  
  fluidRow(
    column(12,
      leafletOutput("map")
    )
  )
  
)

## Server -------------------

server <- function(input, output, session){
  
  filtered_districts <- reactive({
    if (input$filter_chamber == "House") {
      filter(districts, chamber == "House")
    } else {
      filter(districts, chamber == "Senate")
    }
  })
  
  filtered_points <- reactive({
    if (input$filter_district == "All districts") {
      vhtf
    } else {
      filter(vhtf, if_all(c(house, senate), ~ .x == input$filter_district))
    }
  })
  
  observe({
    updateSelectInput(
      session,
      "filter_district",
      choices = c("All districts", unique(filtered_districts()$district))
      )
  })
  

  output$map <- renderLeaflet({
    
    data <- filtered_districts()
    
    if(input$filter_district != "All districts") {
      data <- data[data$district == input$filter_district,]
    }
    
    points <- filtered_points()
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = data,
        fillOpacity = 0.5,
        color = ~vha_pal(chamber),
        weight = 1.5,
        layerId = ~district
      ) |> 
      addMarkers(
        data = points,
        popup = ~popup
      ) |> 
      onRender(
        "function(el, x) {
            L.control.zoom({position:'bottomleft'}).addTo(this);
          }"
      )
    
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if(!is.null(click$id)) {
      updateSelectInput(session, "filter_district", selected = click$id)
    } else {
      updateSelectInput(session, "filter_district", selected = "All districts")
    }
  })
  
  observe({
    if (input$filter_district != "All districts") {
      
      filtered <- filtered_districts()
      specificDistrict <- filtered[filtered$district == input$filter_district, ]
      
      points2 <- filtered_points()
      specificPoints <- filter(points2, if_all(c(house, senate), ~ .x == input$filter_district))
      
      leafletProxy("map") |> 
        clearShapes() |> 
        clearMarkers() |> 
        addPolygons(
          data = specificDistrict,
          fillOpacity = 0.5,
          color = ~vha_pal(chamber),
          weight = 1.5,
          layerId = ~district
        ) |> 
        addMarkers(data = specificPoints)
      
    } else {
      
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(
          data = filtered_districts(),
          fillOpacity = 0.5,
          color = ~vha_pal(chamber),
          weight = 1.5,
          layerId = ~district
        ) |> 
        addMarkers(data = filtered_points())
      
    }
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "filter_district", selected = "All districts")
  })
  
}

## Run app ------------------ 

shinyApp(ui = ui, server = server)
