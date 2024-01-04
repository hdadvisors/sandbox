## Setup --------------------

source("vhtf.R")

library(shiny)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(DT)
library(sf)

setwd("C:/Users/Jonathan/Documents/repos/sandbox/vha/vhtf")

vhtf <- read_rds("vhtf-geocode.rds") |> 
  st_transform(4326)

districts <- read_rds("districts.rds") |> 
  st_transform(4326)

vha_pal <- colorFactor(c("#0C4D4F", "#19787B"), c("Senate", "House"))


awesomeIcons()

## Map ----------------------

leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    data = districts,
    fillColor = ~vha_pal(chamber),
    fillOpacity = 0.5,
    color = ~vha_pal(chamber),
    weight = 1.5,
    label = ~district
  ) |> 
  addMarkers(
    data = vhtf,
    popup = ~popup
  )


## User interface -----------

ui <- fluidPage(
  
  titlePanel("VHTF Dashboard"),
  
  # Drop down filter
  selectInput(
    inputId = "filter_chamber",
    label = "Senate or House",
    choices = districts$chamber
  ),
  
  # Map and table vertically stacked
  mainPanel(
    leafletOutput("map", height = "600px")
    #br(),
    #dataTableOutput("table")
  )
  
)

## Server -------------------

server <- function(input, output){
  
  leaflet() |> 
    addProviderTiles(providers$CartoDB.Positron) |> 
    addPolygons(
      data = districts,
      fillColor = ~vha_pal(chamber),
      fillOpacity = 0.5,
      color = ~vha_pal(chamber),
      weight = 1.5,
      label = ~district
    )
  
}

## Run app ------------------ 

shinyApp(ui = ui, server = server)