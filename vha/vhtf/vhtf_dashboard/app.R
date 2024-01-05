## Setup --------------------

#setwd("~/repos/sandbox/vha/vhtf")

source("setup.R")

## Load data ----------------

vhtf <- read_rds("vhtf-districts.rds")

districts <- read_rds("districts.rds")

## User interface -----------

ui <- fixedPage(
  
  # Load shinyjs to enable "Reset selection" button
  useShinyjs(),
  
  # Load custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # App title
  titlePanel("Virginia Housing Trust Fund project dashboard"),
  
  #p("Some test text to go here"),
  
  # Control panel
  wellPanel(
  
    fluidRow(
      column(7,
        markdown("**Competitive Loan Pool projects** (FY2014 - FY2023)"),
        markdown("Use these controls to view projects by individual House of Delegates or State Senate district. You can also filter projects by clicking on a district in the map. Click the \"Reset selection\" button to reload all data.")
      ),
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
        # District selection
        selectInput(
          "filter_district",
          "Select district:",
          choices = NULL
        ),
        # Reset selection button
        actionButton("reset", "Reset selection")
      )
    )
  
  ),
  
  fluidRow(
    # Map
    leafletOutput("map"),
    br()
  ),
  
  fluidRow(
    # Tables
    tabsetPanel(
      tabPanel("Summary", dataTableOutput("summary")),
      tabPanel("List of projects", dataTableOutput("list"))
    )
  ),
  
  fluidRow(
    br(),
    wellPanel(
      markdown(
        "**Source:** Virginia Department of Housing and Community Development. Data includes all projects awarded funds from the Competitive Loan Pool of the Virginia Housing Trust Fund, from FY2014 to FY2023.<br>
      **Last updated:** January 5, 2024.")
    )
  )
  
)

## Server -------------------

server <- function(input, output, session){
  
  # Reactive action to filter districts by chamber
  filtered_districts <- reactive({
    if (input$filter_chamber == "House") {
      filter(districts, chamber == "House")
    } else {
      filter(districts, chamber == "Senate")
    }
  })
  
  # Reactive action to filter points by district
  filtered_points <- reactive({
    if (input$filter_district == "All districts") {
      vhtf
    } else {
      vhtf[vhtf$house == input$filter_district | vhtf$senate  == input$filter_district, ]
    }
  })
  
  # Update district selection based on chamber filter
  observe({
    updateSelectInput(
      session,
      "filter_district",
      choices = c("All districts", unique(filtered_districts()$district))
      )
  })
  
  # Default map
  output$map <- renderLeaflet({
    
    data <- filtered_districts()
    
    if(input$filter_district != "All districts") {
      data <- data[data$district == input$filter_district,]
    }
    
    points <- filtered_points()
    
    if(input$filter_district != "All districts") {
      points <- points[points$house == input$filter_district | points$senate  == input$filter_district, ]
    }
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = data,
        fillOpacity = 0.5,
        color = ~vha_pal(chamber),
        weight = 1.5,
        layerId = ~district,
        label = ~paste(district, " ")
      ) |> 
      addMarkers(
        data = points,
        label = ~paste(project_name, " "),
        popup = ~popup
      ) |> 
      onRender(
        "function(el, x) {
            L.control.zoom({position:'bottomleft'}).addTo(this);
          }"
      )
    
  })
  
  # Summary
  output$summary <- renderDT({
    
    points <- filtered_points()
    
    if(input$filter_district != "All districts") {
      points <- points[points$house == input$filter_district | points$senate  == input$filter_district, ]
    }
    
    DT::datatable(
      points %>% 
        as.data.frame() %>% 
        summarise(
          n = n(),
          units = sum(units_affordable),
          amt = sum(amt_vhtf)
        ), 
      options = list(
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      colnames = c("Projects awarded", "Affordable units produced", "Amount awarded"),
      selection = "single"
    ) |> 
      formatRound(2, mark = ",", digits = 0) |> 
      formatCurrency(3, digits = 0)
    
  })
  
  # List of projects
  output$list <- renderDT({
    
    points <- filtered_points()
    
    if(input$filter_district != "All districts") {
      points <- points[points$house == input$filter_district | points$senate  == input$filter_district, ]
    }
    
    DT::datatable(
      points %>% 
        as.data.frame() %>% 
        select(26, 27, 1, 4, 7, 15),
      options = list(
        dom = "tp",
        order = list(3, 'asc'),
        pageLength = 5
      ),
      rownames = FALSE,
      colnames = c("HD", "SD", "Award year", "Project", "Affordable units", "VHTF award"),
      selection = "single"
    ) |> 
      formatCurrency(6, digits = 0)
    
  })
  
  # Click to filter district polygon
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
      specificPoints <- points2[points2$house == input$filter_district | points2$senate  == input$filter_district, ]
      
      leafletProxy("map") |> 
        clearShapes() |> 
        clearMarkers() |> 
        addPolygons(
          data = specificDistrict,
          fillOpacity = 0.5,
          color = ~vha_pal(chamber),
          weight = 1.5,
          label = ~paste(district, " "),
          layerId = ~district
        ) |> 
        addMarkers(
          data = specificPoints,
          label = ~paste(project_name, " "),
          popup = ~popup
          )

    } else {
      
      leafletProxy("map") |> 
        clearShapes() |> 
        addPolygons(
          data = filtered_districts(),
          fillOpacity = 0.5,
          color = ~vha_pal(chamber),
          weight = 1.5,
          label = ~paste(district, " "),
          layerId = ~district
        ) |> 
        addMarkers(
          data = filtered_points(),
          label = ~paste(project_name, " "),
          popup = ~popup
          )
      
    }
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "filter_district", selected = "All districts")
  })
  
}

## Run app ------------------ 

shinyApp(ui = ui, server = server)
