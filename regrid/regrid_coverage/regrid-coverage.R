library(tidyverse)
library(shiny)
library(hdatools)

# setwd("~/repos/sandbox/regrid")

regrid <- read_csv("regrid-coverage-va.csv") |> 
  select(2, 6:50)

bin_labs <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")

regrid |>
  mutate(
    bin = cut(parcelnumb_pct, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE)
  ) |>
  mutate(bin_label = gsub("\\[|\\]|\\)", "", as.character(bin))) |>
  mutate(bin_label = gsub(",", "-", bin_label)) |>
  summarise(n = n(), .by = "bin_label") |>
  arrange(bin_label) |>
  ggplot(aes(x = bin_label, y = n, label = n)) +
  geom_col() +
  scale_x_discrete(labels = bin_labs) +
  scale_y_continuous(limits = c(0, 135), breaks = c(0, 50, 100, 133)) +
  add_zero_line() +
  theme_hda()

ui <- fluidPage(
  titlePanel("title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("field", "Data field:", choices = colnames(regrid)[-1])
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    selected_field <- input$field
    if (is.null(selected_field)) return(NULL)
    
    regrid |> 
      mutate(
        bin = cut(as.numeric(.data[[selected_field]]), breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE)
      ) |> 
      mutate(bin_label = gsub("\\[|\\]|\\)", "", as.character(bin))) |> 
      mutate(bin_label = gsub(",", "-", bin_label)) |> 
      summarise(n = n(), .by = "bin_label") |> 
      arrange(bin_label) |> 
      ggplot(aes(x = bin_label, y = n, label = n)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 135), breaks = c(0, 50, 100, 133)) +
      add_zero_line() +
      theme_hda()

  })
}

shinyApp(ui = ui, server = server)

set.seed(123)
x <- sample(1:3, 100, replace = TRUE) 
bins <- cut(x, breaks = 4, labels = 1:4)
bins_tab <- table(bins)
bins_tab