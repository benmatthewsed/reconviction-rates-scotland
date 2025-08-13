library(shiny)
library(dplyr)
library(tibble)
library(DasGuptR)

scot_reconv <- readRDS(here::here("01_data", "combined_reconvictions.rds"))

ui <- fluidPage(
  titlePanel("Scottish Reconviction Data Analysis - Compare Two Years"),
  sidebarLayout(
    sidebarPanel(
      # First year dropdown
      selectInput(
        "year1",
        "Select First Year:",
        choices = unique(scot_reconv$year),
        selected = "1997-1998"
      ),
      # Second year dropdown (different default)
      selectInput(
        "year2",
        "Select Second Year:",
        choices = unique(scot_reconv$year),
        selected = "2020-21"
      ),
      helpText("Results will update automatically when both years are selected")
    ),
    mainPanel(
      h3("Analysis Results"),
      tableOutput("results_table")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression that runs when either year changes
  analysis_results <- reactive({
    req(input$year1, input$year2)  # Both years required
    req(input$year1 != input$year2)  # Years must be different
    
    filtered_data <- scot_reconv |> 
      filter(year %in% c(input$year1, input$year2))
    
    filtered_data |> 
      dgnpop(pop = "year", 
             factors = c("prevalence"), 
             id_vars = c("gender", "age"),
             crossclassified = "number_of_offenders") |> 
      dg_table() |> 
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      rownames_to_column()
  })
  
  output$results_table <- renderTable({
    # Show validation messages or results
    if (input$year1 == input$year2) {
      data.frame(Message = "Please select two different years for comparison")
    } else {
      analysis_results()
    }
  })
}

shinyApp(ui = ui, server = server)

