#' Shiny app
#' 
#' @description 
#' TODO
#' 
#' @return 
#' TODO

start_app <- function() {


  # User interface --------------------------------------------------------
  
  ui <- fluidPage(
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")
  )


  # Server function -------------------------------------------------------

  server <- function(input, output, session) {
    dataset <- reactive({
      get(input$dataset, "package:datasets")
    })
    
    output$summary <- renderPrint({
      summary(dataset())
    })
    
    output$table <- renderTable({
      dataset()
    })
  }

  # Create app --------------------------------------------------------------

  shinyApp(ui, server)
  
}
