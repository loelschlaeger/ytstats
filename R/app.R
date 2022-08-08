library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file", "Choose CSV File", accept = c(".csv")
      )
    ),
    mainPanel(
      plotOutput("contents")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    inFile <- input$file
    
    if (is.null(inFile)) 
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = TRUE)
    
    data[,1] <- as.Date(data[,1])
    
    data
  })
  
  output$contents <- renderPlot({
    if (!is.null(data())) {
      
      plot(data(), type = "l")
      
    }
  })
}

shinyApp(ui, server)