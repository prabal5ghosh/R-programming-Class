library(shiny)

options(shiny.maxRequestSize=10*1024^2)

ui <- fluidPage(
  
  titlePanel("Exercise 7 : Uploading a file"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(inputId = "upload",
                label = "Upload..."
                ),
      
      numericInput(inputId = "nb_row",
                   label = "Select number of rows to display",
                   value = 10)
    ),
    
    mainPanel(
      
      tableOutput("file")
    )
  )
  
)

server <- function(input, output, session) {
  
  output$file <- renderTable({
    
    req(input$upload)
    dataset <- read.csv(input$upload$datapath)
    head(dataset,input$nb_row)
    })
  
}

shinyApp(ui, server)