#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Analysis of the Airbnb dataset"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"),
      textInput("title1", "Barplot title", "This is the first title"),
      uiOutput("checkbox")
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("bar")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  
  data = reactive({
    data <- input$file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(data$datapath, header = T)
  })
  
  
  # Define all the buttons depending on the loaded dataset
  output$checkbox <- renderUI({
    checkboxGroupInput("neightborhood", "Choose the neightborhood", choices = sort(unique(data()$neighbourhood_group)), selected = "Queens")
  })
  
  neigh_group <- reactive({
    data() %>%
      dplyr::filter(neighbourhood_group %in% input$neightborhood)})
  
  
  output$bar <- renderPlot({
    neigh_group() %>%
      ggplot() +
      geom_bar(aes(room_type, fill = as.factor(room_type))) +
      labs(fill = "Room type", title = input$title1) 
  })
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
