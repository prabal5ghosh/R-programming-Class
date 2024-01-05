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
          uiOutput("checkbox"),
          textInput("title2", "Scatter plot title", "This is the second title"),
          
          uiOutput("slider"),
          uiOutput("slider2"),
          uiOutput("room_type"),
          actionButton("dimension", "Click to show the dimension of the data"),
          
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("bar"),
          plotOutput("scat"),
          verbatimTextOutput("dim"),
          
          dataTableOutput("head")
          
          
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
  output$checkbox <- renderUI({
    checkboxGroupInput("neightborhood", "Choose the neightborhood", choices = sort(unique(data()$neighbourhood_group)), selected = "Queens")
  })
  output$slider <- renderUI({
    sliderInput("price", "Choose a price range", min = min(data()$price), max = max(data()$price), value = c(700, 1500))
  })
  output$slider2 <- renderUI({
    sliderInput("nights", "Choose a range of minimum nights", min = min(data()$minimum_nights), max = max(data()$minimum_nights), value = c(1, 20))
  })
  output$room_type <- renderUI({
    checkboxGroupInput("room_type", "Choose the type of room", choices = unique(data()$room_type), selected = "Entire home/apt")
  })
  
  
  
  neigh_group <- reactive({data() %>% dplyr::filter(neighbourhood_group %in% input$neightborhood)})

  output$bar <- renderPlot({
    neigh_group() %>% ggplot() + geom_bar(aes(room_type, fill = as.factor(room_type)))+ labs(fill = "Room type", title = input$title1) 
  })
  
  
  

  price_sel <- reactive({data() %>% dplyr::filter(price >= input$price[1], price <= input$price[2])})
  
  night_sel <- reactive({
    req(input$price)
    price_sel() %>% dplyr::filter(minimum_nights >= input$nights[1], minimum_nights <= input$nights[2])})
  
  room_type_sel <- reactive({
    req(input$nights)
    night_sel() %>% dplyr::filter(room_type %in% input$room_type)
    
  })
  
  output$scat <-renderPlot({
    col = as.factor(room_type_sel()[,'room_type'])
    
    room_type_sel() %>% ggplot()+geom_point(aes(number_of_reviews, minimum_nights,col = col))+ labs(col = "Room type", title = input$title2)
  })
  
  
  
  output$dim <- renderPrint({
    if (input$dimension) {paste("This dataset has ", dim(room_type_sel())[1], "rows and", dim(room_type_sel())[2], "columns" )
}
  })
  output$head <- renderDataTable({
    room_type_sel() 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
