library(readr)
library(dplyr)
library(ggplot2)
library(shiny)

neighborhoods <- list("Bronx", "Brooklyn", "Queens", "Manhattan", "Staten Island")
rooms <- list("Private room", "Entire home/apt", "Shared room")

ui <- fluidPage(
  titlePanel("Analysis of the Airbnb dataset"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput(inputId = "upload", label = "Choose CSV file"),
      textInput(inputId = "barTitle", label = "Barplot Title", value = "This is the first title"),
      checkboxGroupInput(inputId = "neighborhoods", label = "Choose the neighborhood", choices = neighborhoods),
      textInput(inputId = "scatterTitle", label = "Scatterplot Title", value = "This is the second title"),
      sliderInput(inputId = "priceRange", label = "Choose a price range", min=0, max=10000, value = c(700, 1500)),
      sliderInput(inputId = "nights", label = "Choose a range of minimum nights", min=1, max=1250, value = c(1, 20)),
      checkboxGroupInput(inputId = "roomTypes", label = "Choose the type of room", choices = rooms),
      actionButton(inputId = "btn", label = "Click to show the dimension of the data"),
      
    ),
    mainPanel = mainPanel(
      plotOutput(outputId = "barplot"),
      plotOutput(outputId = "scatter"),
      dataTableOutput(outputId = "scatterData")
    ),
  ))

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    
    ds <- read_csv(input$upload$datapath, show_col_types = F)
    
    ds
  })
  
  dataBar <- reactive({
    ds <- data()
    
    ds %>% filter(neighbourhood_group %in% input$neighborhoods)
  })
  
  dataScatter <- reactive({
    ds <- data()
    priceLower <- input$priceRange[1]
    priceUpper <- input$priceRange[2]
    nightsLower <- input$nights[1]
    nightsUpper <- input$nights[2]
    
    # output$scatterData <- renderDataTable({list()}) # reset the output
    
    ds %>%
      filter(room_type %in% input$roomTypes,
             between(price, priceLower, priceUpper),
             between(minimum_nights, nightsLower, nightsUpper),
      )
  })
  
  observe({
    dataScatter()
    
    output$scatterData <- renderDataTable({list()})
  })
  
  output$barplot <- renderPlot({
    ds <- dataBar()
    
    ds %>%
      ggplot(aes(x=ds$room_type, fill=ds$room_type)) +
      geom_bar() +
      xlab("room_type") +
      ylab("count") +
      labs(title=input$barTitle)
  })
  
  output$scatter <- renderPlot({
    ds <- dataScatter()
    
    ds %>%
      ggplot(aes(x=ds$number_of_reviews, y=ds$minimum_nights, color=ds$room_type)) +
      geom_point() +
      xlab("number_of_reviews") +
      ylab("minimum_nights") +
      labs(title=input$scatterTitle)
  })
  
  observeEvent(input$btn, {
    ds <- dataScatter()
    
    output$scatterData <- renderDataTable({
      ds
    })
  })
  
}

options(shiny.maxRequestSize = 10 * 1024 * 1024)
shinyApp(ui, server)