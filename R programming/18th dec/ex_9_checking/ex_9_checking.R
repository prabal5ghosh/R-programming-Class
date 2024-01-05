# # # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above. # # Find out more about
# # building applications with Shiny here: # #    http://shiny.rstudio.com/ # #
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
options(shiny.maxRequestSize = 10*1024^2)

# data_2<- read.csv('C:\\Users\\praba\\Desktop\\uca1\\M1\\R programming\\18th dec\\datasets-20231218\\airbnb.csv')


ui <- fluidPage(
  
  titlePanel("Analysis of airbnb dataset"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        inputId = 'upload_file', label = 'choose a csv file',accept = ".csv"
      ),
      
      textInput(inputId = "bar_plot_name", label = 'Barplot title', value= 'Prabal is ploting barplot'
      ),
      
      uiOutput("checkbox"),
      
      textInput(inputId = "scatter_plot_name", label = 'scatter plot title', value= 'Prabal is ploting scatterplot'
      ),
      uiOutput("slider"),
      uiOutput("slider2"),
      uiOutput("room_type"),
      actionButton("dimension", "Click to show the dimension of the data"),

      
      
    ),
    
    mainPanel(
      # textOutput(outputId = "barplot_1_title"),
      plotOutput(outputId = "barplot_1"),
      
      textOutput(outputId = "scatter_1_title"),
      plotOutput(outputId = "scatter1"),
      
      verbatimTextOutput(outputId = "dimensions"),
      
      dataTableOutput('filtered_table_data')
      
    )
    
  ))


server <- function(input, output) {
  
  data = reactive({
    data <- input$upload_file
    ext <- tools::file_ext(data$datapath)

    req(data)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(data$datapath, header = T)
  })
  
  
  
  output$checkbox <- renderUI({
    checkboxGroupInput("neightborhood_grp", "Choose the neightborhood", choices = sort(unique(data()$neighbourhood_group)), selected = "Queens")
  })
  output$slider <- renderUI({
    sliderInput("bins_price", "Choose a price range", min = min(data()$price), max = max(data()$price), value = c(700, 1500))
  })
  output$slider2 <- renderUI({
    sliderInput("bins_night", "Choose a range of minimum nights", min = min(data()$minimum_nights), max = max(data()$minimum_nights), value = c(1, 20))
  })
  output$room_type <- renderUI({
    checkboxGroupInput("room_type_1", "Choose the type of room", choices = unique(data()$room_type), selected = "Entire home/apt")
  })
  
  
  
  
  
  
  # output$barplot_1_title <- renderText(
  #   {
  #     paste0(input$bar_plot_name)
  #   })
  
  


  output$barplot_1 <- renderPlot({

    data() %>% filter(neighbourhood_group %in% input$neightborhood_grp ) %>% ggplot(aes(room_type)) +
      geom_bar(aes(fill=room_type))+
      xlab("room_type")+
      ylab("count")+
      title(input$bar_plot_name)
  })
  
  # neigh_group <- reactive({
  #   data() %>% dplyr::filter(neighbourhood_group %in% input$neightborhood_grp)
  #                         })
  # 
  # output$barplot_1 <- renderPlot({
  #   neigh_group() %>% ggplot() + geom_bar(aes(room_type, fill = as.factor(room_type)))+ labs(fill = "Room type", title = input$bar_plot_name)
  # })

  
  
  output$scatter_1_title<- renderText({
    paste0(input$scatter_plot_name)
  }
  )

  
  
  
  
  output$scatter1 <- renderPlot({
    data() %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                      &(price>= input$bins_price[1] & price<=input$bins_price[2])) %>%
      ggplot(aes(x= number_of_reviews,y= minimum_nights,col=room_type)) +
      geom_point()+
      xlab("number_of_reviews")+
      ylab("minimum_night")
  })
  
  

  
  
  
  # price_sel <- reactive({data() %>% dplyr::filter(price >= input$bins_price[1], price <= input$bins_price[2])})
  # 
  # night_sel <- reactive({
  #   req(input$bins_price)
  #   price_sel() %>% dplyr::filter(minimum_nights >= input$bins_night[1], minimum_nights <= input$bins_night[2])})
  # 
  # room_type_sel <- reactive({
  #   req(input$bins_night)
  #   night_sel() %>% dplyr::filter(room_type %in% input$room_type_1)
  # 
  # })
  # 
  # output$scatter1 <-renderPlot({
  #   col = as.factor(room_type_sel()[,'room_type'])
  # 
  #   room_type_sel() %>% ggplot()+geom_point(aes(number_of_reviews, minimum_nights,col = col))+ labs(col = "Room type", title = input$scatter_1_title)
  # })

  
  
  
  
  # output$dimensions <- renderPrint({
  #   if (input$dimension) {paste("This dataset has ", dim(room_type_sel())[1], "rows and", dim(room_type_sel())[2], "columns" )
  #   }
  # })
  
  
  # output$filtered_table_data <- renderDataTable({
  #   room_type_sel() 
  # })
  
  
  
  
  

  observeEvent(input$dimension, {
    # When the button is clicked, calculate and display the dimensions
    data_filtered<- data() %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                                      &(price>= input$bins_price[1] & price<=input$bins_price[2]))
    
    
    # output$dimensions <- renderText({
    #   dim_text <- paste("Number of rows:", nrow(data_filtered), "\nNumber of columns:", ncol(data_filtered))
    #   return(dim_text)
    # })
    
    output$dimensions <- renderText({
      dim_text <- paste("This dataset has ", nrow(data_filtered), "rows and", ncol(data_filtered), "columns" )
      return(dim_text)
      
      
    })
  })
  
  
  
  output$filtered_table_data <- renderDataTable({
    data_filtered<- data() %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                                      &(price>= input$bins_price[1] & price<=input$bins_price[2]))
    data_filtered
  })
  
}
shinyApp(ui=ui, server= server)





