# # # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above. # # Find out more about
# # building applications with Shiny here: # #    http://shiny.rstudio.com/ # #
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
options(shiny.maxRequestSize = 10*1024^2)

data_2<- read.csv('C:\\Users\\praba\\Desktop\\uca1\\M1\\R programming\\18th dec\\datasets-20231218\\airbnb.csv')


ui <- fluidPage(
  
  titlePanel("Analysis of airbnb dataset"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        inputId = 'upload', label = 'choose a csv file'
      ),
      
      textInput(inputId = "bar_plot_name", label = 'Barplot title', value= 'Prabal is ploting barplot'
                ),
      
      checkboxGroupInput(
        inputId= 'neighbourhood_grp',label="choose the neighbourhood",
        choices =unique(data_2$neighbourhood_group), selected = 'Queens'
      ),
      
      
      textInput(
        inputId = "scatter_plot_name", label = 'scatter title', value='scatter plot example given by Prabal '
      ),
      
      sliderInput(
       inputId= "bins_price", label= "Choose a price range:", 
        min = min(data_2$price), max =max(data_2$price), value = c(700, 1500)
      ),
      
      sliderInput("bins_night", "Choose a range of minimum nights:", 
                  min = min(data_2$minimum_nights), max = max(data_2$minimum_nights), value = c(1, 20)
      ),
      
      checkboxGroupInput(
        inputId= 'room_type_1',label="choose the type of room",
        choices =unique(data_2$room_type), selected = 'Entire home/apt'
      ),
      
      actionButton( 
        inputId = "clicks_1", label = " Click to show the dimension ofthe data"
      )
      
      
    ),
    
    mainPanel(
      textOutput(outputId = "barplot_1_title"),
      plotOutput(outputId = "barplot_1"),
      
      textOutput(outputId = "scatter_1_title"),
      plotOutput(outputId = "scatter1"),
      
      verbatimTextOutput(outputId = "dimensions"),
      
      # tableOutput('filtered_table_data')
      dataTableOutput('filtered_table_data')
      
    )
    
  ))


server <- function(input, output) {
  
  # output$filtered_table_data <- renderTable({
  #   req(input$upload)
  #   dataset <- read_csv(input$upload$datapath)
  #   head(dataset)
  # })
  
  output$filtered_table_data <- renderTable({
    req(input$upload)
    data_2 <- read_csv(input$upload$datapath)
    # head(dataset, input$number_row)
  })
  
  


  
  
  
  output$barplot_1_title <- renderText(
    {
      paste0(input$bar_plot_name)
    })
  
  
  # output$barplot_1 <- renderPlot({
  #   
  #   data_2 %>% filter(neighbourhood_group == input$neighbourhood_grp ) %>% ggplot(aes(.data[[input$room_type_1]])) +
  #     geom_bar(aes(fill=.data[[input$room_type_1]]))+
  #     xlab("room_type")+
  #     ylab("count")
  # })
  
  
  
  output$barplot_1 <- renderPlot({
    
    data_2 %>% filter(neighbourhood_group %in% input$neighbourhood_grp ) %>% ggplot(aes(room_type)) +
      geom_bar(aes(fill=room_type))+
      xlab("room_type")+
      ylab("count")
  })
  
  
  output$scatter_1_title<- renderText({
    paste0(input$scatter_plot_name)
    }
  )
  
  
  
  

  output$scatter1 <- renderPlot({
    data_2 %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                      &(price>= input$bins_price[1] & price<=input$bins_price[2])) %>%
                      ggplot(aes(x= number_of_reviews,y= minimum_nights,col=room_type)) +
                      geom_point()+
                      xlab("number_of_reviews")+
                      ylab("minimum_night")
                      })

  
  
  # 
  # output$scatter1 <- renderPlot({
  #   data_2 %>% filter((room_type == input$room_type_1)
  #                     & (neighbourhood_group == input$neighbourhood_grp)) %>% 
  #     ggplot(aes(number_of_reviews,.data[[input$bins_night]])) +
  #     geom_point(aes(fill=room_type))+
  #     xlab("number_of_reviews")+
  #     ylab("minimum_night")
  # })
  # 
  
  
  
  observeEvent(input$clicks_1, {
    # When the button is clicked, calculate and display the dimensions
    data_filtered<- data_2 %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                                      &(price>= input$bins_price[1] & price<=input$bins_price[2])) 
    output$dimensions <- renderText({
      dim_text <- paste("Number of rows:", nrow(data_filtered), "\nNumber of columns:", ncol(data_filtered))
      return(dim_text)
    })
  })
  
  
  
  output$filtered_table_data <- renderDataTable({
    data_filtered<- data_2 %>% filter((room_type %in% input$room_type_1)  & (minimum_nights >=input$bins_night[1] & minimum_nights <= input$bins_night[2])
                                      &(price>= input$bins_price[1] & price<=input$bins_price[2])) 
    data_filtered
  })
  
}
shinyApp(ui=ui, server= server)





