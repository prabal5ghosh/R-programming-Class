#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
options(shiny.maxRequestSize = 10*1024^2)

ui <- fluidPage(
  titlePanel("The app"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "attr_choice",
                   label = "select attribute to plot",
                   choices = names(iris)[-5]),
      textInput(inputId = "data_plot_type",
                label = 'enter plot name',
      ),
      numericInput(inputId = 'number_rows',
                   label = 'number of rows in table',value=6)
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "boxplot"),
      
      tableOutput(outputId = "iris_table")
    )
    
  ))




# 
# server <- function(input, output){
#   
#   
#   output$files <- renderTable({
#     req(input$upload)
#     dataset <- load(iris)
#     head(dataset)
#   })
# }

server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    iris %>%
      ggplot(aes(x=Species,y=.data[[input$attr_choice]],fill=Species)) +
      geom_boxplot(show.legend = FALSE) +
      ggtitle(input$data_plot_type)
  })
  
  output$iris_table <- renderTable({
    head(iris,input$number_rows)
  })
  
}



shinyApp(ui=ui, server= server)
