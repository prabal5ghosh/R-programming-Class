#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
options(shiny.maxRequestSize = 10*1024^2)

data_1<- read.csv('C://Users//praba//Desktop//uca1//M1//R programming//15th dec//advs.csv')

ui <- fluidPage(
  titlePanel("prabal ghosh"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
                  inputId = "x_v",label = "select x variable:",
                  choices = sort(names(data_1)),
                  ),
      
      selectInput(
                  inputId = "y_v", label= "select y variable:",
                  choices=  sort(names(data_1))
                  ),
      
      selectInput(
                  inputId = "sub_v", label= "select subject:",
                  choices = unique(data_1$USUBJID)
                  ),
      
      selectInput(
                  inputId = "param_v", label= "select param:",
                  choices=  unique(data_1$PARAM),
                 ),
      
             ),
    
    mainPanel(
      
      plotOutput(outputId = "scatter1"),
      
    )
    
  ))




server <- function(input, output) {
  
  output$scatter1 <- renderPlot({
    
    data_1 %>% filter(USUBJID == input$sub_v & PARAM == input$param_v) %>% ggplot(aes(.data[[input$x_v]],.data[[input$y_v]])) + geom_point(col="black")+
      xlab("VISITNUM")+
      ylab("AVAL")
  })
  
  
}




shinyApp(ui=ui, server= server)

