#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
options(shiny.maxRequestSize = 10*1024^2)


# data_3<- read.csv2("C:\\Users\\praba\\Desktop\\uca1\\M1\\R programming\\exam 21st dec\\winequality-red.csv")
data_3<- read.csv("C:\\Users\\praba\\Desktop\\uca1\\M1\\R programming\\exam 21st dec\\winequality-red.csv",header=TRUE, sep = ";")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  #1.The title of the app is “Analysis of the Wine Quality dataset”.
  #2.Create all buttons in a side panel and all results in the main panel.
  #3.Divide the main panel in 2 tabs and name them “tab1” and “tab2”.
    titlePanel("Analysis of the Wine Quality dataset"),
    

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          #4.Add an input such that you can select the level of quality, more than one level allowed.
          
          checkboxGroupInput(
            inputId= 'quality_input',label="choose the quality",
            choices =sort(unique(data_3$quality)), selected = c('5','6')
          ),
          
          #6.Add an input where you can modify the title of the scatterplot.
          textInput(
            inputId = "scatter_plot_name", label = 'scatter title', 
            value='scatter plot 1  by Prabal '
          ),
          
          #7.Add an input where you can select one variable of the dataset, to be used in 8),
          radioButtons(inputId = "attribute",
                       label = "Select attribute for box plot",
                       choices=names(data_3)[-12],
                      selected = 'sulphates'
                       ),
          textInput(inputId = "box_plot_title",
                    label = "Enter plot name",
                    value='box plot 1 by prabal'),
          
          sliderInput(
            inputId= "free_sulfur_dioxide_range", label= "Choose a range for free.sulfur.dioxide:", 
            min = min(as.numeric(data_3$free.sulfur.dioxide)), max =max(as.numeric(data_3$free.sulfur.dioxide)), value = c(1, 2)
          ),
          
          # numericInput(inputId = "table_row_num",
          #              label = "Number of rows in table",
          #              value = 5),
          # 
          
          checkboxGroupInput(
            inputId= 'column_to_show',label="choose the columns to show in a table",
            choices =names(data_3), selected = c('quality','chlorides')
          ),
          
          numericInput(inputId = "table_column_row_num",
                       label = "Number of rows in table column",
                       value = 5),
          
        ),

        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel(
              title = "tab1", 
              # textOutput(outputId = "scatter_1_title"),
              
              plotOutput(outputId = "scatter_plot_1",width = 1000, height = 700),
              plotOutput(outputId = "box_plot_1"),
              
              tableOutput(outputId = 'filtered_table_data'),
              
              dataTableOutput(outputId = 'filtered_table_column')
              
              
            ),
            
            tabPanel(
              title = "tab2", 
              verbatimTextOutput(outputId = "main_summ"),
              textOutput(outputId = "mean"),
              textOutput(outputId = "sd"),
              plotOutput(outputId = 'barplot_tab2')
              
            ),
           
        )
    )
))



server <- function(input, output, session) {
  
  

  # output$scatter_1_title<- renderText({
  #   paste0(input$scatter_plot_name)
  # }
  # )
  
  #5.Show a scatter plot of citric acid and pH where the color of points depends on the quality selected in 4).
  output$scatter_plot_1 <- renderPlot({
    data_3 %>% filter(quality %in% input$quality_input) %>%
      ggplot(aes(x= citric.acid,y= pH,)) +
      geom_point(aes(col=factor(quality)))+
      xlab("citric.acid")+
      ylab("pH")+
      ggtitle(input$scatter_plot_name)
  })
  
  
  #8.Depending on the selected levels of quality, show boxplots of quality and the variable of the dataset selected in 7), with different colors in the “tab1”.
  
  #9.Pay attention to the label on the y axis, make sure it changes according to the variable selected in 7),
  
  output$box_plot_1 <- renderPlot({
    data_3 %>% filter(quality %in% input$quality_input) %>%
      ggplot(aes(x= factor(quality),y=.data[[input$attribute]])) +
      geom_boxplot(aes(col=quality,fill=factor(quality)))+
      ggtitle(input$box_plot_title)
  })
  
  
  # 
  # output$filtered_table_data <- renderTable({
  #   data_filtered1<- data_3 %>% filter((free.sulfur.dioxide >=input$free_sulfur_dioxide_range[1] &
  #                                        free.sulfur.dioxide <= input$free_sulfur_dioxide_range[2]))
  #                                     
  #   head(data_filtered1, input$table_row_num)
  # })
  # 
  
  
  output$filtered_table_column <- renderDataTable({
    data_filtered2<- data_3 %>% filter((free.sulfur.dioxide >=input$free_sulfur_dioxide_range[1] &
                                         free.sulfur.dioxide <= input$free_sulfur_dioxide_range[2]))
    
    head(data_filtered2[,c(input$column_to_show)], input$table_column_row_num)
  })
  
  
  output$main_summ <- renderPrint({
    data_filtered3<- data_3 %>% filter((free.sulfur.dioxide >=input$free_sulfur_dioxide_range[1] &
                                         free.sulfur.dioxide <= input$free_sulfur_dioxide_range[2]))
    
     summary(data_filtered3[,c(input$column_to_show)])
    
  })
  

  
  output$barplot_tab2 <- renderPlot({
    
    data_3 %>% filter(quality %in% input$quality_input ) %>% ggplot(aes(alcohol)) +
      geom_bar(aes(fill=factor(quality)))+
      xlab("alcohol")+
      ylab("count")
  })
  
}

shinyApp(ui, server)
