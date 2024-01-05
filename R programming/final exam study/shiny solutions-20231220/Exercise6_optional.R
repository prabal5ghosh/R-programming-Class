library(shiny)
library(tidyverse)
data("iris")

ui <- fluidPage(
  
  titlePanel("Exercise 6 : Shiny app with iris dataset"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "attribute",
                   label = "Select attribute to plot",
                   choices=names(iris)[-5]),
      
      textInput(inputId = "plot_name",
                label = "Enter plot name"),
      
      numericInput(inputId = "nb_row",
                   label = "Number of rows in table",
                   value = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
        title = "boxplot", 
        plotOutput(outputId = "boxplot")#, width = 400, height = 400)
      ),
      
      tabPanel(
        title = "iris_table", 
        tableOutput(outputId = "iris_table")
      ),
      
      tabPanel(
        title = "summary", 
        textOutput(outputId = "mean"),
        textOutput(outputId = "sd")
      ))
      
    )
  )
)

server <- function(input, output, session) {
  
  output$boxplot <- renderPlot({
    iris %>%
      ggplot(aes(x=Species,y=.data[[input$attribute]],fill=Species)) +
      geom_boxplot(show.legend = FALSE) +
      ggtitle(input$plot_name)
  })
  
  output$iris_table <- renderTable({
    head(iris,input$nb_row)
    })
  
  output$mean <- renderText({
    paste0("Mean of  ", input$attribute, " : ", round(mean(iris[[input$attribute]]),2))
  })
  
  output$sd <- renderText({
    paste0("Standard deviation of  ", input$attribute, " : ", round(sd(iris[[input$attribute]]),2))
  })
  
}

shinyApp(ui, server)