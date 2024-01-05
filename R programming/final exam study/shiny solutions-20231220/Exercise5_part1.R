library(shiny)

ui <- fluidPage(
  
  titlePanel("Exercise 5"),
  
  textInput(inputId = "first_name",
            label = "Enter first name"),
  
  textInput(inputId = "last_name",
            label = "Enter last name"),
  
  numericInput(inputId = "height",
               label = "Enter approximate height (cm)",
               value=1),
  
  textOutput(outputId = "message1"),
  
  textOutput(outputId = "message2")
  
)

server <- function(input, output, session) {
  
  output$message1 <- renderText({
    paste0("Hello ", input$first_name, " ", input$last_name)
  })
  
  output$message2 <- renderText({
    paste0("You are approximately ", input$height, "cm tall.")
  })
  
}

shinyApp(ui, server)