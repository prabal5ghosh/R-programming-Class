library(shiny)

ui <- fluidPage(
  
  titlePanel("Exercise 5"),
  
  selectInput(inputId = "gender",
              label = "Select gender",
              choices = c("Mister", "Madame")),
  
  textInput(inputId = "first_name",
            label = "Enter first name"),
  
  textInput(inputId = "last_name",
            label = "Enter last name"),
  
  sliderInput(inputId = "height",
              label = "Choose approximate height (cm)",
              value=1,
              min = 1,
              max = 250),
  
  radioButtons(inputId = "age",
               label = "Choose age bracket",
               choices = c("1-10","11-20","21-30","31-40","41-50",
                           "51-60", "61-70", "71-80", "81-90", "90+"),
               #inline=TRUE to display buttons horizontally
               inline = TRUE),
  
  textOutput(outputId = "message1"),
  
  textOutput(outputId = "message2"),
  
  textOutput(outputId = "message3")
  
)

server <- function(input, output, session) {
  
  output$message1 <- renderText({
    paste0("Hello ", input$gender, " ", input$first_name, " ", input$last_name)
  })
  
  output$message2 <- renderText({
    paste0("You are approximately ", input$height, "cm tall.")
  })
  
  output$message3 <- renderText({
    paste0("Your age is in the range ", input$age, ".")
  })
  
}

shinyApp(ui, server)