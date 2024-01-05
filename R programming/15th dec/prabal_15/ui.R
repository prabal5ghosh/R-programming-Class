#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram



# ui <- fluidPage(
#   
#   titlePanel('Excercise 1'),
#   textInput(inputId = "text_choice1",
#             label='Enter the first name',
#             value='peter'),
#   textInput(inputId = "text_choice2",
#             label='Enter the last name',
#             value='rabbit'),
#   
#   numericInput(inputId = 'number_choice',
#                label= 'Enter approx height(cm)',
#                value= 30),
#   textOutput(outputId ='message1'),
#   textOutput(outputId ='message2')
#   
# )
# 
# 
# server <- function(input, output){
#   output$message1 <- renderText(
#     {
#       paste0("Hello", input$text_choice1 ," ", input$text_choice2)
#     })
#     
#     output$message2 <- renderText(
#       {
#         
#         paste0(
#           "You are approx ", input$number_choice, "cm tall" )
#       }
#   )
#   
#   
# }
library(readr)
options(shiny.maxRequestSize = 10*1024^2)


ui <- fluidPage(
  titlePanel("Upolad a file"),
  
  sidebarLayout(
    sidebarPanel(
  fileInput(inputId = 'upload',
             label = 'select file'),
  numericInput(inputId= 'number_row',
               label='select the number of rows to display',
               value= 7)
),

mainPanel(
  tableOutput('files')
  
  
)

))





server <- function(input, output){
  
  
  output$files <- renderTable({
    req(input$upload)
    dataset <- read_csv(input$upload$datapath)
    head(dataset, input$number_row)
  })
}


# ui <- fluidPage(
#   titlePanel("Upolad a file"),
#   
#   sidebarLayout((
#     
#   ))
# )




shinyApp(ui=ui, server= server)

