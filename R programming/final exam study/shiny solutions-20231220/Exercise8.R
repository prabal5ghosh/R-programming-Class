library(shiny)

# /!\ path to change 


# setwd("/Users/justine_labory/Desktop/Cours_Master/R/Cours4_Notebook_and_Shiny/")
# advs <- read.csv("advs.csv")

advs<- read.csv('C://Users//praba//Desktop//uca1//M1//R programming//15th dec//advs.csv')


ui <- fluidPage(
  
  titlePanel("Exercise 8 : To do at home"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "x_var",
                  label = "Select x variable",
                  choices = sort(names(advs))),
      
      selectInput(inputId = "y_var",
                  label = "Select y variable",
                  choices = sort(names(advs))),
      
      selectInput(inputId = "subject",
                  label = "Select subject",
                  choices=unique(advs$USUBJID)),
      
      selectInput(inputId = "param",
                  label = "Select param",
                  choices = unique(advs$PARAM))
      
    ),
    
    mainPanel(
      
      #tableOutput("file")
      
      plotOutput("plot")
    )
  )
  
)

server <- function(input, output, session) {
  
  # data <- reactive(advs %>%
  #                    filter(.data[["USUBJID"]] == input$subject & .data[["PARAM"]] == input$param))
  
  data <- reactive({advs %>%
                     filter(USUBJID == input$subject & PARAM == input$param)})
  
  output$plot <- renderPlot({
    data() %>%
      ggplot(aes(x=.data[[input$x_var]],y=.data[[input$y_var]])) +
      geom_point()
  })
  
}

shinyApp(ui, server)

