#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analysis of the wine quality dataset"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
         
            fileInput("file", "Choose CSV File", accept = ".csv"),            
            textInput("title1", "Scatterplot title", "This is the first title"),
            uiOutput("quality_score"),
            uiOutput("sel_var1"),
            textInput("title2", "Boxplot title", "This is the second title"),
            
            uiOutput("sel_var2"),

            
            sliderInput("freesulfurdioxide", "Select the range of free sulfur dioxide", min = 1, max = 72, value = c(7,9)),
            actionButton("rows_select", "Click to show descriptive statistics"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          plotOutput("scat"),
          plotOutput("box"),
          
          verbatimTextOutput("stats"),
          
           dataTableOutput("head")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data = reactive({
    data <- input$file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(data$datapath, sep = ";", header = T)
  })
  
  output$quality_score <- renderUI({
    checkboxGroupInput("qual", "Choose the quality", choices = sort(unique(data()$quality)), selected = 5)
    
  })
  
  quality = reactive({data()%>%dplyr::filter(quality %in% input$qual)})
  
  output$sel_var1 <- renderUI({
    selectInput("boxplot_var", "Choose a variable for the boxplot", choices = names(data()), selected = "alcohol")
  })
  
  output$sel_var2 <- renderUI({
  checkboxGroupInput("variables", "Which variables you want to display?", choices = names(data()), selected =c("free.sulfur.dioxide"))
  })
  
  variables = reactive({
    req(input$variables)
    data()%>%select(input$variables)%>%dplyr::filter(free.sulfur.dioxide >= input$freesulfurdioxide[1],free.sulfur.dioxide <= input$freesulfurdioxide[2] )
    })
  
    output$head <- renderDataTable({
      variables()
    })
    
    output$stats <- renderPrint({
      if (input$rows_select) {summary(variables())}
    })
    output$scat <- renderPlot({
      col = as.factor(quality()[,'quality'])
      quality()%>% ggplot()+geom_point(aes(citric.acid, pH, color =col))+
       labs(color = "Quality score", title = input$title1) 
   
    })
    output$box <- renderPlot({
      col = as.factor(quality()[,'quality'])
      
      quality() %>% ggplot(aes(factor(quality), y = quality()[,input$boxplot_var], color = col)) +
        geom_boxplot()+labs(color = "Quality score", title = input$title2, y = input$boxplot_var) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
