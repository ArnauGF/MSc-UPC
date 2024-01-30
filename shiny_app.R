library(shiny)
library(ggplot2)
library(DT) #we use DT library for display fancy tables
setwd("/Users/arnaugarcia/Desktop/Q1/R i SAS/SAS/practical_work")
nba<-read.csv("nbaallelo.csv")

# UI components
ui <- fluidPage(
  titlePanel("NBA ELO rating system"),
  
  sidebarLayout(
    sidebarPanel(
      # Select variable from the dataset
      selectInput(inputId = "var", label = "Select a Variable", choices = colnames(nba)),
      helpText("Choose one of the variables from the list.")
    ),
    
    mainPanel(
      # Display the histogram
      plotOutput(outputId = "histogram"),
      br(),
      # Display the summary table
      DTOutput("summary_table")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Reactive expression to filter the dataset based on selected variable
  generate_data <- reactive({
    nba[, input$var]
  })
  
  # Render the histogram based on the selected variable, we take into account the 
  #class of the variable
  output$histogram <- renderPlot({
    if(is.numeric(generate_data())){
      ggplot(data = data.frame(x=generate_data()), aes(x))+
        geom_histogram()
    } else {
      ggplot(data = data.frame(x=generate_data()), aes(x)) +
        geom_bar() 
    }
  })
  
  
  # Render the summary table based on the selected variable
  output$summary_table <- renderDT({
    summary_data <- summary(generate_data())
    datatable(as.matrix(summary_data), options = list(pageLength = 6))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
