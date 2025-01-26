


library(shiny)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Quality of Sleep by Age Group and Occupation"),
  
  # Sidebar layout with slider input for age bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of Age Groups (Bins):",
                  min = 25,  # Minimum number of bins
                  max = 60, # Maximum number of bins
                  value = 8) # Default number of bins
    ),
    
    # Main panel to show the plot
    mainPanel(
      plotOutput("sleepPlot") # Output the plot
    )
  )
)




# Define server logic
server <- function(input, output) {
  
  output$sleepPlot <- renderPlot({
    # Group ages into bins based on the slider input
    df$Age <- cut(df$Age, 
                          breaks = input$bins, 
                          labels = paste( 1:input$bins))
    
    # Create the bar plot
    ggplot(df, aes(x = Age, fill = Occupation)) +
      geom_bar() +
      labs(title = "Quality of Sleep Across Age Groups and Occupations",
           x = "Age Groups",
           y = "Quality.of.sleep_Count",
           fill = "Occupation") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)



