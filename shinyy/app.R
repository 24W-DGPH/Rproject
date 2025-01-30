



#########

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load dataset 
df <- read.csv("df.csv")  

# Convert necessary columns
df$Occupation <- as.factor(df$Occupation)
df$Age <- as.numeric(df$Age)  # Ensure Age is numeric

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Quality of Sleep by Age Group and Occupation"),
  
  # Sidebar layout with slider input for age bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of Age Groups (Bins):",
                  min = 2,  # Minimum number of bins
                  max = 10, # Maximum number of bins
                  value = 5) # Default number of bins
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
    
    # Create dynamic age bins
    bin_breaks <- seq(min(df$Age, na.rm = TRUE), 
                      max(df$Age, na.rm = TRUE), 
                      length.out = input$bins + 1)
    
    df$Age_Group <- cut(df$Age, breaks = bin_breaks, 
                        include.lowest = TRUE, 
                        labels = paste0("Group ", 1:input$bins))
    
    # Create the bar plot
    ggplot(df, aes(x = Age_Group, fill = Occupation)) +
      geom_bar() +
      labs(title = "Quality of Sleep Across Age Groups and Occupations",
           x = "Age Group",
           y = "Count",
           fill = "Occupation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)







