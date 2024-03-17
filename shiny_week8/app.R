library(shiny)
library(tidyverse)

# Define the user interface
ui <- fluidPage(
  titlePanel("Mean Scores Analysis"),
  
  # Sidebar layout for inputs and plot display
  sidebarLayout(
    sidebarPanel(
      # Input: Select gender (Male, Female, All)
      selectInput("gender", "Gender", 
                  choices = c("All", "Male", "Female"), 
                  selected = "All", 
                  # Explains the purpose of the input for gender selection
                  # Why: Allows dynamic filtering of the plot based on gender
      ),
      
      # Input: Toggle for displaying error band in plot
      selectInput("errorBand", "Error Band", 
                  choices = c("Display Error Band", "Suppress Error Band"), 
                  selected = "Display Error Band",
                  # Explains the purpose of the input for error band display
                  # Why: Provides user control over plot aesthetics
      ),
      
      # Input: Checkbox for including data before July 1, 2017
      checkboxInput("includeBeforeJuly", "Include Before July 1, 2017", TRUE,
                    # Explains the purpose of the checkbox for date filtering
                    # Why: Offers an option to refine data analysis scope
      )
    ),
    
    mainPanel(
      # Output: Display scatterplot
      plotOutput("scatterPlot",
                 # Explains the purpose of the plot output
                 # Why: Visualizes the dynamic relationship based on user inputs
      )
    )
  )
)

server <- function(input, output) {
  # Load the dataset 
  data <- readRDS("./data.rds")
  
  # Reactive expression to filter data based on inputs
  filteredData <- reactive({
    df <- data
    if (input$gender != "All") {
      df <- df %>% filter(gender == input$gender)
    }
    if (!input$includeBeforeJuly) {
      df <- df %>% filter(timeEnd >= as.Date("2017-07-01"))
    }
    df
  })
  
  # Render the scatterplot based on filtered data
  output$scatterPlot <- renderPlot({
    df <- filteredData()
    p <- ggplot(df, aes(x = avg16, y = avg810)) + geom_point()
    if (input$errorBand == "Display Error Band") {
      p <- p + geom_smooth(method = "lm", color = "purple", alpha = 0.2)
    }
    p
  })
}

shinyApp(ui = ui, server = server)

