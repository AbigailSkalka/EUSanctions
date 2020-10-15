#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
data <- read_delim("20201009-FULL-1_0.csv", delim = ";")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Model",
             fluidPage(
                 titlePanel("EU Sanctions Data"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Abigail Skalka and I am a masters student in the 
             Regional Studies program on Russia, Eastern Europe, and Central Asia. 
             You can reach me at abigailskalka@g.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- data[, 3],
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 7]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
