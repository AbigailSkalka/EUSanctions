#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
EU_data <- read_rds("EUdataframe_01.rds")
US_data_consolidated <- read_rds("USConsolidated.rds")


# Define UI for application that draws a histogram
ui <- navbarPage(
    "EU Sanctions 2002 - 2020",
    tabPanel("Model",
             fluidPage(theme = shinytheme("sandstone"),
                 titlePanel("Model: Who is Subject to EU Sanctions?"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Subject Type" = "a", "Programme" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Comparison", 
             titlePanel("How do EU Sanctions Compare to the US?"),
             #sidebarLayout(
                 #sidebarPanel(
                     #selectInput(
                         #"plot_type",
                         #"Plot Type",
                         #c("Subject Type" = "c", "Programme" = "d")
                     #)),
                 #mainPanel(plotOutput("line_plot")))
    ),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices I made and 
              an explanation of why I made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My project looks at the people and entities that have been subject to 
             financial sanctions by the European Union since 2002.  I aim to 
             identify patterns in the types of people that are sanctioned.  
             For instance, what countries do they hail from?  What languages
             do they speak?  What did they do to warrant sanctioning?  
             Furthermore, I use the United States consolidated sanctions list
             to provide a lens of comparison.  Looking at the data sets
             side-by-side, I am able to get a better sense of what the EU 
             prioritizes when deciding on whom to implement sanctions versus
             the considerations that the US makes."),
             h3("About Me"),
             p("My name is Abigail Skalka and I am a masters student in the
             Regional Studies program on Russia, Eastern Europe and Central Asia. 
             You can reach me at abigailskalka@g.harvard.edu.")))

server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "Subject_type" column 
            # from the EU_data dataframe
            
            x   <- EU_data %>%
              group_by(Subject_type) %>%
              summarize(Count = n(), .groups = "drop") %>%
              mutate(Type = Subject_type),
            
            # If input$plot_type is "b", plot histogram of "Programme" column
            # from the EU_data dataframe
            
            x   <- EU_data %>%
              group_by(Programme) %>%
              summarize(Count = n(), .groups = "drop") %>%
              mutate(Type = Programme)
        )
        
        # Draw the histogram with the specified number of bins
        
        ggplot(x, aes(x = Type, y = Count)) +
                 geom_bar(stat = "identity", 
                          fill = 'navyblue', 
                          border = 'white') +
          coord_flip()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
