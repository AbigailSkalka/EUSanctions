
#Loading necessary packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(rstanarm)
library(broom.mixed)

#Reading in objects I created in Rmd documents

EU_US_full <- read_rds("EU_US.rds")
EU_data <- read_rds("EUdataframe_01.rds")
US_joined<- read_rds("US_joined.rds")

fit_model <- stan_glm(formula = both ~ Programme, 
                      data = EU_US_full,
                      refresh = 0)

model_tidy <- print(fit_model, digits = 5) %>%
    tidy()

count_data <- EU_data %>%
    select(born_in_eu) %>%
    group_by(born_in_eu) %>%
    summarize(Count = n()) %>%
    arrange(desc(born_in_eu)) %>%
    rename("Born in EU" = born_in_eu) 

birthplot <- EU_data %>%
    filter(born_in_eu == "Yes") %>%
    ggplot(aes(x = Birt_country)) +
    geom_histogram(stat = "count",
                   fill = "navyblue") +
    labs(title = "Which EU Member States were Sanctionees Born in?",
         x = "Country",
         y = "Count") +
    theme_bw() 

dateplot <- ggplot(EU_data, aes(x = Leba_publication_date)) +
    geom_density(aes(y = after_stat(count/sum(count)))) +
    labs(title = "Proportion of Sanctions by Date",
         x = "Date",
         y = "Proportion") +
    theme_bw() 

genderplot <- EU_data %>%
    drop_na(Naal_gender) %>%
ggplot(aes(x = Naal_gender)) +
    geom_histogram(stat = "count",
                   fill = "navyblue") +
    labs(title = "How Many Men Are Sanctioned? How Many Women?",
         x = "Country",
         y = "Count") +
    theme_bw() 


# Define UI for application that draws a histogram
ui <- navbarPage(
    "EU Sanctions 2002 - 2020",
    tabPanel("Comparison",
             fluidPage(theme = shinytheme("sandstone"),
                 titlePanel("How do EU Sanctions Compare to the US?"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("EU" = "a", "US" = "b")
                         )),
                     mainPanel(plotOutput("line_plot"),
                               p("I expected to see more similarities between
                                 the types of entities that the EU sanctions 
                                 and those that the US does, but this data shows
                                 that far more of the US sanctions are devoted to
                                 narcotics, while the lion's share of EU sanctions
                                 focus on terror.  It can also be confusing as there 
                                 is overlap in some of these categories: sanctions under
                                 the North Korea program may also refer to nuclear 
                                 proliferation, for instance.")))
             )),
    tabPanel("Data", 
             titlePanel("Curious Data Points"),
             h3("Place of Birth"),
             sidebarPanel(tableOutput("born_in_eu")),
                 mainPanel(p("Interestingly, the EU sanctions certain people
                             who were actually born in one of its member states.
                             This table shows that distribution (Former refers
                             to Britain, which was a member of the EU until the
                             Brexit referendum was finalized in 2020).  Below I
                             have included a plot of which EU countries are
                             most represented among those where sanctionees were
                             born."),
                           plotOutput("birthplot")),
             h3("Gender Breakdown"),
             mainPanel(plotOutput("genderplot"),
                       p("This plot shows the number of women sanctioned versus
                         men.")),
             h3("Sanctions Over Time"),
             mainPanel(plotOutput("dateplot"),
                       p("This plot shows the proportion of EU sanctions by date.
                         It is neither a steady rate nor a distinct linear
                         increase over time, which are the two possibilities
                         I hypothesized.  Instead, it peaks somewhere in the 
                         middle."))),
    tabPanel("Model", 
                      titlePanel("Regression Model"),
                      mainPanel(
                      p("I wanted to see if the program under which a person or
                        entity was santioned made a difference in the likelihood
                        of that entry appearing in both the EU and the US datasets.
                        I used a linear model with presence in both lists as the
                        dependent variable and program as the independent one.
                        Below you will find a table summarizing my results."),
                      p("There are 45 parameters, all a variation of the 'Program' 
                        variable.  Each has varying impact on the likelihood of
                        an entry appearing in both data sets."),
                      tableOutput("model_tidy"))),
    tabPanel("Discussion",
             titlePanel("My Modeling Choices"),
             p("The most labor-intesive part of this project was the data cleaning.  
               This may be unsurprising to those of you familiar with bureaucracy, 
               but neither the EU nor the US formatted their data in a way 
               that was at all conducive to use in this projecct.  Most of the
               column names were far from intuitive and a lot of the inputs needed
               to be recoded to make any meaningful sense."),
             p("I was interested in determining if variables like country of 
               origin or program might be useful in predicting whether a person 
               would be sanctioned by BOTH the EU and the US, so I first joined 
               the two US datasets together (because they were separated because,
               like I said, bureaucracy is a nightmare) and then I used a full join
               to combine the new US dataset with the EU one and create a 'both' column
               which was a logical with output 0 if the name appeared in only one
               list, and 1 if it appeared in both.  Then I used a regression 
               analysis to examine the impact of independent variables on the
               dependent variable, 'both'."),
             p("I also wanted to make a few plots to demonstrate overall trends.
               I chose the information that was most interesting to me when
               I was determining what to show in these plots.  I have 
               comparative plots showing the number of sanctionees by program
               in both the EU and the US, as well as a graph showing the number
               of EU sanctionees who were born in an EU member-state.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My project looks at the people and entities that have been subject to 
             financial sanctions by the European Union since 2002.  I aim to 
             identify patterns in the types of people that are sanctioned.  
             For instance, what countries do they hail from?  
             What did they do to warrant sanctioning?  
             Furthermore, I use the United States consolidated sanctions list
             to provide a lens of comparison.  Looking at the data sets
             side-by-side, I am able to get a better sense of what the EU 
             prioritizes when deciding on whom to implement sanctions versus
             the considerations that the US makes."),
             h3("Data Sources"),
             p("Data for this project comes from the Consolidated List of Persons
             and Entities Subject to EU Financial Sanctions, the US Specially 
             Designated Nationals List, and the US Consolidated Sanctions List.
             The GitHub repository for this project is linked here:
             https://github.com/AbigailSkalka/FinalProjWork."),
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
              group_by(Programme) %>%
              summarize(Count = n(), .groups = "drop") %>%
              mutate(Type = Programme) %>%
              unnest(Type),
            
            # If input$plot_type is "b", plot histogram of "Programme" column
            # from the EU_data dataframe
            
            x   <- US_joined %>%
              group_by(Programme) %>%
              summarize(Count = n(), .groups = "drop") %>%
              mutate(Type = Programme)
        )
        
        # Draw the histogram with the specified number of bins
        
        ggplot(x, aes(x = Type, y = Count)) +
                 geom_bar(stat = "identity", 
                          fill = 'navyblue', 
                          border = 'white') +
          coord_flip() +
            scale_x_discrete()
    })
    
    output$born_in_eu <- renderTable(count_data)
    output$birthplot <- renderPlot(birthplot)
    output$model_tidy <- renderTable(model_tidy)
    output$genderplot <- renderPlot(genderplot)
    output$dateplot <- renderPlot(dateplot)
}
# Run the application 
shinyApp(ui = ui, server = server)
