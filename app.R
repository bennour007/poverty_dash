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


files <- read_rds("data_clean/names_of_files.rds")

states <- files[1:24]
details <- files[25:26]

data_states <- map(states, function(x) paste("data_clean/",x,".csv", sep = "")) %>%
    map(., read_csv) 

data_details <- map(details, function(x) paste("data_clean/",x,".csv", sep = "")) %>%
    map(., read_csv) 

names(data_states)  <- states
names(data_details) <- details
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Poverty, Education, Dropouts, DATA!"),
    h3("This data has been collected from the INS report on poverty in Tunisia"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(2),
        column(8,
               plotOutput("plot")
        ),
        column(2)
    ),
    
    fluidRow(
        column(1),
        column(2,
               h3("Choose your state"),
               h5("This will change the plot, the table you see, and the summary."),
               radioButtons("state", "gouvernerat", 
                            choices  = states)
        ),
        
        # Show a plot of the generated distribution
        column(5,
               h3("Data from INS report: poverty and dropout rates"),
               h5("County wise data"),
               tableOutput("table")
        ),
        column(3,
               h3("Summary statistics of the chosen state"),
               h5("Try changing the states in the sidebar, to see the changes in the summary."),
               verbatimTextOutput("stats_dp"),
               verbatimTextOutput("stats_ds"),
               verbatimTextOutput("stats_p")
        ),
        column(1)
    ),
    h5("Work will be updated, this is not a final version.")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$table <- renderTable({
        data_states[[input$state]]
    })
    output$stats_dp <- renderText({
        paste("Primary schools Dropout rate in", input$state, "Average:",
              data_states[[input$state]]$dropout_rate_primary %>% 
                  mean() %>%
                  round(., 2))
    })
    output$stats_ds <- renderText({
        paste("Secondary schools Dropout rate in", input$state, "Average:",
              data_states[[input$state]]$dropout_rate_secondary %>% 
                  mean() %>%
                  round(., 2))
    })
    output$stats_p <- renderText({
        paste("Poverty rate in", input$state, "Average:",
              data_states[[input$state]]$poverty %>% 
                  mean() %>%
                  round(., 2))
    })
    
    output$plot <- renderPlot({
        data_states[[input$state]] %>%
            ggplot(aes(x = poverty,
                       y = dropout_rate_secondary)) +
            geom_point() +
            geom_smooth() +
            labs( title = "How much does poverty rates affects the dropout rate?",
                  subtitle = "We can't really know if its a causation relationship or not, but the idea to see how do they both behave together.",
                  y = "Dropout rate",
                  x = "Poverty") +
            theme_light()
    }, res = 96
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
