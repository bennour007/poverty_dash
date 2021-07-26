library(shiny)
library(shinydashboard)
library(gt)

source('resources.R')
source('design.R')

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(
    title = "poverty femtella"
  ),
  dashboardSidebar(
    selectInput("state", "gouvernerat", states)
  ),
  dashboardBody(
    ##
    ## paragraph box in first row
    ##
    fluidRow(
      column(
        6,
        box(
          width = NULL,
          h3("This data has been collected from the INS report on poverty in Tunisia"),
          h5("We aim to check the relation between the dropout rates and the poverty rates in each county and state."),
          h5("You can use the side bar in your left to change what state you are in and check each county.")
        )
      ),
      column(
        3,
        box(
          solidHeader = T,
          title = 'National Averages',
          width = NULL,
          fluidRow(
            infoBox(
              width = NULL,
              title = "secondary", 
              value = "7.37 %"
              )
          ),
          fluidRow(
            infoBox(
              width = NULL,
              title = "primary", 
              value = "0.43 %"
              )
          ),
          fluidRow(
            infoBox(
              width = NULL,
              title = 'poverty', 
              value = "18.15 %")
          )
        )
      ),
      column(
        3,
        box(
          width = NULL,
          title = "This state's averages",
          infoBoxOutput(
            width = NULL,
            "secondary"
          ),
          infoBoxOutput(
            width = NULL,
            "primary"
          ),
          infoBoxOutput(
            width = NULL,
            "poverty"
            
          )
        )
      )
    ),
    ##
    ## Table, plot and figures in second row following column wise layout
    ##  
    fluidRow(
      column(
        4,
        box(
          gt_output("table"),
          width = NULL
        )
      ),
      column(
        8,
        box(
          plotOutput("plot"),
          width = NULL
        )
      )
    )
  )
)





server <- function(input, output){
  
  output$table <- render_gt({
    data_states[[input$state]] %>% 
      rename(
        'primary' = 'dropout_rate_primary',
        'secondary' = 'dropout_rate_secondary',
        'total' = 'dropout_rate_ps'
      ) %>%
      select(-state) %>% 
      gt() %>%
      tab_spanner(
        label = 'Drop out rate',
        columns = c('primary', 'secondary', 'total')
      ) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = "right",
            color = "black",
            weight = px(3)
          )
        ),
        locations = cells_body(
            columns = county
          
        )
      ) %>%
      tab_source_note("@Bennour007sin | DATA: National Institute of Statistics(INS)") %>%
      tab_header(
        title = md("**Dropout rates and Poverty rates in the selected State**"),
        subtitle = "count-wise drop out rate for primary and secondary schools and poverty"
      )
  })
  
  output$primary <- renderInfoBox({
    tmp <- data_states[[input$state]]
    infoBox(
      fill = T,
      "primary",
      paste0(
        mean(tmp$dropout_rate_primary)%>% 
          round(., 2),
        "%"
        )
    )
  })
  
  output$secondary <- renderInfoBox({
    tmp <- data_states[[input$state]]
    infoBox(
      fill = T,
      "secondary",
      paste0(
        mean(tmp$dropout_rate_secondary) %>% 
          round(., 2),
        "%"
      )
    )
  })
  
  output$poverty <- renderInfoBox({
    tmp <- data_states[[input$state]]
    infoBox(
      fill = T,
      "poverty",
      paste0(
        mean(tmp$poverty) %>% 
          round(., 2),
        "%"
      )
    )
  })
  
  output$plot <- renderPlot({
    data_states[[input$state]] %>%
      pivot_longer(
        cols = dropout_rate_primary:dropout_rate_ps,
        names_to = "dropout",
        values_to = "value"
      ) %>% 
      mutate(
        dropout = str_remove_all(dropout, "dropout_rate_"),
        dropout = if_else(dropout == 'ps', "total", dropout)
      ) %>% 
      ggplot(
        aes(x = poverty,
            y = value)
      ) +
      geom_point() +
      geom_smooth(method = 'lm') +
      facet_grid(~dropout) +
      labs(
        title = "How much does poverty rates affects the dropout rate?",
        subtitle = "We can't really know if its a causation relationship or not \nThe idea to see how do they both behave together.",
        y = "Dropout rate",
        x = "Poverty",
        caption = "@Bennour007sin | DATA: National Institute of Statistics(INS)"
      ) +
      plot_theme
  }
  )
}

shinyApp(ui, server)
