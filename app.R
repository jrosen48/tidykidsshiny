#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidykids)
library(dplyr)
library(ggplot2)
library(shinythemes)

variables <- tidykids::tidykids %>% 
    distinct(variable) %>% 
    pull(variable)

states <- tidykids::tidykids %>% 
    distinct(state) %>% 
    pull(state)

ui <- fluidPage(
    
    titlePanel("State-by-State Spending on Kids Dataset Visualization"),
    
    sidebarLayout(sidebarPanel(
        selectInput("var",
                    "Variable:",
                    choices = variables,
                    selected = variables[1]),
        p(),
        tags$a(href = "https://jrosen48.github.io/tidykids/articles/tidykids-codebook.html", "View variable codebook"),
        p(),
        selectInput("var_type",
                    "Variable Transformation",
                    choices = c("None (USD)" = "raw", "USD Inflation-Adjusted" = "inf_adj",
                                "USD in $1,000s Inflation-Adjusted, Per Child" = "inf_adj_perchild"),
                    selected = "inf_adj_perchild"),
        selectInput("model",
                    "Model:",
                    choices = c("None", "Linear", "Quadratic", "Cubic", "Smooth"),
                    selected = "None"),
        checkboxGroupInput("state",
                           "States:",
                           choices = states,
                           selected = c("Tennessee", "Michigan", "North Carolina"))
    ),
    mainPanel(
        plotOutput("plot"),
        hr(),
        p("The data was made available by and is attributable to Julia Isaacs, Eleanor Lauderback, and Erica Greenberg under the ODCAttributionLicense: https://opendatacommons.org/licenses/by/1-0/. The dataset is available here: https://datacatalog.urban.org/dataset/state-state-spending-kids-dataset"),
        p("This data is accessed via the tidykids R data package: https://jrosen48.github.io/tidykids/"),
        p("Source: https://github.com/jrosen48/tidykidsshiny/blob/master/app.R"),
        hr(),
        width = 6
    )
    )
)

server <- function(input, output) {
    
    output$plot <- renderPlot({
        
        d <- tidykids[tidykids$variable == input$var, ]
        
        d <- d[d$state %in% input$state, ]
        
        d <- rename(d, var = input$var_type)
        
        p <- d %>% 
            ggplot(aes(x = year,  y = var, color = state, group = state)) +
            geom_point() +
            geom_line() +
            xlab(NULL) +
            ylab("$1000s") +
            ggtitle(input$var) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_y_continuous(label = scales::comma) +
            theme(text = element_text(size = 15))
        
        if (input$model == "Linear") {
            p + geom_smooth(method = "lm", se = FALSE)
        } else if (input$model == "Quadratic") {
            p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) 
        } else if (input$model == "Cubic") {
            p + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) 
        } else if (input$model == "Smooth") {
            p + geom_smooth(se = FALSE)
        } else {
            p
        }
        
    })
}

shinyApp(ui = ui, server = server)
