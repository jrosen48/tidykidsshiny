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

variables <- tidykids::tidykids %>% 
    distinct(variable) %>% 
    pull(variable)

states <- tidykids::tidykids %>% 
    distinct(state) %>% 
    pull(state)

ui <- fluidPage(

    titlePanel("State-by-State Spending on Kids Dataset Visualization"),

    sidebarLayout(
        sidebarPanel(
            selectInput("var",
                        "Variable:",
                        choices = variables,
                        selected = variables[1]),
            checkboxGroupInput("state",
                          "States:",
                          choices = states,
                          selected = states[1])
        ),
        mainPanel(
           plotOutput("plot"),
           p("The data was made available by and is attributable to Julia Isaacs, Eleanor Lauderback, and Erica Greenberg under the under the ODCAttributionLicense (https://opendatacommons.org/licenses/by/1-0/"),
           p("This data is accessed via the tidykids R package: https://jrosen48.github.io/tidykids/")
           )
    )
)

server <- function(input, output) {

    output$plot <- renderPlot({

        d <- tidykids[tidykids$variable == input$var, ]
        
        d <- d[d$state %in% input$state, ]
        
        d %>% 
            ggplot(aes(x = year,  y = inf_adj_perchild, color = state, group = state)) +
            geom_point() +
            geom_line() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_color_brewer(type = "qual") +
            xlab(NULL) +
            ylab("$1000s")
    })
}

shinyApp(ui = ui, server = server)
