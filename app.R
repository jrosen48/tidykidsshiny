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
    
    theme = shinytheme("united"),
    
    titlePanel("State-by-State Spending on Kids Dataset Visualization"),
    
    sidebarLayout(sidebarPanel(
        selectInput("var",
                    "Variable:",
                    choices = variables,
                    selected = variables[1]),
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
        tabsetPanel(
            tabPanel("Output", 
                     p(),
                     plotOutput("plot"),
                     p("Right-click on the above plot to save an iamge of this plot"),
                     p(),
                     downloadButton("downloadData", "Download data used to create this plot")
            ),
            tabPanel("Codebook", DT::dataTableOutput("dt_table"))
        ),
        width = 6)
    )
)

server <- function(input, output) {
    
    output$dt_table <- DT::renderDataTable(tidykids_data_dictionary, options = list(pageLength = 35))
    
    prep_dataset <- reactive({
        
        d <- tidykids[tidykids$variable == input$var, ]
        
        d <- d[d$state %in% input$state, ]
        
        d <- rename(d, var = input$var_type)
        
    })
    
    output$plot <- renderPlot({
        
        d <- prep_dataset()
        
        p <- d %>% 
            ggplot(aes(x = year,  y = var, color = state, group = state)) +
            geom_point() +
            geom_line() +
            xlab(NULL) +
            ylab("$1,000s") +
            ggtitle(input$var) +
            hrbrthemes::theme_ipsum() +
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
        
        if (input$var_type == "inf_adj_perchild") {
            p + ylab("$1,000s per child")
        }
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$var, ".csv", sep = "")
        },
        content = function(file) {
            d <- prep_dataset()
            write_csv(d, path = file)
        }
    )
}

shinyApp(ui = ui, server = server)
