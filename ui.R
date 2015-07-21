library(shiny)
library(DT)
library(ggplot2)
library(rCharts)
library(rmarkdown)

shinyUI(navbarPage(
    "Impact of Weather on Population Health and Economy",
    tabPanel(
        "Explore Data",
        sidebarPanel(
            sliderInput(
                "timeRange",
                "Date Range (Years):",
                min = 2001,
                max = 2011,
                sep = "",
                value = c(2006, 2011)
            ),
            numericInput(
                'top_n', 'Select the top', 5, min = 1, max = 10, step = 1
            )
        ),
        mainPanel(
            "Use the date range on the left for data to be included.  Click on Help above for more details.",
            h2("Impact On Population Health"),
            dataTableOutput(outputId = "healthTable"),
            plotOutput('health_plot'),
            h2("Impact on Economy"),
            dataTableOutput(outputId = "econTable"),
            plotOutput('econ_plot')
            
        )
    ),
    
    tabPanel("Help",
             mainPanel(includeMarkdown("help.md"))
             )
    )
)