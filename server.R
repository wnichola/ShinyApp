library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(lubridate)
library(ggplot2)
library(rCharts)

# source("data_preparation.R")
load(file="./data/storm_data_clean.RData")

shinyServer(
    function(input, output) {
        storm_range <- reactive({
            getRange(storm_data_clean, 
                     input$timeRange[1],
                     input$timeRange[2])
            })
        
        storm_summary0 <- reactive({
            group_by_evtype(storm_range())
            })
        
        # Analysing top Events in terms of Total_Hlth_Impact
        storm_sum_health <- reactive({
            arrange_top_health(storm_summary0(), input$top_n)
            })
        
        # Analysing top Events in terms of Total_Econ_Impact
        storm_sum_econ <- reactive({
            arrange_top_econ(storm_summary0(), input$top_n)
            })
        
        # Generate Output
        output$healthTable <- 
            renderDataTable({
                displayTopHealth(storm_sum_health(), input$top_n)
                })
        
        output$econTable <- 
            renderDataTable({
                displayTopEcon(storm_sum_econ(), input$top_n)
                })
        
        output$health_plot <- renderPlot({
            print(plot_health(storm_sum_health()))
        })
#         output$health_plot <- renderPlot({
#             print(plot_health_enc(storm_data_clean, 
#                               input$timeRange[1], 
#                               input$timeRange[2],
#                               input$top_n))
#         })
        
        
        output$econ_plot <- renderPlot({
            print(plot_health(storm_sum_econ()))
        })
#         output$econ_plot <- renderPlot({
#             print(plot_econ_enc(storm_data_clean, 
#                                   input$timeRange[1], 
#                                   input$timeRange[2],
#                                   input$top_n))
#         })
    }
)

getRange <- function(dt, startYear, endYear) {
    result <- dt %>% filter(BGN_DATE_YR >= startYear, 
                            BGN_DATE_YR <= endYear)
    
    result
#     return (result)
}

group_by_evtype <- function(dt) {
    result <- dt %>% 
        group_by(EVTYPE) %>% 
        summarise(Total_Fatalities = sum(FATALITIES), 
                  Total_Injuries = sum(INJURIES),
                  Total_Hlth_Impact = sum(FATALITIES + INJURIES),
                  Total_PropDmg = sum(PROPDMG), 
                  Total_CropDmg = sum(CROPDMG),
                  Total_Econ_Impact = sum(PROPDMG + CROPDMG))

    result
#     return (result)
}

arrange_top_health <- function(dt, top_n) {
    result <- arrange(dt, desc(Total_Hlth_Impact))
    result <- result[1:top_n, ] # Take top 5 
    
    result
#     return(result)
}

arrange_top_econ <- function(dt, top_n) {
    result <- arrange(dt, desc(Total_Econ_Impact))
    result <- result[1:top_n, ] # Take top 5 
    
    result
#     return(result)
}

plot_health <- function(dt) {
    result <- ggplot(data=dt) +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Hlth_Impact/1000, 
                           fill="Total_Hlth_Impact"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Injuries/1000, 
                           fill="Total_Injuries"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Fatalities/1000, 
                           fill="Total_Fatalities"), 
                       stat="identity") +
        xlab("Event") + 
        ylab ("Pop. Health Impact (,000)") + 
        ggtitle("Pop. Health Impact by Events") +
        scale_colour_manual(name="Health_Impact", 
                            values=c("Total_Hlth_Impact" = "green", 
                                     "Total_Injuries" = "blue",
                                     "Total_Fatalities" = "red"), 
                            labels=c("Total_Hlth_Impact" = "Ttl Hlth Imp", 
                                     "Total_Injuries" = "Ttl Inj",
                                     "Total_Fatalities" = "Ttl Fataly")) +
        scale_fill_manual(name="Health_Impact", 
                          values=c("Total_Hlth_Impact" = "green", 
                                   "Total_Injuries" = "blue",
                                   "Total_Fatalities" = "red"), 
                          labels=c("Total_Hlth_Impact" = "Ttl Hlth Imp", 
                                   "Total_Injuries" = "Ttl Inj",
                                   "Total_Fatalities" = "Ttl Fataly")) +
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=90, vjust=1))

#     return(result)
#     print(result)
    result
}


plot_health_enc <- function(dt, startYear, endYear, top_n) {
    storm_sum_health <- dt %>% filter(BGN_DATE_YR >= startYear, 
                  BGN_DATE_YR <= endYear) %>%
        group_by(EVTYPE) %>% 
        summarise(Total_Fatalities = sum(FATALITIES), 
                  Total_Injuries = sum(INJURIES),
                  Total_Hlth_Impact = sum(FATALITIES + INJURIES),
                  Total_PropDmg = sum(PROPDMG), 
                  Total_CropDmg = sum(CROPDMG),
                  Total_Econ_Impact = sum(PROPDMG + CROPDMG)) %>%
        arrange(desc(Total_Hlth_Impact))[1:top_n, ]

    result <- ggplot(data=storm_sum_health) +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Hlth_Impact/1000, 
                           fill="Total_Hlth_Impact"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Injuries/1000, 
                           fill="Total_Injuries"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Fatalities/1000, 
                           fill="Total_Fatalities"), 
                       stat="identity") +
        xlab("Event") + 
        ylab ("Pop. Health Impact (,000)") + 
        ggtitle("Pop. Health Impact by Events") +
        scale_colour_manual(name="Health_Impact", 
                            values=c("Total_Hlth_Impact" = "green", 
                                     "Total_Injuries" = "blue",
                                     "Total_Fatalities" = "red"), 
                            labels=c("Total_Hlth_Impact" = "Ttl Hlth Imp", 
                                     "Total_Injuries" = "Ttl Inj",
                                     "Total_Fatalities" = "Ttl Fataly")) +
        scale_fill_manual(name="Health_Impact", 
                          values=c("Total_Hlth_Impact" = "green", 
                                   "Total_Injuries" = "blue",
                                   "Total_Fatalities" = "red"), 
                          labels=c("Total_Hlth_Impact" = "Ttl Hlth Imp", 
                                   "Total_Injuries" = "Ttl Inj",
                                   "Total_Fatalities" = "Ttl Fataly")) +
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=90, vjust=1))
    
    #     return(result)
    #     print(result)
    result
}

plot_econ <- function(dt) {
    result <- ggplot(data=dt) +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Econ_Impact/1000000000, 
                           fill="Total_Econ_Impact"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_PropDmg/1000000000, 
                           fill="Total_PropDmg"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_CropDmg/1000000000, 
                           fill="Total_CropDmg"), 
                       stat="identity") +
        xlab("Event") +
        ylab ("Econ. Impact (US $ ,000,000,000)") + 
        ggtitle("Econ. Impact by Events") +
        scale_colour_manual(name="Econ_Impact", 
                            values=c("Total_Econ_Impact" = "green", 
                                     "Total_PropDmg" = "blue",
                                     "Total_CropDmg" = "red"), 
                            labels=c("Total_Econ_Impact" = "Ttl Econ Imp", 
                                     "Total_PropDmg" = "Ttl PropDmg",
                                     "Total_CropDmg" = "Ttl CropDmg")) +
        scale_fill_manual(name="Econ_Impact", 
                          values=c("Total_Econ_Impact" = "green", 
                                   "Total_PropDmg" = "blue",
                                   "Total_CropDmg" = "red"), 
                          labels=c("Total_Econ_Impact" = "Ttl Econ Imp", 
                                   "Total_PropDmg" = "Ttl PropDmg",
                                   "Total_CropDmg" = "Ttl CropDmg")) +
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=90, vjust=1))

#     return(result)
#     print(result)
    result
}


plot_econ_enc <- function(dt, startYear, endYear, top_n) {
    storm_range <- getRange(dt, startYear, endYear)
    storm_summary <- group_by_evtype(storm_range)
    storm_sum_econ <- arrange_top_econ(storm_summary, top_n)
    result <- ggplot(data=storm_sum_econ) +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_Econ_Impact/1000000000, 
                           fill="Total_Econ_Impact"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_PropDmg/1000000000, 
                           fill="Total_PropDmg"), 
                       stat="identity") +
        geom_histogram(aes(x=EVTYPE, 
                           y=Total_CropDmg/1000000000, 
                           fill="Total_CropDmg"), 
                       stat="identity") +
        xlab("Event") +
        ylab ("Econ. Impact (US $ ,000,000,000)") + 
        ggtitle("Econ. Impact by Events") +
        scale_colour_manual(name="Econ_Impact", 
                            values=c("Total_Econ_Impact" = "green", 
                                     "Total_PropDmg" = "blue",
                                     "Total_CropDmg" = "red"), 
                            labels=c("Total_Econ_Impact" = "Ttl Econ Imp", 
                                     "Total_PropDmg" = "Ttl PropDmg",
                                     "Total_CropDmg" = "Ttl CropDmg")) +
        scale_fill_manual(name="Econ_Impact", 
                          values=c("Total_Econ_Impact" = "green", 
                                   "Total_PropDmg" = "blue",
                                   "Total_CropDmg" = "red"), 
                          labels=c("Total_Econ_Impact" = "Ttl Econ Imp", 
                                   "Total_PropDmg" = "Ttl PropDmg",
                                   "Total_CropDmg" = "Ttl CropDmg")) +
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=90, vjust=1))
    
    #     return(result)
    #     print(result)
    result
}

displayTopHealth <- function (dt, top_n) {
    healthSum <- select(dt, EVTYPE, Total_Injuries, Total_Fatalities, Total_Hlth_Impact)
    result <- datatable(healthSum, options = list(iDisplayLength = top_n))
#     return(result)
    result
}


displayTopEcon <- function (dt, top_n) {
    econSum <- select(dt, EVTYPE, Total_PropDmg, Total_CropDmg, Total_Econ_Impact)
    result <- datatable(econSum, options = list(iDisplayLength = top_n))
#     return(result)
    result
}