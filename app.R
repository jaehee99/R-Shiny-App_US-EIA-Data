library(tidyverse)
library(devtools)
library(blsAPI)
library(rjson)
library(eia)
library(lubridate)
library(diffdf)
library(shiny)
library(ggplot2)
library(ggthemes)
eia_set_key("8a87a727635f5c834e2799cd76fcb820")

pull_data <- function(api_start, api_end) {
    tibble(start = api_start, abrv = state.abb, end = api_end) %>% 
        mutate(id = str_c(start, abrv, end)) %>%
        select(id) %>% 
        as.list() %>% 
        magrittr::extract2(1) %>% 
        eia_series() %>% 
        select(data) %>% 
        mutate(state = state.name) %>% 
        select(state, everything()) %>% 
        unnest(data)
}
avg_elec <- pull_data(api_start = "ELEC.PRICE.", api_end = "-IND.A") %>% 
    rename("electricity_price" = value)
emission <- pull_data(api_start = "EMISS.CO2-TOTV-EC-TO-", api_end = ".A") %>% 
    rename("carbon_emissions" = value)
customers <- pull_data(api_start = "ELEC.CUSTOMERS.", api_end = "-ALL.A") %>% 
    rename("customers" = value)
retail_sales <- pull_data(api_start = "ELEC.SALES.", api_end = "-ALL.A") %>% 
    rename("retail_sales" = value)
total_electricity <- pull_data(api_start = "ELEC.GEN.ALL-", api_end = "-99.A") %>% 
    rename("total_electricity" = value)

Full_data <- left_join(avg_elec, emission, by = c("state", "date", "year")) %>% 
    left_join(customers, by = c("state", "date", "year")) %>% 
    left_join(retail_sales, by = c("state", "date", "year")) %>% 
    left_join(total_electricity, by = c("state", "date", "year")) %>% 
    select(state, year, electricity_price, carbon_emissions, customers, retail_sales, total_electricity)
rm(avg_elec, customers, emission, retail_sales, total_electricity)

ui <- fluidPage(
    titlePanel("EIA Data Project"), 
    tabsetPanel(
        tabPanel("univiate anaylsis",
                 sidebarPanel(
                     varSelectInput("filt1",
                                    "Filter Variable",
                                    data = Full_data[c(1:2)]),
                     selectInput("filt2", "Select Filter",
                                 choices = ""),
                     varSelectInput("var1",
                                    "Plot 1 Variable",
                                    data = Full_data[3:7])),
                 mainPanel(fluidRow(10,
                                    column(5,
                                           plotOutput("plot1")),
                                    column(5,
                                           verbatimTextOutput("table1"))))
        ), 
        tabPanel("Bivariate",
                 sidebarPanel(
                     varSelectInput("filt3",
                                    "Filter Variable",
                                    data = Full_data[c(1:2)]),
                     selectInput("filt4", "Select Filter",
                                 choices = ""),
                     varSelectInput("var2",
                                    "X axis",
                                    data = Full_data[3:7]),
                     varSelectInput("var3",
                                    "Y axis",
                                    data = Full_data[3:7]),
                     checkboxInput("OLSselect",
                                   "Add OLS"),
                     checkboxInput("all_states0", 
                                   "All states? show summary?")),
                 mainPanel(plotOutput("plot2")),
                 conditionalPanel(condition = "input.all_states0",
                                  mainPanel(
                                      column(10, offset = 2,
                                             plotOutput("plot0"),
                                             
                                             verbatimTextOutput("lm")
                                      )))
        )
        ,
        
        
        tabPanel("Time Series",
                 sidebarPanel(
                     varSelectInput("filt5",
                                    "Filter Variable",
                                    data = Full_data[1]),
                     selectInput("filt6", "Choose state",
                                 choices = ""),
                     varSelectInput("var4",
                                    "X axis",
                                    data = Full_data[2]),
                     varSelectInput("var5",
                                    "Y axis",
                                    data = Full_data[3:7]),
                     checkboxInput("smooth_line",
                                   "Add trend smooth line?"),
                     checkboxInput("all_states", 
                                   "All states?")), 
                 mainPanel(plotOutput("plot3")),
                 conditionalPanel(condition = "input.all_states",
                                  mainPanel(
                                      column(width = 10, offset = 2,plotOutput("plot4"))
                                  )
                 )),
        
        tabPanel("spreadsheet",
                 fluidPage(
                     tableOutput("table2")
                 ))
    )
)



server <- function(input, output, session){
    observe({
        
        updateSelectInput(session,
                          "filt2",
                          choices = Full_data %>% 
                              select(!!input$filt1) %>% 
                              distinct(!!input$filt1))
    })
    
    observe({
        updateSelectInput(session,
                          "filt4",
                          choices = Full_data %>% 
                              select(!!input$filt3) %>% 
                              distinct(!!input$filt3))
    })
    observe({
        updateSelectInput(session,
                          "filt6",
                          choices = Full_data %>% 
                              select(!!input$filt5) %>% 
                              distinct(!!input$filt5))
    })
    
    output$plot1 <- renderPlot({
        
        Full_data %>%
            filter(!!input$filt1 == !!input$filt2) %>% 
            ggplot(aes(x=!!input$var1))+
            geom_density()
        
    })
    
    
    
    output$plot2 <- renderPlot({
        p2 <- Full_data %>%  
            filter(!!input$filt3 == !!input$filt4) %>%
            ggplot(aes(x =!!input$var2, y = !!input$var3)) +
            geom_point() +
            theme_classic() +               
            labs(title = paste(input$var3, " VS ", input$var2, "BY ", input$filt4)) 
        
        
        if(input$OLSselect){
            p2 +
                geom_smooth(method = "lm", se = F)
        }
        else{
            p2
        }
        
    })
    output$plot0 <- renderPlot({
        if(input$all_states0){
            Full_data %>%
                ggplot(aes(x =!!input$var2, y = !!input$var3))+
                geom_point() +
                geom_smooth(method = "lm", se = F, color = "red")+
                labs(title = paste(input$var3, " VS ", input$var2, "BY ALL STATES")) +
                theme_classic()
            
        }
        else{
            print("")
        }
        
    })
    
    output$plot3 <- renderPlot({
        p3 <- Full_data %>% 
            filter(!!input$filt5 == !!input$filt6) %>%
            ggplot(aes(x = !!input$var4, y = !!input$var5))+
            geom_line(color ="#FC4E07", size = 0.7)+
            theme_minimal()+
            labs(title = paste(input$var5, "BY TIME IN ", input$filt6, "STATE"))
        
        if(input$smooth_line){
            p3 +
                geom_smooth(color = "#33CCCC",size =0.5, method = "loess", se = F) 
            
            
        }
        else{
            p3
        }
        
    })
    
    output$plot4 <- renderPlot({
        
        if(input$all_states){
            Full_data %>%
                ggplot(aes(x = !!input$var4, y = !!input$var5, color = state))+
                geom_line()+
                theme_minimal()+
                labs(title = paste(input$var5, "BY TIME IN ALL STATE"))
            
        }
        else{
            print("")
        }
        
    })
    #
    output$lm <- renderPrint({
        if(input$all_states0){
            lmout <- lm(Full_data[[input$var3]] ~ Full_data[[input$var2]], data = Full_data)
            print(summary(lmout))
        }
        else {
            print("")
        }
    })
    
    #
    
    output$table2 <- renderTable({
        Full_data %>%
            select_if(is.numeric)
    })
}

shinyApp(ui, server)
 