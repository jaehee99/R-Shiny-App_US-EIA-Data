#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

 library(tidyverse)
 library(devtools)
 library(blsAPI)
 library(rjson)
 library(eia)
 library(lubridate)
 library(diffdf)
 library(shiny)
 library(ggplot2)

 # Creates a dataframe with the state name, abbreviation, and "state code"
 state_code <-c(1:2,4:6, 8:13, 15:42, 44:51,54:56)
 states <-tibble(state_name = state.name, state_abrv = state.abb, state_code = state_code)
 states %>%
   mutate(state_code = case_when(
     state_code<10 ~ str_c("0",as.character(state_code)),
     state_code>=10 ~ str_c(as.character(state_code))
   ))->states
 
 eia_avg_elect <- tibble(start = "ELEC.PRICE.", abrv = list(state.abb), end = "-IND.A")
 eia_carbon_emission <-tibble(start = "EMISS.CO2-TOTV-EC-TO-", abrv = list(state.abb), end = ".A")
 eia_customers <- tibble(start = "ELEC.CUSTOMERS.", abrv = list(state.abb), end = "-ALL.A")
 eia_retail_sales <- tibble(start = "ELEC.SALES.", abrv = list(state.abb), end = "-ALL.A")
 eia_total_electricity <- tibble(start = "ELEC.GEN.ALL-",  abrv = list(state.abb), end = "-99.A")
 
 unnest(eia_avg_elect)->eia_avg_elect_id
 unnest(eia_carbon_emission)->eia_carbon_e
 unnest(eia_customers)->eia_customers
 unnest(eia_retail_sales) -> eia_retail_sales
 unnest(eia_total_electricity) -> eia_total_electricity
 
 eia_avg_elect_id %>% 
   mutate(id = str_c(start, abrv, end))->eia_avg_elect_id
 eia_carbon_e %>% 
   mutate(id = str_c(start, abrv, end))->eia_carbon_e
 eia_customers %>% 
   mutate(id = str_c(start, abrv, end))->eia_customers
 eia_retail_sales %>%  
   mutate(id = str_c(start, abrv, end))-> eia_retail_sales 
 eia_total_electricity %>%  
   mutate(id = str_c(start, abrv, end))-> eia_total_electricity 
 
 
 eia_set_key("8a87a727635f5c834e2799cd76fcb820")
 eia_avg_elec <- eia_avg_elect_id$id
 eia_emmission<- eia_carbon_e$id
 eia_customers<- eia_customers$id
 eia_retail_sales <- eia_retail_sales$id
 eia_total_electricity <- eia_total_electricity$id
 
 eia_series(eia_avg_elec)->avg_elec
 eia_series(eia_emmission)->emmission
 eia_series(eia_customers)->customers
 eia_series(eia_retail_sales)->retail_sales
 eia_series(eia_total_electricity)->total_electricity
 
 
 avg_elec %>%  
   select(data) -> avg_elec
 
 emmission %>%  
   select(data) -> emmission
 
 customers %>% 
   select(data) -> customers
 
 retail_sales %>%  
   select(data) -> retail_sales
 
 total_electricity %>%  
   select(data) -> total_electricity
 
 avg_elec %>%
   mutate(data = Map(cbind, avg_elec$data, state=states$state_name)) -> avg_elec
 
 emmission %>%
   mutate(data = Map(cbind, emmission$data, state=states$state_name)) -> emmission
 
 customers %>%
   mutate(data = Map(cbind, customers$data, state=states$state_name)) -> customers
 
 retail_sales %>%
   mutate(data = Map(cbind, retail_sales$data, state=states$state_name)) -> retail_sales
 
 total_electricity %>%
   mutate(data = Map(cbind, total_electricity$data, state=states$state_name)) -> total_electricity
 
 avg_elec %>%  
   unnest() -> avg_elec
 
 emmission %>%  
   unnest() -> emmission
 
 customers %>%  
   unnest() -> customers
 
 retail_sales %>%  
   unnest() -> retail_sales
 
 total_electricity %>%  
   unnest() -> total_electricity
 
 rename(avg_elec, "Electricity_Price" = value) -> avg_elec
 rename(emmission, "Carbon_emissions" = value) -> emmission
 rename(customers, "Customers" = value) -> customers
 rename(retail_sales, "Retail_Sales" = value) -> retail_sales
 rename(total_electricity, "Total_electricity" = value) -> total_electricity
 
 avg_elec %>% 
   filter(year>=2008 & year <= 2017) -> avg_elec
 
 emmission %>% 
   filter(year>=2008 & year <= 2017) -> emmission
 
 customers %>% 
   filter(year>=2008 & year <= 2017) -> customers
 
 retail_sales %>%  
   filter(year>=2008 & year <= 2017) -> retail_sales
 
 total_electricity %>%  
   filter(year>=2008 & year <= 2017) -> total_electricity
 
 merge(avg_elec, emmission, by = c("year", "date", "state")) -> full_data_0
 merge(full_data_0, customers, by = c("year", "date", "state")) -> full_data_1
 merge(full_data_1, retail_sales, by = c("year", "date", "state")) -> full_data_2
 merge(full_data_2, total_electricity, by = c("year", "date", "state")) -> Full_data
 
Full_data

ui <- shinyUI(navbarPage("FinalProject",
                         tabPanel("univiate anaylsis",
                                  sidebarPanel(
                                      varSelectInput("filt1",
                                                     "Filter Variable",
                                                     data = Full_data[c(1,3)]),
                                      selectInput("filt2", "Select Filter",
                                                  choices = ""),
                                      varSelectInput("var1",
                                                     "Plot 1 Variable",
                                                     data = Full_data[4:8])),
                                  mainPanel(fluidRow(10,
                                                     column(5,
                                                            plotOutput("plot1")),
                                                     column(5,
                                                            verbatimTextOutput("table1"))))
                         ),
                         tabPanel("bivariate anaylsis",
                                  sidebarPanel(
                                      varSelectInput("filt3",
                                                    "Filter Variable",
                                                    data = Full_data[c(1,3)]),
                                      selectInput("filt4", "Select Filter",
                                                  choices = ""),
                                      varSelectInput("var2",
                                                     "X axis",
                                                     data = Full_data),
                                      varSelectInput("var3",
                                                     "Y axis",
                                                     data = Full_data),
                                      checkboxInput("OLSselect",
                                                    "Add OLS")),
                                  mainPanel(plotOutput("plot2"))),
                         tabPanel("spreadsheet",
                                  fluidPage(
                                      tableOutput("table2")
                                  ))
)
)

# if (is.numeric(Full_data[[input$var1]])){
#     if (input$logselect == TRUE){
#         ggplot(Full_data, aes(x=log(!!input$var1)))+
#             geom_histogram(bins = input$bins)
#     }
#     else{
#         ggplot(Full_data, aes(x= !!input$var1))+
#             geom_histogram(bins = input$bins)
#     }
# }
# else{
#     if (input$logselect == TRUE){
#         validate(
#             need(is.factor(input$var1), "cannot use log transformation on factor.")
#         )
#         ggplot(Full_data, aes(x=log(!!input$var1)))+
#             geom_bar()
#     }
#     else{
#         ggplot(Full_data, aes(x=!!input$var1))+
#             geom_bar()
#     }
# }

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
    output$plot1 <- renderPlot({

        Full_data %>%
            filter(!!input$filt3 == !!input$filt4) %>% 
        ggplot(aes(x=!!input$var1))+
            geom_density()

    })

    
    # output$table1 <- renderPrint({
    #     if (input$logselect == TRUE){
    #         t.test(log(Full_data[[input$var1]]))
    #     }
    #     else{
    #         t.test(Full_data[[input$var1]])
    #     }
    # })
    
    output$plot2 <- renderPlot({
        if(is.numeric(Full_data[[input$var2]]) & is.numeric(Full_data[[input$var3]])){
            if(input$OLSselect == TRUE){
                Full_data %>%
                    filter(!!input$filt3 == !!input$filt4) %>% 
                ggplot( aes(x = !!input$var2, y = !!input$var3))+
                        geom_point()+
                        geom_smooth(method = 'lm')
                }
                else{
                    Full_data %>%
                        filter(!!input$filt3 == !!input$filt4) %>% 
                    ggplot( aes(x = !!input$var2, y = !!input$var3))+
                        geom_point()
                }
            }
        
        else if(is.factor(Full_data[[input$var2]]) & is.factor(Full_data[[input$var3]])){
            if(input$OLSselect == TRUE){
                Full_data %>%
                    filter(!!input$filt3 == !!input$filt4) %>% 
                ggplot(, aes(x = !!input$var2, y = !!input$var3))+
                    geom_jitter()+
                    geom_smooth(method = 'lm')
                }
            else{
                Full_data %>%
                    filter(!!input$filt3 == !!input$filt4) %>% 
                ggplot( aes(x = !!input$var2, y = !!input$var3))+
                    geom_jitter()
                }
            }
        else{
            if(is.numeric(Full_data[[input$var3]])){
                if(input$OLSselect == TRUE){
                    Full_data %>%
                        filter(!!input$filt3 == !!input$filt4) %>% 
                    ggplot( aes(x = !!input$var2, y = !!input$var3))+
                        geom_boxplot()+
                        geom_smooth(method = 'lm')
                    }
                else{
                    Full_data %>%
                        filter(!!input$filt3 == !!input$filt4) %>% 
                    ggplot( aes(x = !!input$var2, y = !!input$var3))+
                        geom_boxplot()
                    }
                }
            else{
                if(input$OLSselect == TRUE){
                    Full_data %>%
                        filter(!!input$filt3 == !!input$filt4) %>% 
                    ggplot(aes(x = !!input$var3, y = !!input$var2))+
                        geom_boxplot()+
                        geom_smooth(method = 'lm')
                    }
                else{
                    Full_data %>%
                        filter(!!input$filt3 == !!input$filt4) %>% 
                    ggplot(aes(x = !!input$var3, y = !!input$var2))+
                        geom_boxplot()
                    }
                }
            }
        })
    
    output$table2 <- renderTable({
        Full_data %>% 
            select_if(is.numeric)
    })
}

shinyApp(ui, server)