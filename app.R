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
# 
# #Creating a tibble of state name, state abrv and BLS state code
# #This is a list of the state codes for the BLS database
# state_code <-c(1:2,4:6, 8:13, 15:42, 44:51,54:56)
# 
# #Makes the tibble states
# states <-tibble(state_name = state.name, state_abrv = state.abb, state_code = state_code)
# 
# states
# 
# states %>% 
#     mutate(state_code = case_when(
#         state_code<10 ~ str_c("0",as.character(state_code)),
#         state_code>=10 ~ str_c(as.character(state_code))
#     ))->states
# 
# 
# #Creating IDs BLS
# 
# BLS_id <- tibble (start = c("LAUST", "TEST"), state = list(states$state_code), end = "0000000000003")
# 
# unnest(BLS_id)->BLS_id
# 
# BLS_id %>% 
#     mutate(seriesID = str_c(start, state, end))->BLS_id
# 
# BLS_id %>% 
#     filter(start == "LAUST")->BLS_id
# 
# #Data set name i.e. Unemployment rate, Employment rate, etc.
# BLS_id %>% 
#     mutate(type = case_when(
#         start == "LAUST" ~ "Unemployment Rate"
#     ))->BLS_id
# 
# BLS_id
# #The Next part is for the eia data.
# 
# #Creating IDs EIA
# 
# eai_avg_elect <- tibble(start = "ELEC.PRICE.", abrv = list(state.abb), end = "-IND.M")
# 
# eai_carbon_emission <-tibble( start = "EMISS.CO2-TOTV-EC-TO-", abrv = list(state.abb), end = ".A")
# 
# eai_customers <- tibble( start = "ELEC.CUSTOMERS.", abrv = list(state.abb), end = "-ALL.A")
# 
# unnest(eai_avg_elect)->eai_avg_elect_id
# 
# eai_avg_elect_id %>% 
#     mutate(id = str_c(start, abrv, end))->eai_avg_elect_id
# 
# eai_avg_elect_id
# 
# unnest(eai_carbon_emission)->eai_carbon_emission_id
# 
# eai_carbon_emission_id %>% 
#     mutate(id = str_c(start, abrv, end))->eai_carbon_emission_id
# 
# unnest(eai_customers)->eai_customers
# 
# eai_customers %>% 
#     mutate(id = str_c(start, abrv, end))->eai_customers_id
# #We can pull lots of data now pretty quickly.
# 
# BLS_id$seriesID
# #Getting BLS data using generated IDs
# #BLS API has a limit of 25 returns per querey so you have to run it twice.
# data<-list(seriesid = BLS_id$seriesID)
# data2<-list(seriesid = BLS_id$seriesID[26:50])
# data
# data2
# blsAPI(data , 2, TRUE)->Unemployment1
# blsAPI(data2, 2, TRUE)->Unemployemnt2
# Unemployment1
# Unemployemnt2
# 
# full_join(Unemployment1, Unemployemnt2)->Unemployment
# 
# #And just like that we have the data frame for unemployment for every state for a year.
# 
# #Time to do the eia api
# 
# eia_set_key("8a87a727635f5c834e2799cd76fcb820")
# eia_avg_elec <- eai_avg_elect_id$id
# eia_emmission<- eai_carbon_emission_id$id
# eia_customers<- eai_customers_id
# eia_series(eia_avg_elec)->avg_elec
# eia_emmission
# eia_series("EMISS.CO2-TOTV-EC-TO-AL.A")
# eia_series(eia_emmission)->emmissions_carbon
# avg_elec
# emmissions_carbon
# 
# 
# unnest(avg_elec, data)->Retail_cost_electricity
# Retail_cost_electricity
# 
# 
# #And now we hat the retail electric cost data frame.
# #We can further clean the data from here and pull it into a shiny app.
# #We can also quickly build a data frame with a large query letting us acess a lot of data.
# #We can also engineer custom queries if we have the time.
# #Let me know if you have questions.
# 
# #Graph friendly data frame
# 
# Retail_cost_electricity %>% 
#     select(geography, date, value)->eai_data
# eai_data 
# 
# #So we can add the months to the eai data
# month_num<-tibble(month_name = month.name, month = c(1:12))
# #allows us to join the data frames by month
# month_num %>% 
#     mutate(month = case_when(
#         month<10 ~ str_c("0",as.character(month)),
#         month>=10 ~ str_c(as.character(month))
#     ))->month_num
# #Get's us year and month which we will need.
# eai_data %>% 
#     separate(geography, c("country", "abb"),"-") %>% 
#     separate(date, c("year", "month", "day"), "-") %>% 
#     select(-day)->eai_data
# #lets us add states name to data frame
# states %>% 
#     rename(abb = state_abrv)->states
# 
# #adds month name and state names
# left_join(eai_data, month_num, by = "month")->eai_data
# left_join(eai_data, states, by = "abb")->eai_data
# 
# eai_data
# #Makes things look nice.
# eai_data %>% 
#     select(-c(country, month, state_code)) %>% 
#     relocate(c(state_name, abb, month_name, year, value)) %>% 
#     rename(month = month_name)->eai_data
# 
# #Making a better BLS
# #Changed name to do join
# states %>% 
#     rename(state = state_code)->states
# Unemployment->BLS_data
# 
# #adding state name and type
# left_join(BLS_data, BLS_id, by = "seriesID")->BLS_data
# left_join(BLS_data, states, by = "state")->BLS_data
# 
# #cleaning the data frame
# BLS_data %>% 
#     select(-c(period, start, end, seriesID, state)) %>% 
#     rename(month = periodName) %>% 
#     relocate(c(state_name, abb, month))->BLS_data
# 
# #The BLS data frame is more up to date. We will need to remove march of this year.
# BLS_data %>% 
#     filter(!(month == "March" & year == "2021"))->BLS_data
# #The eai data frame goes back farther than the BLS so we need to remove the excess.
# eai_data %>% 
#     filter(year>=2019)->eai_data
# 
# #So we have two columns with the values from each data frame.
# eai_data %>% 
#     rename(avg_electricity = value)->eai_data
# BLS_data %>% 
#     rename(unemployment_rate = value)->BLS_data
# 
# #creating the full data set
# full_join(BLS_data, eai_data)->Full_data
# Full_data %>% 
#     select(-type)->Full_data
Full_data

library(shiny)
library(ggplot2)

ui <- shinyUI(navbarPage("FinalProject",
                         tabPanel("univiate anaylsis",
                                  sidebarPanel(
                                      varSelectInput("filt1",
                                                     "Filter Variable",
                                                     data = Full_data[c(1, 3:4)]),
                                      selectInput("filt2", "Select Filter",
                                                  choices = ""),
                                      varSelectInput("var1",
                                                     "Plot 1 Variable",
                                                     data = Full_data[5:6])),
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
                                                    data = Full_data[c(1, 3:4)]),
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