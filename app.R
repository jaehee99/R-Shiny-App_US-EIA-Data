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

<<<<<<< HEAD
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
=======
 library(tidyverse)
 library(devtools)
 library(blsAPI)
 library(rjson)
 library(eia)
 library(lubridate)
 library(diffdf)
 library(shiny)
 library(ggplot2)
 
 # First, create a tibble with the api start, the state abbreviation, and the api end
 # Next, we create an id for each state based on that tibble, which we will use in the api call
 # Next, we select only the ID and convert it to a list, using the magrittr::extract to keep it in the pipe
 # Next, we pipe that ID into the api call
 # Next, we select only the "data" column, which originally is a dataframe containing the value, date, and year for every state
 # Next, we add on the states (so each row is a state, then the dataframe of its data) and unnest() into a longer dataframe
 # Finally, we filter for the years all dataframes have in common
 pull_data <- function(api_start, api_end) {
     str_c(api_start, state.abb, api_end) %>% 
         eia_series() %>% 
         select(data) %>% 
         mutate(state = state.name) %>% 
         select(state, everything()) %>% 
         unnest(data) %>%
         filter(year>=2008 & year <= 2017)
 }
 
 # call function for each variable we want and rename the standard "value" column to the more descriptive name we want in the final data
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
 
 # join tibbles together using the state, date, and year columns
 Full_data <- left_join(avg_elec, emission, by = c("state", "date", "year")) %>% 
     left_join(customers, by = c("state", "date", "year")) %>% 
     left_join(retail_sales, by = c("state", "date", "year")) %>% 
     left_join(total_electricity, by = c("state", "date", "year")) %>% 
     select(state, year, electricity_price, carbon_emissions, customers, retail_sales, total_electricity)
 rm(avg_elec, customers, emission, retail_sales, total_electricity)
 
 
#  # Creates a dataframe with the state name, abbreviation, and "state code"
#  state_code <-c(1:2,4:6, 8:13, 15:42, 44:51,54:56)
#  states <-tibble(state_name = state.name, state_abrv = state.abb, state_code = state_code)
#  states %>%
#    mutate(state_code = case_when(
#      state_code<10 ~ str_c("0",as.character(state_code)),
#      state_code>=10 ~ str_c(as.character(state_code))
#    ))->states
#  
#  eia_avg_elect <- tibble(start = "ELEC.PRICE.", abrv = list(state.abb), end = "-IND.A")
#  eia_carbon_emission <-tibble(start = "EMISS.CO2-TOTV-EC-TO-", abrv = list(state.abb), end = ".A")
#  eia_customers <- tibble(start = "ELEC.CUSTOMERS.", abrv = list(state.abb), end = "-ALL.A")
#  eia_retail_sales <- tibble(start = "ELEC.SALES.", abrv = list(state.abb), end = "-ALL.A")
#  eia_total_electricity <- tibble(start = "ELEC.GEN.ALL-",  abrv = list(state.abb), end = "-99.A")
#  
#  unnest(eia_avg_elect)->eia_avg_elect_id
#  unnest(eia_carbon_emission)->eia_carbon_e
#  unnest(eia_customers)->eia_customers
#  unnest(eia_retail_sales) -> eia_retail_sales
#  unnest(eia_total_electricity) -> eia_total_electricity
#  
#  eia_avg_elect_id %>% 
#    mutate(id = str_c(start, abrv, end))->eia_avg_elect_id
#  eia_carbon_e %>% 
#    mutate(id = str_c(start, abrv, end))->eia_carbon_e
#  eia_customers %>% 
#    mutate(id = str_c(start, abrv, end))->eia_customers
#  eia_retail_sales %>%  
#    mutate(id = str_c(start, abrv, end))-> eia_retail_sales 
#  eia_total_electricity %>%  
#    mutate(id = str_c(start, abrv, end))-> eia_total_electricity 
#  
#  
#  eia_set_key("8a87a727635f5c834e2799cd76fcb820")
#  eia_avg_elec <- eia_avg_elect_id$id
#  eia_emmission<- eia_carbon_e$id
#  eia_customers<- eia_customers$id
#  eia_retail_sales <- eia_retail_sales$id
#  eia_total_electricity <- eia_total_electricity$id
#  
#  eia_series(eia_avg_elec)->avg_elec
#  eia_series(eia_emmission)->emmission
#  eia_series(eia_customers)->customers
#  eia_series(eia_retail_sales)->retail_sales
#  eia_series(eia_total_electricity)->total_electricity
#  
#  
#  avg_elec %>%  
#    select(data) -> avg_elec
#  
#  emmission %>%  
#    select(data) -> emmission
#  
#  customers %>% 
#    select(data) -> customers
#  
#  retail_sales %>%  
#    select(data) -> retail_sales
#  
#  total_electricity %>%  
#    select(data) -> total_electricity
#  
#  avg_elec %>%
#    mutate(data = Map(cbind, avg_elec$data, state=states$state_name)) -> avg_elec
#  
#  emmission %>%
#    mutate(data = Map(cbind, emmission$data, state=states$state_name)) -> emmission
#  
#  customers %>%
#    mutate(data = Map(cbind, customers$data, state=states$state_name)) -> customers
#  
#  retail_sales %>%
#    mutate(data = Map(cbind, retail_sales$data, state=states$state_name)) -> retail_sales
#  
#  total_electricity %>%
#    mutate(data = Map(cbind, total_electricity$data, state=states$state_name)) -> total_electricity
#  
#  avg_elec %>%  
#    unnest() -> avg_elec
#  
#  emmission %>%  
#    unnest() -> emmission
#  
#  customers %>%  
#    unnest() -> customers
#  
#  retail_sales %>%  
#    unnest() -> retail_sales
#  
#  total_electricity %>%  
#    unnest() -> total_electricity
#  
#  rename(avg_elec, "Electricity_Price" = value) -> avg_elec
#  rename(emmission, "Carbon_emissions" = value) -> emmission
#  rename(customers, "Customers" = value) -> customers
#  rename(retail_sales, "Retail_Sales" = value) -> retail_sales
#  rename(total_electricity, "Total_electricity" = value) -> total_electricity
#  
#  avg_elec %>% 
#    filter(year>=2008 & year <= 2017) -> avg_elec
#  
#  emmission %>% 
#    filter(year>=2008 & year <= 2017) -> emmission
#  
#  customers %>% 
#    filter(year>=2008 & year <= 2017) -> customers
#  
#  retail_sales %>%  
#    filter(year>=2008 & year <= 2017) -> retail_sales
#  
#  total_electricity %>%  
#    filter(year>=2008 & year <= 2017) -> total_electricity
#  
#  merge(avg_elec, emmission, by = c("year", "date", "state")) -> full_data_0
#  merge(full_data_0, customers, by = c("year", "date", "state")) -> full_data_1
#  merge(full_data_1, retail_sales, by = c("year", "date", "state")) -> full_data_2
#  merge(full_data_2, total_electricity, by = c("year", "date", "state")) -> Full_data
#  
# Full_data
>>>>>>> 3f2343d8d16ab415092f5ec4aabe4f2ea8a0cc26

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
 