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
 
 eia_set_key("8a87a727635f5c834e2799cd76fcb820")
 
 # First, create a tibble with the api start, the state abbreviation, and the api end
 # Next, we create an id for each state based on that tibble, which we will use in the api call
 # Next, we select only the ID and convert it to a list, using the magrittr::extract to keep it in the pipe
 # Next, we pipe that ID into the api call
 # Next, we select only the "data" column, which originally is a dataframe containing the value, date, and year for every state
 # Next, we add on the states (so each row is a state, then the dataframe of its data) and unnest() into a longer dataframe
 # Finally, we filter for the years all dataframes have in common
 pull_data <- function(api_start, api_end) {
     tibble(start = api_start, abrv = state.abb, end = api_end) %>% 
         mutate(id = str_c(start, abrv, end)) %>%
         select(id) %>% 
         as.list() %>% 
         # magrittr:extract2(x, 1) is equivalent to x[[1]]
         magrittr::extract2(1) %>% 
         eia_series() %>% 
         select(data) %>% 
         mutate(state = state.name) %>% 
         select(state, everything()) %>% 
         unnest(data)
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
 
 # Create list of API calls for hourly electricity demand data using str_c
 # Manually add electric grid regions, since they don't align with states
 # Unnest and select relevant data
 # Create list of API calls for hourly electricity demand data using str_c
 # Manually add electric grid regions, since they don't align with states
 # Unnest and select relevant data
 # Create local time variable (rather than UTC) so plots are easier to intepret
 load_data <- str_c("EBA.",
                    c("CAL", "CAR", "CENT", "FLA", "MIDA", "MIDW", "NE", "NY", "NW", "SE", "SW", "TEN", "TEX"),
                    "-ALL.D.H") %>%
     eia_series() %>%
     mutate(
         region = c(
             "California",
             "Carolinas",
             "Central",
             "Florida",
             "Mid-Atlantic",
             "Midwest",
             "New England",
             "New York",
             "Northwest",
             "Southeast",
             "Southwest",
             "Tennessee",
             "Texas"
         )
     ) %>%
     unnest(data) %>%
     select(region, "MWh" = value, "date_utc" = date) %>%
     mutate(
         date_local = case_when(
             region == "California" ~ with_tz(date_utc, tzone = "America/Los_Angeles"),
             region == "Carolinas" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "Central" ~ with_tz(date_utc, tzone = "America/Denver"),
             region == "Florida" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "Mid-Atlantic" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "Midwest" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "New England" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "New York" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "Northwest" ~ with_tz(date_utc, tzone = "America/Los_Angeles"),
             region == "Southeast" ~ with_tz(date_utc, tzone = "America/New_York"),
             region == "Southwest" ~ with_tz(date_utc, tzone = "America/Denver"),
             region == "Tennessee" ~ with_tz(date_utc, tzone = "America/Chicago"),
             region == "Texas" ~ with_tz(date_utc, tzone = "America/Chicago")
         )
     ) %>%
     select(-date_utc)%>% 
     filter(date_local >= ymd("2020-04-26"))
 
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

<<<<<<< HEAD
ui <- shinyUI(navbarPage("FinalProject",
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
                         tabPanel("bivariate anaylsis",
                                  sidebarPanel(
                                      varSelectInput("filt3",
                                                    "Filter Variable",
                                                    data = Full_data[c(1:2)]),
                                      selectInput("filt4", "Select Filter",
                                                  choices = ""),
                                      varSelectInput("var2",
                                                     "X axis",
                                                     data = Full_data[1:2]),
                                      varSelectInput("var3",
                                                     "Y axis",
                                                     data = Full_data[3:7]),
                                      checkboxInput("OLSselect",
                                                    "Add OLS")),
                                  mainPanel(plotOutput("plot2"))),
                         tabPanel("daily load analysis",
                             sidebarPanel(
                                 selectInput("tab3_state1", "State 1",
                                             choices = ""),
                                 selectInput("tab3_state2", "State 2",
                                             choices = ""),
                                 dateInput(
                                     "tab3_date1", "Date to Compare"
                                 ),
                         ),
                         mainPanel(plotOutput("tab3_plot1"),
                                   plotOutput("tab3_plot2"))),
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
=======
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

>>>>>>> origin/main


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
        
<<<<<<< HEAD
        else if(is.factor(Full_data[[input$var2]]) & is.factor(Full_data[[input$var3]])){
            if(input$OLSselect == TRUE){
                Full_data %>%
                    filter(!!input$filt3 == !!input$filt4) %>% 
                ggplot(aes(x = !!input$var2, y = !!input$var3))+
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
=======
        if(input$smooth_line){
            p3 +
                geom_smooth(color = "#33CCCC",size =0.5, method = "loess", se = F) 
            
            
        }
>>>>>>> origin/main
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
    
    observe({
        
        updateSelectInput(session,
                          "tab3_state1",
                          choices = load_data %>% 
                              distinct(region))
    })
    
    observe({
        
        updateSelectInput(session,
                          "tab3_state2",
                          choices = load_data %>% 
                              distinct(region))
    })
    
    output$tab3_plot1 <- renderPlot({
        load_data %>% 
            filter(floor_date(date_local, unit = "day") == !!input$tab3_date1) %>% 
            filter(region == !!input$tab3_state1) %>% 
            ggplot(aes(x = date_local, y = MWh)) +
            geom_line()
    })
    
    output$tab3_plot2 <- renderPlot({
        load_data %>% 
            filter(floor_date(date_local, unit = "day") == !!input$tab3_date1) %>% 
            filter(region == !!input$tab3_state2) %>% 
            ggplot(aes(x = date_local, y = MWh)) +
            geom_line()
    })
    
}

shinyApp(ui, server)
 