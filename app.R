library(tidyverse)
#library(devtools)
#library(rjson)
library(eia)
library(lubridate)
#library(diffdf)
library(shiny)
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(GGally)
library(broom)
library(shinythemes)


eia_set_key("8a87a727635f5c834e2799cd76fcb820")

full_data <- readRDS("./data/yearly_data.rds")

# List of regional electric grids and their abbreviations to make syntax clearer when pulling data
region_list <-
  tibble(
    abbr = c(
      "CAL",
      "CAR",
      "CENT",
      "FLA",
      "MIDA",
      "MIDW",
      "NE",
      "NY",
      "NW",
      "SE",
      "SW",
      "TEN",
      "TEX"
    ),
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
  )

# Create list of API calls for hourly electricity demand data using str_c
# Add electric grid regions, since they don't align with states
# Unnest and select relevant data
# Create local time variable (rather than UTC) so plots are easier to intepret
str_c("EBA.",
      region_list$abbr,
      "-ALL.D.H") %>%
  eia_series() %>%
  mutate(region = region_list$region) %>%
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
  select(-date_utc) %>%
  filter(date_local >= ymd("2020-04-26")) -> load_data

plot_choices = c("Density Plot", "Histogram", "Frequency Polygon")

# Create UI 
ui <- fluidPage(
   #Add theme, make it interactive to the users
    shinythemes::themeSelector(),
    theme = shinytheme("cerulean"),
 
     titlePanel("US EIA Data Analysis"),
  # EIA stands for Energy Information Administration
  tabsetPanel(
    # tab 1: Univariate
    tabPanel(
      "Univariate",
      sidebarPanel(
        
        varSelectInput("univariate_filt1",
                       "Filter Variable",
                       data = Full_data[c(1:2)]),
        selectInput("univariate_filt2", "Select Filter",
                    choices = ""),
        varSelectInput("univariate_var",
                       "Choose Variable?",
                       data = Full_data[3:7]), 
        radioButtons("Choices", "Choose a plot type?", choices = plot_choices),
        sliderInput("bins",
                    "Histogram: Number of Bins?:",
                    min = 1,
                    max = 100,
                    value = 40),
        tableOutput("t_test"), 
        helpText("Warning: No data for 'carbon emissions'(2018 ~ 2020) & 'customers'(2001 ~ 2007).
                        That is why we cannot see the graphs when we click these options. ")
        
      ),
      mainPanel(fluidRow(
        
        column(8, 
             plotOutput("univariate_plot")
               )
      ))
    ),
    # tab 2: Bivariate
    tabPanel(
      "Bivariate",
      sidebarPanel(
        varSelectInput("bivariate_filt1",
                       "Filter Variable",
                       data = Full_data[c(1:2)]),
        selectInput("bivariate_filt2", "Select Filter",
                    choices = ""),
        varSelectInput("bivariate_var1",
                       "X axis",
                       data = Full_data[3:7]),
        varSelectInput("bivariate_var2",
                       "Y axis",
                       data = Full_data[3:7]),
        checkboxInput("OLSselect",
                      "Add OLS to [ PLOT 1 ]? "),
        checkboxInput("all_states0",
                      "All states? & All year?"), 
        checkboxInput("OLSselect_2",
                      "Add OLS to [ PLOT 2 ]? "),
        checkboxInput("summary_summary", 
                      "Show summary?"), 
        helpText("Warning: No data for 'carbon emissions'(2018 ~ 2020) & 'customers'(2001 ~ 2007).
                        That is why we cannot see the graphs when we click these options. ")
      ),
      mainPanel(column(8,
        plotOutput("bivariate_plot1")
        )),
      conditionalPanel(condition = "input.all_states0",
                       mainPanel(
                         column(8,
                                plotOutput("bivariate_plot2"),
                                verbatimTextOutput("bivariate_table"))
                       ))
    ),
    # tab 3: Multivariate
    tabPanel(
      "Multivariate",
      mainPanel(
        tabsetPanel(
          tabPanel("Correlation", 
                   mainPanel( 'Correlation Matrix Heatmap (2008 ~ 2017)',
                   plotOutput("correlation_plot"))), 
          tabPanel("Pairs",
                   mainPanel('Scatter plot Matrices (2008 ~ 2017)'),
                   plotOutput("pair_plot")))
        
      )
    ),
    # tab 4: Daily load
    tabPanel(
      "Daily Load",
      sidebarPanel(
        selectInput("daily_load_var1", "Choose State 1? ",
                    choices = ""),
        selectInput("daily_load_var2", "Choose State 2?",
                    choices = ""),
        dateInput("daily_load_date", "Date to Compare"),
      ),
      mainPanel(
        column(8, 
               plotOutput("daily_load_plot1"),
               plotOutput("daily_load_plot2")))
    ),
    
    # tab 5: Time Series
    tabPanel(
      "Time Series",
      sidebarLayout(position = "left", 
        sidebarPanel(varSelectInput("time_series_filt1",
                       "Filter Variable",
                       data = Full_data[1]),
        selectInput("time_series_filt2", "Choose state",
                    choices = ""),
        varSelectInput("time_series_var1",
                       "What variable trend do you want to see (choice1)?",
                       data = Full_data[3:7]),
        varSelectInput("time_series_var2",
                       "What variable trend do you want to see (choice2)?",
                       data = Full_data[3:7]),
        checkboxInput("smooth_line",
                      "Add trend smooth line?"),
        checkboxInput("all_states",
                      "All states by states (color)?")),
      mainPanel(
                fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("time_series_plot_1"),
                              plotOutput("time_series_plot_2"))
                ))
      ),
      conditionalPanel(condition = "input.all_states",
      mainPanel( 
                fluidRow(
                  column(width = 6),
                  splitLayout(cellWidths = c("50%", "50%"), 
                              plotOutput("time_series_plot_3"),
                              plotOutput("time_series_plot_4"))
                )  ))
      ),
    
    # tab 6: Spreadsheet
    tabPanel("spreadsheet",
             fluidPage(DT::dataTableOutput("spreadsheet_table")))
  ))

# Create Server
server <- function(input, output, session) {
  
  # tab 1: Univariate
  observe({
    updateSelectInput(session,
                      "univariate_filt2",
                      choices = Full_data %>%
                        select(!!input$univariate_filt1) %>%
                        distinct(!!input$univariate_filt1))
  })

  # Univariate tab first plot
  output$univariate_plot <- renderPlot({
    Full_data %>%  
      filter(!!input$univariate_filt1 == !!input$univariate_filt2) %>% 
    ggplot(aes(x = !!input$univariate_var)) +
      switch(
        input$Choices, 
        "Histogram" = geom_histogram(bins = input$bins, color = "hot pink", fill = "light blue"),
        "Density Plot" = geom_density(color = "#018571"), 
        "Frequency Polygon" = geom_freqpoly(color = "#018571")
      ) +
      theme_bw()+
      labs(title = paste(input$univariate_var, "in", input$univariate_filt2))
  })
  # Univariate t test table
  output$t_test <- renderTable({
     Full_data %>%
       select(input$univariate_var) %>%
      t.test(alternative = "two.sided", mu = 0, conf.level = 0.95) %>% 
       tidy() %>%
      select(p.value,estimate, conf.low, conf.high) %>%
      rename(c('P-Value' = p.value, 'Estimate' = estimate, '95% Lower' = conf.low, '95% Higher' = conf.high))


  })
  # tab 2: Bivariate
  observe({
    updateSelectInput(session,
                      "bivariate_filt2",
                      choices = Full_data %>%
                        select(!!input$bivariate_filt1) %>%
                        distinct(!!input$bivariate_filt1))
  })
  # Bivariate tab, first plot
  # scatter plot based on two variables (with filter, depends on year or state)
  output$bivariate_plot1 <- renderPlot({

    biv_plot1 <- Full_data %>%
      filter(!!input$bivariate_filt1 == !!input$bivariate_filt2) %>%
      ggplot(aes(x = !!input$bivariate_var1, y = !!input$bivariate_var2)) +
      geom_point() +
      theme_bw() +
      labs(title = paste("[ PLOT 1 ]", input$bivariate_var2, " VS ", input$bivariate_var1, "(", input$bivariate_filt2 , ")"))
    
    if (input$OLSselect) {
      biv_plot1 +
        geom_smooth(method = "lm", se = F)
    }
    else{
      biv_plot1
    }
    
  })
  # Bivariate tab, second plot
  # scatter plot based on two variables (with no filter, all data)
  output$bivariate_plot2 <- renderPlot({
   biv_plot2 <- Full_data %>%
      ggplot(aes(x = !!input$bivariate_var1, y = !!input$bivariate_var2, color = state)) +
      geom_point() +
      labs(title = paste("[ PLOT 2 ]",input$bivariate_var2, " VS ", input$bivariate_var1, "(all data)")) +
      theme_bw()
  
    if (input$OLSselect_2) {
      biv_plot2 +
        geom_smooth(method = "lm",
                    se = F,
                    color = "blue") 
    }
    else{
      biv_plot2
    }
  })
  # Bivarate tab, summary table 
  output$bivariate_table <- renderPrint({
    if (input$summary_summary) {
      lmout <-
        lm(Full_data[[input$bivariate_var2]] ~ Full_data[[input$bivariate_var1]], data = Full_data)
      print(summary(lmout))
    }
    else {
      print("If you want to see the summary of [ PLOT2 ], click 'Show summary'")
    }
  })
  # tab 3: Multivariate
  # Multivariate first tab: correlation heat map 
  # filtered years(2008~2017), this is because in order to create correlation plot we have to have same rows
  output$correlation_plot <- renderPlot({
    Full_data %>%  
      filter(year>=2008 & year <= 2017) %>% 
      select(electricity_price, carbon_emissions, customers, retail_sales, total_electricity) -> new_data
    
    ggcorrplot(cor(new_data, use="complete.obs"), 
               hc.order = TRUE,
               lab = TRUE)
  })
  # Multivariate second tab: pairs 
  output$pair_plot <- renderPlot({
    Full_data %>%  
      filter(year>=2008 & year <= 2017) %>% 
      select(electricity_price, carbon_emissions, customers, retail_sales, total_electricity) -> new_data
    pairs(new_data)+ 
      theme_bw()
  })
  # tab 4: Daily load   
  observe({
    updateSelectInput(session,
                      "daily_load_var1",
                      choices = load_data %>%
                        distinct(region))
  })
  
  observe({
    updateSelectInput(session,
                      "daily_load_var2",
                      choices = load_data %>%
                        distinct(region))
  })
  # Daily load first plot (first choice of the state)
  output$daily_load_plot1 <- renderPlot({
    load_data %>%
      filter(floor_date(date_local, unit = "day") == !!input$daily_load_date) %>%
      filter(region == !!input$daily_load_var1) %>%
      ggplot(aes(x = date_local, y = MWh)) +
      geom_line(color = "#FC4E07", size = 0.7)+
      theme_bw()+
      labs(title = paste("[ PLOT 1 ] ",input$daily_load_var1, "electricity"))
  })
  # Daily load second plot(second choice of the state)
  output$daily_load_plot2 <- renderPlot({
    load_data %>%
      filter(floor_date(date_local, unit = "day") == !!input$daily_load_date) %>%
      filter(region == !!input$daily_load_var2) %>%
      ggplot(aes(x = date_local, y = MWh)) +
      geom_line(color = "#FC4E07", size = 0.7)+
      theme_bw()+
      labs(title = paste("[ PLOT 2 ] ",input$daily_load_var2, "electricity"))
  })
  # tab 5: Time series
  observe({
    updateSelectInput(session,
                      "time_series_filt2",
                      choices = Full_data %>%
                        select(!!input$time_series_filt1) %>%
                        distinct(!!input$time_series_filt1))
  })
  # Time series first plot
  output$time_series_plot_1 <- renderPlot({
    p1 <- Full_data %>%
      filter(!!input$time_series_filt1 == !!input$time_series_filt2) %>%
      ggplot(aes(x = year, y = !!input$time_series_var1)) +
      geom_line(color = "#FC4E07", size = 0.7) +
      theme_bw() +
      labs(title = paste("[ PLOT 1 ] ",input$time_series_var1, "vs year (", input$time_series_filt2, ")"))
    
    if (input$smooth_line) {
      p1 +
        geom_smooth(
          color = "#33CCCC",
          size = 0.5,
          method = "loess",
          se = F
        )
    }
    else{
      p1
    }
  })
  output$time_series_plot_2 <- renderPlot({
    p2 <- Full_data %>%
      filter(!!input$time_series_filt1 == !!input$time_series_filt2) %>%
      ggplot(aes(x = year, y = !!input$time_series_var2)) +
      geom_line(color = "#FC4E07", size = 0.7) +
      theme_bw() +
      labs(title = paste("[ PLOT 2 ] ",input$time_series_var2, "vs year (", input$time_series_filt2, ")"))
    
    if (input$smooth_line) {
      p2 +
        geom_smooth(
          color = "#33CCCC",
          size = 0.5,
          method = "loess",
          se = F
        )
    }
    else{
      p2
    }
  })
  # Time series second plot
  output$time_series_plot_3 <- renderPlot({
    if (input$all_states) {
      Full_data %>%
        ggplot(aes(
          x = year,
          y = !!input$time_series_var1,
          color = state
        )) +
        geom_line() +
        theme_bw() +
        labs(title = paste("[ PLOT 3 ] ",input$time_series_var1, "vs year (all data)"))
    }
    else{
      print("")
    }
  })
  output$time_series_plot_4 <- renderPlot({
    if (input$all_states) {
      Full_data %>%
        ggplot(aes(
          x = year,
          y = !!input$time_series_var2,
          color = state
        )) +
        geom_line() +
        theme_bw() +
        labs(title = paste("[ PLOT 4 ] ",input$time_series_var2, "vs year (all data)"))
    }
    else{
      print("")
    }
  })

  # tab 6: Spreadsheet   
  output$spreadsheet_table <- DT::renderDataTable({
    Full_data
  })
}
shinyApp(ui, server)