library(shiny)
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
library(bslib)

Full_data <- read_csv(file = "../data/Full_data_4_26.csv")


ui <- fluidPage(
    theme = bs_theme(
        bg = "#9F8C6C", 
        fg = "#000000", 
        primary = "#FAFAFA",
        base_font = "Times New Roman",
        code_font = font_google("JetBrains Mono")),
    #End of theme
    
    titlePanel("EIA Data Project"),
    tabsetPanel(
        tabPanel(
            "univiate anaylsis",
            sidebarPanel(
                varSelectInput("filt1",
                               "Filter Variable",
                               data = Full_data[c(1:2)]),
                selectInput("filt2", "Select Filter",
                            choices = ""),
                varSelectInput("var1",
                               "Plot 1 Variable",
                               data = Full_data[3:7])),
            mainPanel(fluidRow(
                10,
                column(5,
                       plotOutput("plot1")),
                column(5,
                       verbatimTextOutput("table1"))
            ))
            
      ),#End of first tab, Univariate analysis
        tabPanel(
            "Bivariate",
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
                              "All states? show summary?")
            ),
            mainPanel(plotOutput("plot2")),
            conditionalPanel(condition = "input.all_states0",
                             mainPanel(
                                 column(10, offset = 2,
                                        plotOutput("plot0"),
                                        verbatimTextOutput("lm"))
                             ))
            
    ),#End of second tab, bivariate
    
      tabPanel(
          "daily load analysis",
          sidebarPanel(
              selectInput("tab3_state1", "State 1",
                          choices = ""),
              selectInput("tab3_state2", "State 2",
                          choices = ""),
              dateInput("tab3_date1", "Date to Compare"),
          ),
          mainPanel(plotOutput("tab3_plot1"),
                    plotOutput("tab3_plot2"))
          
    ), #End of third tab, daily analysis
      tabPanel(
        "Time Series",
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
                          "All states?")
        ),
        mainPanel(plotOutput("plot3")),
        conditionalPanel(condition = "input.all_states",
                         mainPanel(column(
                             width = 10, offset = 2, plotOutput("plot4")
                         )))
    ), #End of 4th tab, time series
      tabPanel("spreadsheet",
             fluidPage(tableOutput("table2")))
 
    ) #End tabset panel
) #End of fluid page


server <- function(input, output, session) {
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
            ggplot(aes(x = !!input$var1)) +
            theme_economist_white()+
            geom_density()

    })
    
    output$plot2 <- renderPlot({
        p2 <- Full_data %>%
            filter(!!input$filt3 == !!input$filt4) %>%
            ggplot(aes(x = !!input$var2, y = !!input$var3)) +
            geom_point() +
            theme_economist_white()+
            labs(title = paste(input$var3, " VS ", input$var2, "BY ", input$filt4))
        
        
        if (input$OLSselect) {
            p2 +
                geom_smooth(method = "lm", se = F)
        }
        else{
            p2
        }
        
    })
    
    output$plot0 <- renderPlot({
        if (input$all_states0) {
            Full_data %>%
                ggplot(aes(x = !!input$var2, y = !!input$var3)) +
                geom_point() +
                geom_smooth(method = "lm",
                            se = F,
                            color = "red") +
                labs(title = paste(input$var3, " VS ", input$var2, "BY ALL STATES")) +
                theme_economist_white()
            
        }
        else{
            print("")
        }
        
    })
    
    output$plot3 <- renderPlot({
        p3 <- Full_data %>%
            filter(!!input$filt5 == !!input$filt6) %>%
            ggplot(aes(x = !!input$var4, y = !!input$var5)) +
            geom_line(color = "#FC4E07", size = 0.7) +
            theme_economist_white()+
            labs(title = paste(input$var5, "BY TIME IN ", input$filt6, "STATE"))
        
        if (input$smooth_line) {
            p3 +
                geom_smooth(
                    color = "#33CCCC",
                    size = 0.5,
                    method = "loess",
                    se = F
                )
            
            
        }
        else{
            p3
        }
        
    })
    
    output$plot4 <- renderPlot({
        if (input$all_states) {
            Full_data %>%
                ggplot(aes(
                    x = !!input$var4,
                    y = !!input$var5,
                    color = state
                )) +
                geom_line() +
                theme_economist_white()+
                labs(title = paste(input$var5, "BY TIME IN ALL STATE"))
            
        }
        else{
            print("")
        }
        
    })
    
    output$lm <- renderPrint({
        if (input$all_states0) {
            lmout <-
                lm(Full_data[[input$var3]] ~ Full_data[[input$var2]], data = Full_data)
            print(summary(lmout))
        }
        else {
            print("")
        }
    })
    
    
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
            geom_line()+
            theme_economist_white()
    })
    
    output$tab3_plot2 <- renderPlot({
        load_data %>%
            filter(floor_date(date_local, unit = "day") == !!input$tab3_date1) %>%
            filter(region == !!input$tab3_state2) %>%
            ggplot(aes(x = date_local, y = MWh)) +
            geom_line()+
            theme_economist_white()
    })
    
    
    

    
    
 
}#End of server






# Run the application 
shinyApp(ui = ui, server = server)
