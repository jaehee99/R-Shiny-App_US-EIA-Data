
library(shiny)

#Creating a tibble of state name, state abrv and BLS state code
#This is a list of the state codes for the BLS database
state_code <-c(1:2,4:6, 8:13, 15:42, 44:51,54:56)

#Makes the tibble states
states <-tibble(state_name = state.name, state_abrv = state.abb, state_code = state_code)

states

states %>% 
    mutate(state_code = case_when(
        state_code<10 ~ str_c("0",as.character(state_code)),
        state_code>=10 ~ str_c(as.character(state_code))
    ))->states


#Creating IDs BLS

BLS_id <- tibble (start = c("LAUST", "TEST"), state = list(states$state_code), end = "0000000000003")

unnest(BLS_id)->BLS_id

BLS_id %>% 
    mutate(seriesID = str_c(start, state, end))->BLS_id

BLS_id %>% 
    filter(start == "LAUST")->BLS_id

#Data set name i.e. Unemployment rate, Employment rate, etc.
BLS_id %>% 
    mutate(type = case_when(
        start == "LAUST" ~ "Unemployment Rate"
    ))->BLS_id

BLS_id

read_csv(file = "data/eai_retail_cost_elec.csv")->eia_df


ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
