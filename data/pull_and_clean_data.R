library(tidyverse)
library(eia) 

eia_set_key("8a87a727635f5c834e2799cd76fcb820")

# First, create a tibble with the api start, the state abbreviation, and the api end
# Next, we create an id for each state based on that tibble, which we will use in the api call
# Next, we select only the ID and convert it to a list, using the magrittr::extract to keep it in the pipe
# Next, we pipe that ID into the api call
# Next, we select only the "data" column, which originally is a dataframe containing the value, date, and year for every state
# Next, we add on the states (so each row is a state, then the dataframe of its data) and unnest() into a longer dataframe
# Finally, we filter for the years all dataframes have in common

pull_and_clean_data <- function(api_start, api_end) {
  str_c(api_start, state.abb, api_end) %>% 
    eia_series() %>% 
    select(data) %>% 
    mutate(state = state.name) %>% 
    select(state, everything()) %>% 
    unnest(data) %>%
    filter(year>=2008 & year <= 2017)
}

# call function for each variable we want and rename the standard "value" column to the more descriptive name we want in the final data
avg_elec <- pull_and_clean_data(api_start = "ELEC.PRICE.", api_end = "-IND.A") %>% 
  rename("electricity_price" = value)
emission <- pull_and_clean_data(api_start = "EMISS.CO2-TOTV-EC-TO-", api_end = ".A") %>% 
  rename("carbon_emissions" = value)
customers <- pull_and_clean_data(api_start = "ELEC.CUSTOMERS.", api_end = "-ALL.A") %>% 
  rename("customers" = value)
retail_sales <- pull_and_clean_data(api_start = "ELEC.SALES.", api_end = "-ALL.A") %>% 
  rename("retail_sales" = value)
total_electricity <- pull_and_clean_data(api_start = "ELEC.GEN.ALL-", api_end = "-99.A") %>% 
  rename("total_electricity" = value)

# join tibbles together using the state, date, and year columns
yearly_data <- left_join(avg_elec, emission, by = c("state", "date", "year")) %>% 
  left_join(customers, by = c("state", "date", "year")) %>% 
  left_join(retail_sales, by = c("state", "date", "year")) %>% 
  left_join(total_electricity, by = c("state", "date", "year")) %>% 
  select(state, year, electricity_price, carbon_emissions, customers, retail_sales, total_electricity)

saveRDS(yearly_data, file = "./data/yearly_data.rds")

rm(list=ls(all=TRUE))