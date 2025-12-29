#Data Collection
#This code utilizes the variables and years defined in prior scripts and accesses the Census API for the correct data. It works well with the loop function in 'Looping Script' as well

Staterawdata <- get_acs(geography = "state", variables = Variablesforanalysis, year = Year, geometry = TRUE)
Countyrawdata <- get_acs(geography = "county", variables = Variablesforanalysis, year = Year, geometry = TRUE)
Tractrawdata <- get_acs(state = States, geography = "tract", variables = Variablesforanalysis, year = Year, geometry = TRUE)