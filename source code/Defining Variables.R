# Defining Variables

library(tidycensus)

#census_api_key("1c510a3d15934abd0b1ed726f34a78d2459b3683", install = TRUE)

#Update lines 12, 13, 15, and 16 if you want to add variables. lines 9 and 10 to allow you to view what is available

VariablesinACS <- load_variables(2015, "acs1", cache = TRUE)
#View(VariablesinACS)

Variableindex = 1
Numberofvariables <- 12

Variablesforanalysis <- c("B01003_001", "B25001_001", "B11011_001", "B19019_001", "B01002_001","B23025_004","B08101_049", "B25109_001", "B25031_001", "B19083_001", "B19081_006", "B15003_022")
Variablename <- c("Population", "Households", "Housing Units", "Median Household Income", "Median Age","Number of Employed People", "Employees Working from Home", "Median Home Value", "Median Gross Rent", "Gini Index of Income Inequality", "Income Reached by Top 5% of Earners", "Percent with Bachelor Degrees")

States <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
            "DC")

Fips <- c(
  "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", 
  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
  "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
  "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", 
  "54", "55", "56"
)
  
Statenames <- c(  
                  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                  "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
                  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                  "New Hampshire", "New Jersey", "New Mexico", "New York",
                  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                  "West Virginia", "Wisconsin", "Wyoming"
                  )
  
  
     