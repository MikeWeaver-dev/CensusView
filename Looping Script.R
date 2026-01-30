#Looping Through Years

#You can edit these two variables to change the years shown
Startyear <- 2024
Endyear <- 2024

Yearsavailable <- c(2020, 2021, 2022, 2023, 2024)

#don't edit this part
Year <- Startyear

while (Endyear >= Year) {
  
  source(paste0(getwd(),"/Defining Variables.R"))
  source(paste0(getwd(),"/Data Collection.R"))
  source(paste0(getwd(),"/Data Cleaning.R"))
  
  Year = Year + 1
  
}


