#the data came in with odd names and needs some cleaning and organizing before we can map and present it. This code accomplishes that

library(tidyverse)
library(sf)

DF1 <- data.frame(Variablename, Variablesforanalysis)
DF2 <- data.frame(Fips, Statenames)

#Updating variable names
colnames(DF1)[2] <- "variable"

Staterawdata <- merge(Staterawdata, DF1, by="variable")
Countyrawdata <- merge(Countyrawdata, DF1, by="variable")
Tractrawdata <- merge(Tractrawdata, DF1, by="variable")

#adding state names to tract
Tractrawdata <- Tractrawdata %>% 
  mutate (Fips = GEOID)

Tractrawdata$Fips<- substr(Tractrawdata$Fips, 1, 2)

Tractrawdata <- merge(Tractrawdata, DF2, by="Fips")

rm(DF1, DF2)

#transpose Data Frames
Statedata <- data.frame(unique(Staterawdata[,c(2,3)]))
Countydata <- data.frame(unique(Countyrawdata[,c(2,3)]))
Tractdata <- data.frame(unique(Tractrawdata[,c(3,4,8)]))

while (Variableindex <= Numberofvariables){
  
  #Adding variable names for State level
  Statedatacleaning <- Staterawdata %>% 
    filter (variable == Variablesforanalysis[Variableindex]) %>% 
    select(estimate, GEOID) %>% 
    st_set_geometry(NULL)
  
  colnames(Statedatacleaning)[1] <- Variablename[Variableindex]
  
  Statedata <- merge(Statedatacleaning, Statedata, by="GEOID")
  
  
  # Adding variable names for County level
   Countydatacleaning <- Countyrawdata %>% 
    filter (variable == Variablesforanalysis[Variableindex]) %>% 
    select(estimate, GEOID) %>% 
    st_set_geometry(NULL)
  
   colnames(Countydatacleaning)[1] <- Variablename[Variableindex]
  
   Countydata <- merge(Countydatacleaning, Countydata, by="GEOID")
  
  
  #Adding variable names for Tract level
  Tractdatacleaning <- Tractrawdata %>% 
    filter (variable == Variablesforanalysis[Variableindex]) %>% 
    select(estimate, GEOID) %>% 
    st_set_geometry(NULL)
  
  colnames(Tractdatacleaning)[1] <- Variablename[Variableindex]
  
  Tractdata <- merge(Tractdatacleaning, Tractdata, by="GEOID")
  
  Variableindex = Variableindex + 1

}
#Two new calculation
Statedata <- Statedata %>% 
  mutate(`Percent with Bachelor Degrees` = 100 * `Percent with Bachelor Degrees` / Households)
Statedata <- Statedata %>% 
  mutate(`Gini Index of Income Inequality` = 100 * `Gini Index of Income Inequality`)

Countydata <- Countydata %>% 
  mutate(`Percent with Bachelor Degrees` = 100 * `Percent with Bachelor Degrees` / Households)
Countydata <- Countydata %>% 
  mutate(`Gini Index of Income Inequality` = 100 * `Gini Index of Income Inequality`)

Tractdata <- Tractdata %>% 
  mutate(`Percent with Bachelor Degrees` = 100 * `Percent with Bachelor Degrees` / Households)
Tractdata <- Tractdata %>% 
  mutate(`Gini Index of Income Inequality` = 100 * `Gini Index of Income Inequality`)


#this turns the dataframe into an SF the map can read
Statedata <- st_as_sf(Statedata, sf_column_name = "geometry")
Statedata <- st_transform(Statedata, 4326)

Countydata <- st_as_sf(Countydata, sf_column_name = "geometry")
Countydata <- st_transform(Countydata, 4326)

Tractdata <- st_as_sf(Tractdata, sf_column_name = "geometry")
Tractdata <- st_transform(Tractdata, 4326)



assign(paste0("State",Year),Statedata)
assign(paste0("County",Year),Countydata)
assign(paste0("Tract",Year),Tractdata)

rm(Staterawdata, Statedata, Statedatacleaning, Countyrawdata, Countydata, Countydatacleaning, Tractrawdata, Tractdata, Tractdatacleaning)


