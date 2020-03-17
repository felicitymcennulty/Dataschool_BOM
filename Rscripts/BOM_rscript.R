#10/03/19
#dataschool BOM 

#https://csiro-data-school.github.io/r/15-Reproducibility/index.html

#Download two data files from the Bureau of Meterology, 
  #one containing meterological information, and 
  #one containing metadata about weather stations
#Take some time to explore the data files and understand what they contain

library(tidyverse)
BOM_data <- read_csv("Data/BOM_data.csv")
BOM_stations <- read_csv("Data/BOM_stations.csv")

#show data
BOM_data
head(BOM_data)
tail(BOM_data)
summary(BOM_data)

#Question 1
#For each station, how many days have a minimum temperature, 
#a maximum temperature and a rainfall measurement recorded?


##separate

#need to separate temp_min_max "12/22"
?separate
BOM_separated <- BOM_data %>% 
                separate(Temp_min_max, into = c("Min_temp", "Max_temp"),
                          sep = "/")
BOM_separated

##filter

# filter() the data to keep only rows that have minimum temperature, 
#maximum temperature, and rainfall measurements.
#filter(gapminder, country == "Australia", year >= 1997)
BOM_filtered <- BOM_separated %>%
                filter(Min_temp != "-", Max_temp != "-", Rainfall != "-")

#A group_by() followed by summarise() will then allow you to 
#count the number of rows remaining for each station.

#group by station
BOM_grouped <- BOM_filtered %>% 
                group_by(Station_number) 
#see tibble header - there are 20 stations

BOMbyStn_countdays <- BOM_grouped%>%summarise(n_days = n())

#to nest these so you don't make lots of dataframes


#For each station, how many days have a minimum temperature, 
#a maximum temperature and a rainfall measurement recorded?