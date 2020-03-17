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

#to nest these so you don't make lots of dataframes link with pipes
BOM_stncountdays <- BOM_separated %>%
                filter(Min_temp != "-", Max_temp != "-", Rainfall != "-")%>%
                group_by(Station_number) %>%
                summarise(n_days = n())

#Which month saw the lowest average daily temperature difference?

#this question will need a mutate() to calculate the temperature difference.
#The temperature values are stored as characters after you have run separate() 
#(see the <chr> in the second row if you print the data frame to the console).
#To be able to calculate the difference without an error, 
#you will need to convert them to numeric values with as.numeric() first.
#For rows that are missing a temperature measurement, 
#the temperature difference will be NA.
#How will you deal with these in the rest of the analysis?

?mutate
#as.numeric(Min_temp)
#change char to numeric 
BOM_temp_numeric <- mutate(BOM_filtered, min = as.numeric(Min_temp), 
                      max = as.numeric(Max_temp))

#to mutate to numeric and do maths at same time
BOM_temp_diff <- BOM_separated  %>% 
              mutate(Temp_diff = as.numeric(Max_temp)- as.numeric(Min_temp))
BOM_temp_diff
view(BOM_temp_diff)

#to deal with missing data use NA
BOM_temp_diff <- BOM_separated  %>% 
  mutate(Temp_diff = as.numeric(Max_temp)- as.numeric(Min_temp)) %>% #to mutate to numeric and do maths at same time
  group_by(Month) %>% #group by month
  summarise(Av_daily_temp = mean(Temp_diff, na.rm = TRUE)) %>% #calculate mean temp and remove null values 
  arrange(Av_daily_temp) #sort order by specifying column



