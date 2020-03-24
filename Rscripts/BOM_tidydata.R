#18/03/19
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

?separate

BOM_tidy_data <- BOM_data %>% 
                separate(Temp_min_max, into = c("Min_temp", "Max_temp"),sep = "/") %>% 
                #used to separate temp_min_max "12/22" to own columns
                mutate(Min_temp = as.numeric(Min_temp), Max_temp = as.numeric(Max_temp),
                       Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure)) 
                #convert chr to numbers for all columns that need changing 


#Q1. For each station, how many days have a minimum temperature, 
#a maximum temperature and a rainfall measurement recorded?

#process to in code below:
# filter() the data to keep only rows that have minimum temperature, 
#maximum temperature, and rainfall measurements.
#A group_by() followed by summarise() will then allow you to 
#count the number of rows remaining for each station.
Q1_BOM_stncountdays <- BOM_tidy_data %>% 
                filter(Min_temp != "-", Max_temp != "-", Rainfall != "-")%>% 
                # filter() to keep only rows that have min temp, max temp, and rainfall.
                group_by(Station_number) %>%  #see tibble header - there are 20 stations
                summarise(n_days = n()) #count the number of rows remaining for each station

Q1_BOM_stncountdays #show answer on screen

#Answer for Q1 shown in n_days column

#save answer to Q1 to a file in the results folder
write.csv(Q1_BOM_stncountdays, "Results/Q1_BOM_stncountdays.csv")



#Q2. Which month saw the lowest average daily temperature difference?

#process to follow in code below;
#this question will need a mutate() to calculate the temperature difference.

##--note - now have fixed below chr data issues in BOM_tidy_data dataframe at top of code
#The temperature values are stored as characters after you have run separate() 
#(see the <chr> in the second row if you print the data frame to the console).
#To be able to calculate the difference without an error, 
#you will need to convert them to numeric values with as.numeric() first.
#For rows that are missing a temperature measurement, 
#the temperature difference will be NA.
#How will you deal with these in the rest of the analysis?

?mutate

Q2_BOM <- BOM_tidy_data  %>% 
          mutate(Temp_diff = Max_temp-Min_temp) %>% #calculate difference between min and max
          group_by(Month) %>% #group by month (see - there is 12 groups in tibble)
          summarise(Av_daily_temp = mean(Temp_diff, na.rm = TRUE)) %>% #calculate mean temp and remove null values 
          arrange(Av_daily_temp) #sort order by specifying column so lowest is at top of table


#note alternatively could use filter to remove null values (!is.na):
#Q2_BOM <- BOM_tidy_data  %>% 
#          mutate(Temp_diff = Max_temp-Min_temp) %>% #calculate difference between min and max
#          filter(!is.na(Temp_diff)) %>% 
#          group_by(Month) %>% #group by month (see - there is 12 groups in tibble)
#          summarise(Av_daily_temp = mean(Temp_diff)) %>% #calculate mean temp and remove null values 
#          arrange(Av_daily_temp) #sort order by specifying column so lowest is at top of table

Q2_BOM
#Answer Q2 The month with the lowest average daily temperature difference is month = 6 = June

#save answer to Q2 to a file in the results folder
write.csv(Q2_BOM, "Results/Q2_BOM_avdailytempbymonth.csv")


#for Q3 need to tidy BOM_stations

#To tidy it before merging, you will need to gather() the station data into an 
#intermediate form that has three columns, one for the station ID number, 
#one for the type of data being recorded (the info column in the original data), and 
#one for the actual recorded value itself. (Is this intermediate data tidy?)

#This data frame can then be spread() into a shape with one row for each station. 
#Remember that the key argument to spread() identifies the column that will provide 
#the data for the new column names, and the value argument identifies the column that 
#will provide the data for the new cells.

#gather
#cows_long <- gather(cows, key = weight_type, value = weight, -id) 
#gather - rearranges all columns except ones that you "minus"

BOM_tidy_stations <- BOM_stations %>% #column names are the station ids in orginal
  gather(key = "Station_number", value = "Values", -info) %>%        #match column name in BOM_tidy_data
  # is the same as gather(Station_number, Values, -info)
  #exclude "info" and switch the rest of the data as "info" has the row names that we need to spread to column names
  spread(key = info, value = Values) %>% 
 #is the same as spread(info, Values)
  mutate(Station_number = as.numeric(Station_number)) #change this to match column type in BOM_tidy_data

#Q3 Which state saw the lowest average daily temperature difference?

#Finally, you will want to join the two datasets together to identify the state of 
#each weather station. If you run into errors at this step, check that the two data frames
#have a shared column to merge, and that they are the same data type 
#(eg. you can’t merge a character column with a numeric column).
#full_join(df1, df2)
#make sure column names are the same Station_ID/Station_number
#rename(gapminder, country_name = country, population = pop)
#new_name = old_name


BOM_merged_tidy <- full_join(BOM_tidy_data,BOM_tidy_stations ) #Stephen used a left join

#Which state saw the lowest average daily temperature difference?

Q3_BOM_merged_temp_diff <- BOM_merged_tidy  %>% 
  mutate(Temp_diff = (Max_temp)- (Min_temp)) %>% #to mutate to do maths at same time
  group_by(state) %>% #group by month
  summarise(Av_daily_temp = mean(Temp_diff, na.rm = TRUE)) %>% #calculate mean temp and remove null values 
  arrange(Av_daily_temp) #sort order by specifying column

Q3_BOM_merged_temp_diff
#Answer Q3
#BOM_merged_temp_diff #QLD av daily temp diff is 7.36
#save answer to Q3 to a file in the results folder
write.csv(Q3_BOM_merged_temp_diff, "Results/Q3_BOM_av_temp_diff.csv")

#Q4, Does the westmost (lowest longitude) or eastmost (highest longitude) weather station
# in our dataset have a higher average solar exposure?

Q4_BOM_lon_sol_exp <- BOM_merged_tidy %>% 
    group_by(lon, state, Station_number) %>%
    summarise(Av_solar_exp = mean(Solar_exposure, na.rm = TRUE)) %>%
    #filter(Av_solar_exp > 0) #this removes the null
    filter(!is.nan(Av_solar_exp) ) %>%  #this removes nan values
    #calculate mean solar exp and remove null values 
    ungroup() %>% #this is needed because n() doesn't work otherwise, can group/ungroup to do multiple averages eg. state then lon
    # slice(-2: - (n()-1)) #this deletes rows 2 to (n= end row) minus 1
    filter(lon == max(lon) |lon == min(lon))

#Answer Q4
Q4_BOM_lon_sol_exp
#highest av solar average is at eastmost (highest longitude) weather station

#save answer to Q4 to a file in the results folder
write.csv(Q4_BOM_lon_sol_exp, "Results/Q4_BOM_lon_sol_exp.csv")



#note Stephen did his slightly differently
#q4_ans <- BOM_combined %>% 
#  mutate(Solar_exposure = as.numeric(Solar_exposure), lon = as.numeric(lon)) %>%
#  # Could have combined these into one filter function, but I'll separate them for a clearer example
#  filter(!is.na(Solar_exposure)) %>% 
#  filter(lon == min(lon) | lon == max(lon)) %>% # Could instead use range(): lon %in% range(lon)
#  group_by(Station_number, lon) %>% # Group by lon as well so that it appears in our final table
#  summarise(avg_solar_exp = mean(Solar_exposure))


?slice


