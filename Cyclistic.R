#setting the working directory
setwd("C:/Users/sinsi/Downloads/202307-202406_Cyclistic")

library(tidyverse)
library(dplyr)
library(lubridate)

#get all the files with .csv data type
#in the working directory which is our folder containing the data
#and assign it to a variable
aggregate_files <- list.files(pattern = "*.csv")

#read the .csv files in the aggregate_files
#and map it into a single data frame
aggregate_data <- map_df(aggregate_files, read_csv)

str(aggregate_data)

#create a new data frame with a clean data, and we will name it as cyclistic_clean_data)
#Step 1: Assigning a variable
cyclistic_clean_data <- aggregate_data %>% 
  #Step 2: Selecting the necessary columns for analysis
    select("ride_id", "rideable_type", "started_at", "ended_at",
         "start_station_name", "end_station_name", "member_casual") %>% 
  #Step 3: Removing all the missing values
  na.omit() %>% 
  #Step 4: Creating new columns needed for analysis
  mutate(trip_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
         weekday = format(as.Date(started_at), "%A")) %>% 
  #Step 5: Select the columns with the trip_length and weekday added
  select("ride_id", "rideable_type","started_at", "ended_at", "start_station_name", "end_station_name",
         "weekday", "trip_length", "member_casual") %>% 
  #filter(trip_length >=1, trip_length <= (24*60))

#Step 6: Filter the trip_length
min(cyclistic_clean_data$trip_length)
max(cyclistic_clean_data$trip_length)

dcolSums(is.na(cyclistic_clean_data))

any(duplicated(cyclistic_clean_data$ride_id))
cyclistic_clean_data <- cyclistic_clean_data[!duplicated(cyclistic_clean_data$ride_id), ]
any(duplicated(cyclistic_clean_data$ride_id))

min(cyclistic_clean_data$trip_length)
max(cyclistic_clean_data$trip_length)

str(cyclistic_clean_data)
