#setting the working directory
setwd("C:/Users/sinsi/Downloads/202307-202406_Cyclistic/dataset")

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

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
  filter(trip_length >=1, trip_length <= (24*60))

#Step 6: Filter the trip_length
min(cyclistic_clean_data$trip_length)
max(cyclistic_clean_data$trip_length)

colSums(is.na(cyclistic_clean_data))

any(duplicated(cyclistic_clean_data$ride_id))
cyclistic_clean_data <- cyclistic_clean_data[!duplicated(cyclistic_clean_data$ride_id), ]
any(duplicated(cyclistic_clean_data$ride_id))

min(cyclistic_clean_data$trip_length)
max(cyclistic_clean_data$trip_length)

str(cyclistic_clean_data)

#Comparing the number of rides permonth
month_count = cyclistic_clean_data %>% 
  group_by(months = month.name[month(started_at)], member_casual) %>% 
  summarize(row_count = n()) %>% 
  arrange(match(months,month.name))
#save the data frame to import into tableau
write.csv(month_count, "month count.csv", row.names=FALSE)
head(month_count)

#Comparing bike usage on days of theweek
weekday_count = cyclistic_clean_data %>% 
  group_by(weekday = weekday, member_casual = member_casual) %>% 
  summarize(row_count = n())
write.csv(weekday_count, "weekday count.csv", row.names=FALSE )

#Top 10 starting and ending stations
top_start_station <- cyclistic_clean_data %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(row_count = n()) %>% 
  arrange(desc(row_count))

top_end_station <- cyclistic_clean_data %>% 
  group_by(end_station_name, member_casual) %>% 
  summarize(row_count = n()) %>% 
  arrange(desc(row_count))

# Calculate average riding duration for members and casual riders
avg_duration <- cyclistic_clean_data %>%
  group_by(member_casual) %>%
  summarise(avg_duration_mins = mean(trip_length, na.rm = TRUE))
print(avg_duration)
# Plotting
ggplot(avg_duration, aes(x = member_casual, y = avg_duration_mins, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rider Type", y = "Average Riding Duration (minutes)",
       title = "Average Riding Duration Between Members and Casual Riders") +
  theme_minimal()
#average number of rides by hour
# Ensure 'started_at' is in POSIXct format
cyclistic_clean_data$started_at <- ymd_hms(cyclistic_clean_data$started_at)
# Extract hour from 'started_at' timestamp
cyclistic_clean_data <- cyclistic_clean_data %>%
  mutate(hour = hour(started_at))
# Calculate average number of rides by hour for each rider type
avg_rides_by_hour <- cyclistic_clean_data %>%
  group_by(member_casual, hour) %>%
  summarise(avg_rides = n() / n_distinct(date(started_at)))  # Average rides per hour per day
# Plotting with facetting
ggplot(avg_rides_by_hour, aes(x = hour, y = avg_rides)) +
  geom_line() +
  geom_point() +
  labs(x = "Hour of Day", y = "Average Number of Rides") +
  scale_x_continuous(breaks = 0:23) +  # Ensure x-axis shows all hours
  facet_wrap(~ member_casual, scales = "free_y", nrow = 3) +  # Facet by member_casual
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))  # Adjust facet labels if needed

#Usage of Different Bikes: Members vs. Casual Riders
# Calculate bike usage frequency for members
member_bike_usage <- cyclistic_clean_data %>%
  filter(member_casual == "member") %>%
  count(rideable_type) %>%
  mutate(rider_type = "Member")
# Calculate bike usage frequency for casual riders
casual_bike_usage <- cyclistic_clean_data %>%
  filter(member_casual == "casual") %>%
  count(rideable_type) %>%
  mutate(rider_type = "Casual")
# Combine data for plotting
combined_usage <- bind_rows(member_bike_usage, casual_bike_usage)
# Plotting
ggplot(combined_usage, aes(x = rideable_type, y = n, fill = rider_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Rideable Type", y = "Number of Rides",
       title = "Usage of Different Bikes: Members vs. Casual Riders") +
  scale_fill_manual(values = c("Member" = "#0072B2", "Casual" = "#D55E00")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20 , hjust = 1))
