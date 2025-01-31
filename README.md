# Google Data Analytics Capstone Project: Cyclistic Bike-Share Analysis

# Project Overview:
 This project analyzes bike-share trip data for Cyclistic, a fictional bike-share company in Chicago.  The goal is to identify trends and patterns in rider behavior to inform marketing strategies and optimize operations.  Specifically, the analysis focuses on understanding the differences between casual riders (those who purchase single-ride or day passes) and members (those who have annual memberships).

# Data Source:
 The data used in this analysis is provided by Cyclistic and consists of trip records from October 2023 to September 2024.  The data includes information such as trip start and end times, locations, rider type (member or casual), and bike type.
## 📂 Dataset
🔗 **Download the dataset:** [cyclistic dataset](https://divvy-tripdata.s3.amazonaws.com/index.html)


# Analysis Objectives:
 1. Compare the ride patterns of members and casual riders.
 2. Identify peak usage times and days for different rider types.
 3. Analyze trip durations and distances for members and casual riders.
 4. Explore the most popular starting and ending stations.
 5. Develop recommendations for Cyclistic based on the analysis findings.

# Code and Data Cleaning:
 The R code below performs data cleaning, transformation, and analysis.  It includes steps such as:
 - Reading and combining multiple CSV files.
 - Converting date and time variables to appropriate formats.
 - Handling missing values and erroneous data (e.g., negative ride lengths).
 - Calculating ride length.
 - Creating new features like day of the week and month.

# Data Analysis and Visualization:
### The analysis involves calculating summary statistics, identifying top stations, and visualizing ride patterns using ggplot2.  Key visualizations include:
 - Total trips and average ride length by day of the week and month, comparing members and casual riders.
 - Maps of popular starting and ending stations (not included in this basic example, but readily added with `ggmap` or similar packages).

# Key Findings and Recommendations:
 - Casual riders tend to take longer trips and are more likely to use the bike-share service on weekends.
 - Members exhibit more consistent usage throughout the week, potentially for commuting.


# Based on these findings, Cyclistic could consider:
 - Offering targeted promotions to casual riders on weekends to encourage membership.
 - Optimizing bike availability at popular stations during peak hours.

# R Code

##libraries
```
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
```

## Set the directory containing the data files.  Adjust if necessary.
```
data_dir <- "Google Data Analytics Project/Data/Data"
```
## List all CSV files in the directory
```
files <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)
```
## Read all CSV files into a list of data frames
```
df_list <- lapply(files, read_csv)
```
## Combine all data frames into a single data frame
```
trip_data <- bind_rows(df_list)
```

## Data Cleaning and Preparation

## Convert start and end times to proper datetime objects
```
trip_data$started_at <- as.POSIXct(trip_data$started_at, format = "%Y-%m-%d %H:%M:%S")
trip_data$ended_at <- as.POSIXct(trip_data$ended_at, format = "%Y-%m-%d %H:%M:%S")
```
## Remove rows with missing values (NA)
```
trip_data_v1 <- na.omit(trip_data)
```
## Calculate ride length in minutes
```
trip_data_v2 <- trip_data_v1 %>%
  mutate(ride_length = difftime(ended_at, started_at, units = "mins"))
```
## Remove rows with negative ride lengths (likely errors)
```
trip_data_v3 <- trip_data_v2 %>%
  filter(ride_length >= 0)
```

## Feature Engineering (extract day of week and month)
```
trip_data_v4 <- trip_data_v3 %>%
  mutate(
    trip_day = format(started_at, "%A"),  # Day of the week
    trip_month = format(started_at, "%B-%y") # Month and Year
  )
```
# Data Analysis

## Summary Statistics (Members vs. Casuals)
```
summary_stats <- trip_data_v4 %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length = mean(ride_length),
    standard_deviation = sd(ride_length),
    median_ride_length = median(ride_length),
    min_ride_length = min(ride_length),
    max_ride_length = max(ride_length)
  )

print(summary_stats)  # Print the summary statistics
```

## Long Usage (Over 24 hours)
```
summary_stats_long_usage <- trip_data_v4 %>%
  filter(ride_length > 1440) %>%
  group_by(member_casual) %>%
  summarise(number_of_trips_over_24h = n())

print(summary_stats_long_usage)
```
## Station List
```
station_list <- trip_data_v4 %>%
  distinct(start_station_name, start_station_latitude, start_station_longitude) # More efficient way to get distinct rows

print(head(station_list))
```
## Top Start Stations (Members)
```
total_start_stations_member <- trip_data_v4 %>%
  filter(member_casual == "member") %>%
  count(start_station_name) %>% # Use count() for concise counting
  filter(start_station_name != "") %>%
  arrange(desc(n))

total_start_stations_member_top30 <- total_start_stations_member %>%
  head(30) %>%
  left_join(station_list, by = "start_station_name") # Use left_join for merging

print(head(total_start_stations_member_top30))
```

## Top Start Stations (Casuals)
```
total_start_stations_casual <- trip_data_v4 %>%
  filter(member_casual == "casual") %>%
  count(start_station_name) %>%
  filter(start_station_name != "") %>%
  arrange(desc(n))

total_start_stations_casual_top30 <- total_start_stations_casual %>%
  head(30) %>%
  left_join(station_list, by = "start_station_name")

print(head(total_start_stations_casual_top30))

```

## Weekly Usage
```
trip_data_v4$trip_day <- factor(trip_data_v4$trip_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) # Factor for correct order

weekly_usage <- trip_data_v4 %>%
  group_by(member_casual, trip_day) %>%
  summarise(total_trips = n(), avg_trip_length = mean(ride_length))
```
## Weekly Usage Plots
```
ggplot(weekly_usage, aes(x = trip_day, y = total_trips, fill = member_casual)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Total Trips per Weekday", subtitle = "Members Vs.Casuals", x = "Day of the Week", y = "Trips", fill = "Member Type") +
  facet_wrap(~member_casual)

ggplot(weekly_usage, aes(x = trip_day, y = avg_trip_length, fill = member_casual)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Average Ride Length per Weekday", subtitle = "Members vs. Casuals", x = "Day of the Week", y = "Avg Ride Length in Minutes", fill = "Member Type") +
  facet_wrap(~member_casual)
```

## Monthly Usage
```
trip_data_v4$trip_month <- factor(trip_data_v4$trip_month, levels = c('October-23','November-23','December-23','January-24','February-24','March-24','April-24','May-24','June-24','July-24','August-24','September-24'))

monthly_usage <- trip_data_v4 %>%
  group_by(member_casual, trip_month) %>%
  summarise(total_trips = n(), avg_trip_length = mean(ride_length))
```
## Monthly Usage Plots
```
ggplot(monthly_usage, aes(x = trip_month, y = total_trips, fill = member_casual)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Total Trips per Month", subtitle = "Member vs.Casuals", x = "Month-Year", y = "Trips", fill = "Member Type") +
  facet_wrap(~member_casual)

ggplot(monthly_usage, aes(x = trip_month, y = avg_trip_length, fill = member_casual)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Avg Trip Length per Month", subtitle = "Members vs. Casuals", x = "Month-Year", y = "Avg Trip Length in Minutes", fill = "Member Type") +
  facet_wrap(~member_casual)
```

## Save the cleaned and prepared data (optional but recommended)
```
write_csv(trip_data_v4, "Google Data Analytics Project/Data/Data/trip_data_cleaned.csv")
```
