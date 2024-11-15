library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)

dir("Google Data Analytics Project\\Data\\Data",full.names=1)

#loading each datasets
df1<-read.csv("Google Data Analytics Project\\Data\\Data/202310-divvy-tripdata.csv")
df2<-read.csv("Google Data Analytics Project\\Data\\Data/202311-divvy-tripdata.csv")
df3<-read.csv("Google Data Analytics Project\\Data\\Data/202312-divvy-tripdata.csv")
df4<-read.csv("Google Data Analytics Project\\Data\\Data/202401-divvy-tripdata.csv")
df5<-read.csv("Google Data Analytics Project\\Data\\Data/202402-divvy-tripdata.csv")
df6<-read.csv("Google Data Analytics Project\\Data\\Data/202403-divvy-tripdata.csv")
df7<-read.csv("Google Data Analytics Project\\Data\\Data/202404-divvy-tripdata.csv")
df8<-read.csv("Google Data Analytics Project\\Data\\Data/202405-divvy-tripdata.csv")
df9<-read.csv("Google Data Analytics Project\\Data\\Data/202406-divvy-tripdata.csv")
df10<-read.csv("Google Data Analytics Project\\Data\\Data/202407-divvy-tripdata.csv")
df11<-read.csv("Google Data Analytics Project\\Data\\Data/202408-divvy-tripdata.csv")
df12<-read.csv("Google Data Analytics Project\\Data\\Data/202409-divvy-tripdata.csv")

#combining 12 datasets into one
trip_data<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

dim(trip_data)

write.csv(trip_data, "Google Data Analytics Project\\Data\\Data\\trip_data.csv")


head(trip_data)


#cleaning data

str(trip_data)

trip_data$started_at <- as.POSIXct(trip_data$started_at, format = "%Y-%m-%d %H:%M:%S")
trip_data$ended_at <- as.POSIXct(trip_data$ended_at, format = "%Y-%m-%d %H:%M:%S")
str(trip_data)


#removing all NA/null values(5854544 to 5847103)

trip_data_v1<-na.omit(trip_data)

dim(trip_data_v1)

#praparing data for analysis

#adding ride-length column

trip_data_v2<-mutate(trip_data_v1,ride_length=difftime(ended_at,started_at,units = "mins"))

View(trip_data_v2)


str(trip_data_v2)

#to calculate -ve ride lengtrh rows
nrow(trip_data_v2[trip_data_v2$ride_length < 0,])

## We use the ! to NOT show observations where ride_length < 0.
trip_data_v3 <- trip_data_v2[!trip_data_v2$ride_length < 0,]
head(trip_data_v3, 5)


#to calculate -ve ride lengtrh rows
nrow(trip_data_v3[trip_data_v3$ride_length < 0,])

#Analysing the Data 



#Some Quick statistics-members vs.Casual

summary_stats<-trip_data_v4 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_length=mean(ride_length),standard_deviation=sd(ride_length),median_ride_length=median(ride_length),min_ride_length=min(ride_length),max_ride_length=max(ride_length))
head(summary_stats)

View(summary_stats)


#Let's see how many members are keeping the bikes for more than 24 hours:

## We will filter for ride length > 1440 minutes, which is 24 hours.
## We will also group by member_casual field to compare between casuals and members
summary_stats_long_usage<- trip_data_v4 %>% 
  filter(trip_data_v4$ride_length > 1440) %>% 
  group_by(member_casual) %>% 
  summarise(number_of_trips_over_24h = n())
head(summary_stats_long_usage)

## To extract the station list we will extract columns 5, 9 and 10 from out main dataset. Then we will remove the duplicated station names to end up with a our station list.

station_list<-trip_data_v4[,c(5,9,10)]
station_list<-station_list[!duplicated(station_list$start_station_name),]
nrow(station_list)
head(station_list,5)


## This code creates the dataset for members
total_start_stations_member<-trip_data_v4 %>% 
  filter(member_casual=="member") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts=n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(-number_of_starts)

## Then we create a top 30 list and merge with stations list to get lat and long info.

total_start_stations_member_top30<-(merge(head(total_start_stations_member,30),station_list)) %>% 
  arrange(-number_of_starts)

head(total_start_stations_member_top30,5)



total_start_stations_casual<-trip_data_v4 %>% 
  filter(member_casual=='casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts=n()) %>% 
  filter(start_station_name!="") %>% 
  arrange(-number_of_starts)

total_start_stations_casual_top30<-merge(head(total_start_stations_casual,30),station_list) %>% 
  arrange(-number_of_starts)

head(total_start_stations_casual_top30)


#Weekly and Monthly usage-Member vs.Casuals

#Weekly Comparision of trip frequency and Avg Trip Length

## First, we will order the dataset by the specified vector. This is because in default R arranges by alphabet. 
trip_data_v4$trip_day<-ordered(trip_data_v4$trip_day,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## Then we will create a new dataset which will summarise our total trips and average trip length information.
weekly_usage<-trip_data_v4 %>% 
  group_by(member_casual,trip_day) %>% 
  summarise(total_trips=n(),avg_trip_length=mean(ride_length))

head(weekly_usage, 14)


#Now, we plot our findings on trip frequency:

ggplot(data = weekly_usage)+
  geom_col(mapping=aes(x=trip_day,y=total_trips,fill = member_casual))+
  theme(axis.text.x = element_text(angle=45))+
  labs(title='Total Trips per weekday',subtitle='Members Vs.Casuals',x='Day of the Week',y='Trips',fill='Member Type')+
  facet_wrap(~member_casual)

#Now, let's plot our findings on average trip length:

ggplot(data=weekly_usage)+geom_col(mapping = aes(x=trip_day,y=avg_trip_length,fill=member_casual))+
  theme(axis.text.x=element_text(angle=45))+
  labs(title='Average Ride Length per Weekday',subtitle='Members vs. Casuals',x='Day of the Week',y='Avg Ride Length in Minutes',fill='Member Type')+
  facet_wrap(~member_casual)






#Monthly Comparison of Trip Frequency and Average Trip Length¶

#same as weekly

## First, we will order the dataset by the specified vector. This is because in default R arranges by alphabet.
trip_data_v4$trip_month<-ordered(trip_data_v4$trip_month,levels=c('October-23','November-23','December-23','January-24','February-24','March-24','April-24','May-24','June-24','July-24','August-24','September-24'))

Monthly_usage<-trip_data_v4 %>% 
  group_by(member_casual,trip_month) %>% 
  summarise(total_trips=n(),avg_trip_length=mean(ride_length))
head(Monthly_usage)

#Let's plot our findings on monthly trip frequency:

ggplot(data=Monthly_usage)+geom_col(mapping = aes(x=trip_month,y=total_trips,fill = member_casual))+
  theme(axis.text.x = element_text(angle=90))+
  labs(title='Total Trips per Month',subtitle='Member vs.Casuals',x='Month-Year',y='Trips',fill='Member Type')+
  facet_wrap(~member_casual)

#plot our average trip length:

ggplot(data = Monthly_usage)+
  geom_col(mapping = aes(x=trip_month,y=avg_trip_length,fill = member_casual))+
  theme(axis.text.x = element_text(angle=90))+
  labs(title='Avg Trip Length per Month',subtitle='Members vs. Casuals',x='Month-Year',y='Avg Trip Length in Minutes',file='Member Type')+
  facet_wrap(~member_casual)
