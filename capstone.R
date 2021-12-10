# set working directory and load libraries
setwd("~/R/capstone-data-analytics")
library(tidyverse)
library(lubridate)

# import csv files
df1 <- read_csv('data/202101-divvy-tripdata.csv')
df2 <- read_csv('data/202102-divvy-tripdata.csv')
df3 <- read_csv('data/202103-divvy-tripdata.csv')
df4 <- read_csv('data/202104-divvy-tripdata.csv')
df5 <- read_csv('data/202105-divvy-tripdata.csv')
df6 <- read_csv('data/202106-divvy-tripdata.csv')
df7 <- read_csv('data/202107-divvy-tripdata.csv')
df8 <- read_csv('data/202108-divvy-tripdata.csv')
df9 <- read_csv('data/202109-divvy-tripdata.csv')
df10 <- read_csv('data/202110-divvy-tripdata.csv')
df11 <- read_csv('data/202011-divvy-tripdata.csv')
df12 <- read_csv('data/202012-divvy-tripdata.csv')

# combine into a single data frame
df_all <- rbind(df12, df11, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# add row for the time each ride took and make sure it's correct type
df_all <- mutate(df_all, ride_length = ended_at - started_at)
as.numeric(df_all$ride_length)

# add columns that list the date, month, day, and year of each ride.
df_all$date <- as.Date(df_all$started_at) #The default format is yyyy-mm-dd
df_all$month <- format(as.Date(df_all$date), "%m")
df_all$day <- format(as.Date(df_all$date), "%d")
df_all$year <- format(as.Date(df_all$date), "%Y")
df_all$day_of_week <- format(as.Date(df_all$date), "%A")

# save as csv to replace so can delete initial csv files
write.csv(df_all, file = 'data/all-data.csv', row.names=FALSE)

# filter ride lengths under 0 seconds or at invalid start station
# remove columns that won't need for analysis
df_all <- filter(df_all, ride_length > 0 | start_station_name != "HQ QR")
df_all <-subset(df_all, select = -c(ride_id, start_station_name, start_station_id, end_station_name, end_station_id, started_at, ended_at, start_lat, start_lng, end_lat, end_lng))

##########
# ZSCORE
##########

# calculate zscore and create new data frame
df_mean <- mean(df_all$ride_length)
df_sd <- sd(df_all$ride_length)
df_zscore <- (df_all$ride_length-mean(df_all$ride_length))/sd(df_all$ride_length)
df_outliers <- data.frame(df_all, df_zscore)

# remove rows that have a zscore +/- 3 and add back to the 
no_outliers <- df_outliers[!(df_zscore>3), ]
df_all <- no_outliers
df_all <-subset(df_all, select = -c(df_zscore))

##########
# VISULIZATIONS
##########

##########
# BAR CHART OF WEEK
##########

# first analysis we need to order the days 
df_all$day_of_week <- ordered(df_all$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# create new df that gets the mean ride length by member type and day of week
counts <- aggregate(df_all$ride_length ~ df_all$member_casual + df_all$day_of_week, FUN = mean)
colnames(counts) <- c('member_casual', 'day_of_week', 'ride_length')
counts <- mutate(counts, ride_length = ride_length / 60)  # change to minutes

# graph bar chart of both types
ggplot(counts, aes(day_of_week, ride_length, fill = member_casual)) +
  geom_col(position = "dodge") + 
  xlab('Day of the Week') + 
  ylab('Ride Length')

##########
# LINE CHART OF MONTHS
##########

# create new df that gets the mean ride length by member type and month
mo_counts <- aggregate(df_all$ride_length ~ df_all$member_casual + df_all$month, FUN = mean)
colnames(mo_counts) <- c('member_casual', 'month', 'ride_length')
mo_counts <- mutate(mo_counts, ride_length = ride_length / 60)

# second analysis we need to order the months 
mo_counts$month <- as.numeric(mo_counts$month)
mo_counts$month <- as.character(month(ymd(010101) + months(mo_counts$month-1),label=TRUE,abbr=TRUE))
mo_counts$month <- ordered(mo_counts$month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"))

# graph line chart of both types
ggplot(mo_counts, mapping = aes(x=month, y=ride_length, group = member_casual, color = member_casual)) + 
  geom_line() +
  xlab('Month') + 
  ylab('Ride Length')

##########
# PIE CHART OF BIKE TYPES CASUAL
##########

# create two dfs that show ride length by type of bike for each rider type
type_df <- select(df_all, member_casual, ride_length, rideable_type)
type_df <- mutate(type_df, ride_length = ride_length / 60)

# create casual data frame 
c_df <- filter(type_df, member_casual == 'casual')
c_df <- aggregate(c_df$ride_length ~ c_df$member_casual + c_df$rideable_type, FUN = mean)
colnames(c_df) <- c('member_casual', 'rideable_type', 'ride_length')

# prep data for pie chart so it will show percentages
# https://dk81.github.io/dkmathstats_site/rvisual-piecharts.html
c_df <- c_df %>% 
  mutate(
    percent = (paste0(round((ride_length/ sum(ride_length)) * 100, 1), "%")),
    levels = (rideable_type[length(rideable_type):1]),
    cumulative = (cumsum(ride_length)),
    midpoint = cumulative - ride_length / 2)

# graph pie chart
ggplot(c_df, aes(x="", y=ride_length, fill=rideable_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x = 1.2, y = midpoint , label = percent), color="black",
            fontface = "bold") +
  theme_void() +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5))

##########
# PIE CHART OF BIKE TYPES MEMBER
##########

# create member data frame 
m_df <- filter(type_df, member_casual == 'member')
m_df <- aggregate(m_df$ride_length ~ m_df$member_casual + m_df$rideable_type, FUN = mean)
colnames(m_df) <- c('member_casual', 'rideable_type', 'ride_length')

# prep data for pie chart so it will show percentages
# https://dk81.github.io/dkmathstats_site/rvisual-piecharts.html
m_df <- m_df %>% 
  mutate(
    percent = (paste0(round((ride_length/ sum(ride_length)) * 100, 1), "%")),
    levels = (rideable_type[length(rideable_type):1]),
    cumulative = (cumsum(ride_length)),
    midpoint = cumulative - ride_length / 2)

# graph pie chart
ggplot(m_df, aes(x="", y=ride_length, fill=rideable_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x = 1.2, y = midpoint , label = percent), color="black",
            fontface = "bold") +
  theme_void() +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5))

##########
# SAVE FILES
##########

# save all dfs used to create graphics as csv files
write.csv(counts, file = 'data/avg_ride_length.csv', row.names=FALSE)
write.csv(mo_counts, file = 'data/avg_ride_length_mo.csv', row.names=FALSE)
write.csv(c_df, file = 'data/casual_ridebytype.csv', row.names=FALSE)
write.csv(m_df, file = 'data/member_ridebytype.csv', row.names=FALSE)