#---------------------------------------------------------------------------------
# Title: Data Preparation of the project
# Author: Rahat, A.D.
# Date: 2023-11-25
# Description: This scripts prepare the data for the further analysis and modeling
#---------------------------------------------------------------------------------


# clear environment
rm(list = ls(all.names = TRUE))

# add the library
library(dplyr)
library(ggplot2)
library(lubridate)

setwd('../ahmed/Desktop/ADR/ML-2/')

# Load the dataset
df <- read.csv("projects/nyc_bus_delay_prediction/dataset/nyc_traffic_sample.csv")

head(df)

names(df) <- c('recorded_at', 'direction', 'line_name', 'org_name', 'org_lat', 'org_long', 
               'dest_name', 'dest_lat', 'dest_long', 'vech_name', 'vech_lat', 
               'vech_long', 'next_point_name', 'arrivial_app', 'dist_from_stop', 
               'expected_arr_time', 'schedule_arr_time')



# Calculate the percentage of nan in each columns
na_count <- colMeans(is.na(df)) * 100

# Print or inspect the percentage of N/A values in each column
print(na_count)

# only pick the percentage which has more than 0%
selected_columns <- na_count[na_count > 0]

# Convert to dataframe for ggplot
na_percentage_df <- data.frame(
  Column = names(selected_columns),
  Percentage = selected_columns
)

# Set the factor levels to maintain the order
na_percentage_df$Column <- factor(na_percentage_df$Column, levels = names(selected_columns))

# Remove row names
rownames(na_percentage_df) <- NULL


# Bar plot using ggplot without legend and with the original order
ggplot(na_percentage_df, aes(x = Column, y = Percentage, fill = Column)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Missing Values in Columns",
       y = "Percentage", x = "Columns") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)), vjust = -0.5, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Nan is any columns of the dataframe
total_rows_with_nulls <- sum(!complete.cases(df))

# Print the result
cat("Total number of rows with any null values:", total_rows_with_nulls, "\n")


# Remove rows where col1 has NA values
clean_df <- df[!is.na(df$expected_arr_time) & !is.na(df$schedule_arr_time), ]

# new row counts
new_row_counts <- nrow(clean_df)

# Print the result
cat("After Null removal total rows become:", new_row_counts, "\n")
cat("Total deducted rows are:", nrow(df) - new_row_counts, "\n")


# now check the null entries again

# count total null entries
(new_na_count <- colSums(is.na(clean_df)))

# only pick the percentage which has more than 0
(new_selected_columns <- new_na_count[new_na_count > 0])

# We can see that only 1 entry has null values. So we also ignore this one entry
clean_df <- na.omit(clean_df)

cat("Final Dataset have:", nrow(clean_df), "Entries\n")


# now calculate the delay time of the dataset


# see the datatype of the two columns which have time data
class(clean_df$expected_arr_time)
class(clean_df$schedule_arr_time)
# both the columns have character types

# convert string to date-time
clean_df$expected_arr_time_tt <- as.POSIXct(clean_df$expected_arr_time, format="%Y-%m-%d %H:%M:%S")

clean_df$expected_arr_time_tt

clean_df$schedule_arr_time_tt <- as.POSIXct(clean_df$schedule_arr_time, format="%Y-%m-%d %H:%M:%S")

clean_df$schedule_arr_time_tt
# while converting the schedule arrival time, we found a lots of Nan, so we further investigate the issue

schedule_time <- clean_df$schedule_arr_time

# found a lot of values start with 24 and 25. So, make them 00 and 01.

# Filter rows where column_name starts with a number from 24 to 48
filtered_df <- clean_df[grepl("^(2[4-9]|3[0-9]|4[0-8])", clean_df$schedule_arr_time), ]

nrow(filtered_df)

# print the snapshort of the erroneous data
print(filtered_df[, c("expected_arr_time", "schedule_arr_time")])

# get the date from expected arrival time

# split the columns to date and time
split_columns <- strsplit(clean_df$expected_arr_time, " ")

# Creating two new columns
schedule_date <- sapply(split_columns, function(x) x[1])

# this function replace the string 24 with 00 and 25 with 01
replace_time <- function(time_string) {
  if(startsWith(time_string, "24")) {
    time_string <- gsub("^24", "00", time_string)
  } else if(startsWith(time_string, "25")) {
    time_string <- gsub("^25", "01", time_string)
  }
  return(time_string)
}

# make the changes into all string of the schedule time
new_schedule_time <- lapply(schedule_time, replace_time)

# Concatenating date and time
new_date_time <- mapply(paste, schedule_date, new_schedule_time, MoreArgs = list(sep = " "))

names(new_date_time) <- NULL

# now convert the string to datetime object
clean_df$schedule_arr_time_tt <- as.POSIXct(new_date_time, format="%Y-%m-%d %H:%M:%S")

# check if there any null values encounter
print(clean_df[is.na(clean_df$schedule_arr_time_tt) | is.na(clean_df$expected_arr_time_tt), 
               c("expected_arr_time", "schedule_arr_time", "expected_arr_time_tt", "schedule_arr_time_tt")])

# Finally all the values of time are converted to datetime format

# Now time to calculate the delay

clean_df$delay_mins <- as.numeric(difftime(clean_df$expected_arr_time_tt, 
                                      clean_df$schedule_arr_time_tt, units = "mins"))

# the delay time is calculated and we saw some of the delay time is negative because of the early arrival

# Assuming your dataframe is named df and the delay column is named delay
ggplot(clean_df, aes(y = delay_mins)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Bus Delays", x = "", y = "Delay Time")

# Here, we can see that the delay_mins is sometimes a huge negative, so we investigate the issue

# check if there any null values encounter
print(clean_df[clean_df$delay_mins < -100, c("expected_arr_time", "schedule_arr_time",
                                            "expected_arr_time_tt", "schedule_arr_time_tt", "delay_mins")])


# Problem found
# The huge negative values occurred because of:
# we dont have date in the schedule arrival time, so we get the date from expected time.
# Now if we pick the date from expected time, sometimes the bus came eralier like 11.50 PM on the same day
#   but its schedule time was 12.05 AM. So, this means the bus came 15 minutes earlier but in the previous day
#   So, in that case we need to pick the date before the date of expected to get the original date.


# Get indices where delay_mins is less than -100
indices <- which(clean_df$delay_mins < -100)

# Subtract one day from schedule_arr_time_tt for these indices
clean_df$schedule_arr_time_tt[indices] <- clean_df$schedule_arr_time_tt[indices] - 86400 # 86400 seconds in a day

# Now time to calculate the delay
clean_df$delay_mins <- as.numeric(difftime(clean_df$expected_arr_time_tt, 
                                           clean_df$schedule_arr_time_tt, units = "mins"))

# Visualize the delay
ggplot(clean_df, aes(y = delay_mins)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Bus Delays", x = "", y = "Delay Time (Mins)")


# calculate the weekend status
clean_df$weekend_status <- ifelse(weekdays(as.Date(clean_df$schedule_arr_time_tt)) 
                                  %in% c("Saturday", "Sunday"), TRUE, FALSE)

print(clean_df[1:10, c("expected_arr_time_tt", "schedule_arr_time_tt", "weekend_status")])

# calculate the day of the year
clean_df$day_of_year <- yday(clean_df$schedule_arr_time_tt)
print(clean_df[1:10, c("expected_arr_time_tt", "schedule_arr_time_tt", "day_of_year")])

plot(clean_df$day_of_year, clean_df$delay_mins, 
     main = "Scatter Plot of Delay by Day of Year",
     xlab = "Day of Year", 
     ylab = "Delay in Minutes",
     pch = 19,    # Type of point. 19 is solid circle
     col = "blue" # Color of points
)


# Extract hours and minutes and calculate total minutes
clean_df$time_of_day <- as.numeric(format(clean_df$schedule_arr_time_tt, 
                                   "%H")) * 60 + as.numeric(format(clean_df$schedule_arr_time_tt, "%M"))


print(clean_df[1:10, c("schedule_arr_time_tt", "day_of_year", "time_of_day")])


plot(clean_df$time_of_day, clean_df$delay_mins, 
     main = "Scatter Plot of Delay by Time of Day",
     xlab = "Time of Day", 
     ylab = "Delay in (Minutes)",
     pch = 19,    # Type of point. 19 is solid circle
     col = "red" # Color of points
)

names(clean_df)

# Here, is the list of columns in the dataset. We don't need the time based features as we have the derived features
#   out of them like delay_mins, day_of_year, weekend_status, and time_of_day. We can remove the time based features.

df <- subset(clean_df, select = -c(recorded_at, expected_arr_time, expected_arr_time_tt, 
                                   schedule_arr_time, schedule_arr_time_tt))


# set the predicted columns at the ends
# Store the column
delay_mins <- df$delay_mins

# Remove the column from its current position
df$delay_mins <- NULL

# Append it to the end of the dataframe
df <- cbind(df, delay_mins = delay_mins)

names(df)

write.csv(df, file = "projects/nyc_bus_delay_prediction/dataset/nyc_ds_eda.csv", row.names = FALSE)

