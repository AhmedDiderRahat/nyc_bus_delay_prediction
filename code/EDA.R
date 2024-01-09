#---------------------------------------------------------------------------------
# Title: Exploratory data analysis
# Author: Tania Sultana
# Date: 2023-12-23
# Description: This scripts prepare all the EDA of the project
#---------------------------------------------------------------------------------

# clear environment
rm(list = ls(all.names = TRUE))

# add the library
library(dplyr)
library(ggplot2)
library(lubridate)

setwd('D:/Msc/Winter 23-24/ML 2/Project/nyc_bus_delay_prediction/')

#root_rahat_dir <- "Desktop/ADR/ML-2/projects/nyc_bus_delay_prediction/"
root_tania_dir <- "D:/Msc/Winter 23-24/ML 2/Project/nyc_bus_delay_prediction/documentation/plots/eda"

# Load the dataset
df <- read.csv(paste("", "dataset/nyc_ds_eda.csv", sep=""))


## Uni-variate Analysis

#---------------------------------------------------------------------------------
## Direction column

(tab_dir <- table(df$direction))

# there are two values in the column: Outbound & Inbound directions for the bus route
# Outbound = 0 = the bus is moving away from center
# Inbound = 1 = the bus is moving inside towards center

# Convert to dataframe for plotting
tab_dir_df <- as.data.frame(tab_dir)

# Rename the columns
names(tab_dir_df) <- c("Direction", "Count")

# Rename the levels in the Direction column
levels(tab_dir_df$Direction) <- c("Outbound", "Inbound")
tab_dir_df

# Visualize the direction
# Create bar plot
ggplot(tab_dir_df, aes(x = Direction, y = Count, fill = Direction)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  labs(title = "Distribution of Directions", x = "Direction", y = "Count")


# Note: The dataset has a balance distribution among 2 different direction



#---------------------------------------------------------------------------------
## Line Name

# get the count of each line name
(tab_line_name <- table(df$line_name))

# Convert to dataframe for plotting
tab_line_df <- as.data.frame(tab_line_name)

head(tab_line_df)

# Rename the columns
names(tab_line_df) <- c("line_name", "count")

head(tab_line_df)

# sort using count
tab_line_df <- tab_line_df[order(-tab_line_df$count), ]

# Reset the row names
rownames(tab_line_df) <- NULL

cat("Total unique line name:", (nrow(tab_line_df)), "\n")

avg_line_count <- mean(tab_line_df$count)

cat("Average line counts:", avg_line_count, "\n")

# get top 20 linename
(top_line_df <- tab_line_df[1:20, ])

# visualize top-20 line name
ggplot(top_line_df, aes(x = reorder(line_name, -count), y = count, fill = line_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Line Names by Count", x = "Line Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for readability
        legend.position = "none")  # Remove the legend

# calculate the percentage of top-20 frequent line name
top_line_count_percentage <- sum(top_line_df$count) / sum(tab_line_df$count) * 100
cat("Top 20 line's frequency percentage:", top_line_count_percentage, "%\n")

# visualize the counts of each line with respect to this index
plot(tab_line_df$count,
     xlab = "Line Number", 
     ylab = "Count", 
     main = "Distribution of Counts")

# Add a horizontal green line at the mean count
abline(h = avg_line_count, col = "darkgreen", lwd = 2) # lwd = 2 makes the line a bit thicker

# Find the first index where count is less than the average
first_index <- which(tab_line_df$count < avg_line_count)[1]

# Add 'X' mark at the first such index on the average line
if (!is.na(first_index)) {
  points(first_index, avg_line_count, pch = "X", col = "darkorange", lwd = 3)
  
  # Add a dashed vertical line through the 'X' point
  abline(v = first_index, lty = "dashed", col = "darkred", lwd=2)
  
  # Add text label for the X-axis value
  text(first_index, par("usr")[3] - 1, labels = first_index, srt = 00, adj = 1, xpd = TRUE, col = "darkblue")
}

# Summary Statistics: 
## 1. Total unique line name is: 220
## 2. Average frequency of each line name: 7.58
## 3. Top 20 line's frequency percentage is 26.07914%
## 4. 88 line names have more counts than their average counts


#---------------------------------------------------------------------------------
## Origin/Source Name

# origin name table
tab_oname <- table(df$org_name)

# Convert to dataframe for plotting
df_origin_name <- as.data.frame(tab_oname)

head(df_origin_name)

# Rename the columns
names(df_origin_name) <- c("orogin_name", "count")

# sort using count
df_origin_name <- df_origin_name[order(-df_origin_name$count), ]

# Reset the row names
rownames(df_origin_name) <- NULL

# Get total unique origin name
cat("Total unique origin name:", (nrow(df_origin_name)), "\n")

# Get average count of each origin name
avg_origin_count <- mean(df_origin_name$count)
cat("Average line counts:", avg_origin_count, "\n")

# get top 20 origin name
(top_origin_df <- df_origin_name[1:20, ])

# visualize top-20 origin name
ggplot(top_origin_df, aes(x = reorder(orogin_name, -count), y = count, fill = orogin_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Orign/Start Station Names by Count", x = "Origin Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for readability
        legend.position = "none")  # Remove the legend


# calculate the percentage of top-20 frequent origin name
top_origin_count_percentage <- sum(top_origin_df$count) / sum(df_origin_name$count) * 100
cat("Top 20 line's frequency percentage:", top_origin_count_percentage, "%\n")


# visualize the counts of each origin with respect to their indices
plot(df_origin_name$count,
     xlab = "Origin Number", 
     ylab = "Count", 
     main = "Distribution of Counts")

# Add a horizontal green line at the mean count
abline(h = avg_origin_count, col = "darkgreen", lwd = 2)

# Find the first index where count is less than the average
first_index <- which(df_origin_name$count < avg_origin_count)[1]

# Add 'X' mark at the first such index on the average line
if (!is.na(first_index)) {
  points(first_index, avg_origin_count, pch = "X", col = "darkorange", lwd = 3)
  
  # Add a dashed vertical line through the 'X' point
  abline(v = first_index, lty = "dashed", col = "darkred", lwd=2)
  
  # Add text label for the X-axis value
  text(first_index, par("usr")[3] - 1, labels = first_index, srt = 00, adj = 1, xpd = TRUE, col = "darkblue")
}

# Summary Statistics: 
## 1. Total unique origin name is: 350
## 2. Average frequency of each origin name: 4.77
## 3. Top 20 origin's frequency percentage is 20.02398%
## 4. 136 origin names have more counts than their average counts



#---------------------------------------------------------------------------------
## Destination/End station Name

# destination name table
tab_dname <- table(df$dest_name)

# Convert to dataframe for plotting
df_dest_name <- as.data.frame(tab_dname)

head(df_dest_name)

# Rename the columns
names(df_dest_name) <- c("destination_name", "count")

# sort using count
df_dest_name <- df_dest_name[order(-df_dest_name$count), ]

# Reset the row names
rownames(df_dest_name) <- NULL

# Get total unique destination name
cat("Total unique destination name:", (nrow(df_dest_name)), "\n")

# Get average count of each destination name
avg_dest_count <- mean(df_dest_name$count)
cat("Average line counts:", avg_dest_count, "\n")

# get top 20 destination name
top_dest_df <- df_dest_name[1:20, ]

# visualize top-20 destination name
ggplot(top_dest_df, aes(x = reorder(destination_name, -count), y = count, fill = destination_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Destination/End Station Names by Count", x = "Destination Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),  # Rotate x labels for readability
        legend.position = "none")  # Remove the legend


# calculate the percentage of top-20 frequent destination name
top_dest_count_percentage <- sum(top_dest_df$count) / sum(df_dest_name$count) * 100
cat("Top 20 line's frequency percentage:", top_dest_count_percentage, "%\n")


# visualize the counts of each origin with respect to their indices
plot(df_dest_name$count,
     xlab = "Destination Number", 
     ylab = "Count", 
     main = "Distribution of Counts")

# Add a horizontal green line at the mean count
abline(h = avg_dest_count, col = "darkgreen", lwd = 2)

# Find the first index where count is less than the average
first_index <- which(df_dest_name$count < avg_dest_count)[1]

# Add 'X' mark at the first such index on the average line
if (!is.na(first_index)) {
  points(first_index, avg_dest_count, pch = "X", col = "darkorange", lwd = 3)
  
  # Add a dashed vertical line through the 'X' point
  abline(v = first_index, lty = "dashed", col = "darkred", lwd=2)
  
  # Add text label for the X-axis value
  text(first_index, par("usr")[3] - 1, labels = first_index, srt = 00, adj = 1, xpd = TRUE, col = "darkblue")
}

# Summary Statistics: 
## 1. Total unique destination name is: 438
## 2. Average frequency of each destination: 3.81
## 3. Top 20 destination's frequency percentage is 15.35%
## 4. 190 destinations have more counts than their average counts


#---------------------------------------------------------------------------------

## Map Visualization of each routes from source to destination

# install the package if needed
#install.packages("maps", dep = TRUE)

library(maps)

# init map object for NYC
nyc_map <- map_data("state", region = "new york")


# Aggregate the data to count the frequency of each route
df_aggregated <- df %>%
  group_by(org_long, org_lat, dest_long, dest_lat) %>%
  summarize(count = n())


# Determine the range of longitude and latitude for the routes area
# You may need to adjust these values based on your specific data
x_min <- min(df_aggregated$org_long, df_aggregated$dest_long) - 0.05
x_max <- max(df_aggregated$org_long, df_aggregated$dest_long) + 0.05
y_min <- min(df_aggregated$org_lat, df_aggregated$dest_lat) - 0.05
y_max <- max(df_aggregated$org_lat, df_aggregated$dest_lat) + 0.05

# Create a ggplot using the aggregated data
ggplot() +
  geom_polygon(data = nyc_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_segment(data = df_aggregated, aes(x = org_long, 
                                         y = org_lat, 
                                         xend = dest_long, 
                                         yend = dest_lat, 
                                         color = count), size = 2) +
  scale_color_gradient(low = "blue", high = "red", guide = "legend") +
  coord_fixed(1.3) +
  labs(x = "Longitude", y = "Latitude", title = "Routes in NYC and their counts") +
  xlim(x_min, x_max) +
  ylim(y_min, y_max)



#---------------------------------------------------------------------------------

## Vehicle Name

# vehicle name table
tab_vname <- table(df$vech_name)

# Convert to dataframe for plotting
df_vehicle_name <- as.data.frame(tab_vname)

head(df_vehicle_name)

# Rename the columns
names(df_vehicle_name) <- c("vehicle_name", "count")

# sort using count
df_vehicle_name <- df_vehicle_name[order(-df_vehicle_name$count), ]

# Reset the row names
rownames(df_vehicle_name) <- NULL

# Get total unique vehicle name
cat("Total unique vehicle name:", (nrow(df_vehicle_name)), "\n")

# Get average count of each vehicle name
avg_vehicle_count <- mean(df_vehicle_name$count)
cat("Average line counts:", avg_vehicle_count, "\n")

# get top 20 vehicle name
(top_vehicle_df <- df_vehicle_name[1:20, ])

# visualize top-20 vehicle name
ggplot(top_vehicle_df, aes(x = reorder(vehicle_name, -count), y = count, fill = vehicle_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Orign/Start Station Names by Count", x = "Origin Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels for readability
        legend.position = "none")  # Remove the legend


# calculate the percentage of top-20 frequent vehicle name
top_vehicle_count_percentage <- sum(top_vehicle_df$count) / sum(df_vehicle_name$count) * 100
cat("Top 20 vehicle's frequency percentage:", top_vehicle_count_percentage, "%\n")


# visualize the counts of each origin with respect to their indices
plot(df_vehicle_name$count,
     xlab = "Vehicle Number", 
     ylab = "Count", 
     main = "Distribution of Counts")

# Add a horizontal green line at the mean count
abline(h = avg_vehicle_count, col = "darkgreen", lwd = 2)

# Find the first index where count is less than the average
first_index <- which(df_vehicle_name$count < avg_vehicle_count)[1]

# Add 'X' mark at the first such index on the average line
if (!is.na(first_index)) {
  points(first_index, avg_vehicle_count, pch = "X", col = "darkorange", lwd = 3)
  
  # Add a dashed vertical line through the 'X' point
  abline(v = first_index, lty = "dashed", col = "darkred", lwd=2)
  
  # Calculate the y-position for the text label
  y_position <- par("usr")[3] - 0.15
  x_position = first_index + 20
  
  # Add text label for the X-axis value
  text(x_position, y_position, labels = first_index, srt = 00, adj = 1, xpd = TRUE, col = "darkblue")
}



# Summary Statistics: 
## 1. Total unique vehicle name: 1381 
## 2. Average frequency of each vehicle name: 1.21
## 3. Top 20 vehicle's frequency percentage: 3.78%
## 4. 251 vehicle names have more counts than their average counts


#---------------------------------------------------------------------------------

## next_point_name, arrivial_app, and dist_from_stop have no relevance to our analysis.

## Weekend Status


# weekend status table
tab_ws <- table(df$weekend_status)

# Convert to dataframe for plotting
df_ws <- as.data.frame(tab_ws)

head(df_ws)

# Rename the columns
names(df_ws) <- c("weekend_status", "count")

# sort using count
df_ws <- df_ws[order(-df_ws$count), ]

# Reset the row names
rownames(df_ws) <- NULL

# weekend status has two variable: FALSE = weekdays AND TRUE = weekend


# Calculate the percentage for each group
df_ws <- df_ws %>%
  group_by(weekend_status) %>%
  summarise(total_count = sum(count)) %>%
  mutate(percentage = (total_count / sum(total_count)) * 100)

# Create the bar plot with percentage labels
ggplot(df_ws, aes(x = reorder(weekend_status, -percentage), y = total_count, fill = weekend_status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.5, size = 4) +  # Add percentage labels
  labs(title = "Top 20 Origin/Start Station Names by Count and Percentage",
       x = "Origin Name",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),  # Reset X-axis text position to default
  legend.position = "none")  # Remove the legend

# Summary Statistics: 
# both type has pretty much balanced in the dataset. 
# weekdays has almost 80% counts and weekend has rest of 20%


#---------------------------------------------------------------------------------

## Day of the year

boxplot(df$day_of_year, 
        main = "Distribution of Day of the Year",
        ylab = "day of year") 

# Add the mean to the boxplot
points(1, mean(df$day_of_year), col = "red", pch = 19)
text(1.05, mean(df$day_of_year), labels = round(mean(df$day_of_year), 2), col = "red")

# Calculate quantiles
quantiles <- quantile(df$day_of_year, probs = c(0.25, 0.75))

# Add quantiles to the boxplot
segments(0.8, quantiles[1], 1.2, quantiles[1], col = "blue")
segments(0.8, quantiles[2], 1.2, quantiles[2], col = "blue")

# Annotate quantiles
text(1.25, quantiles[1], labels = round(quantiles[1], 2), col = "blue")
text(1.25, quantiles[2], labels = round(quantiles[2], 2), col = "blue")



#---------------------------------------------------------------------------------

## Time of the Day

boxplot(df$time_of_day, 
        main = "Distribution of time of the day",
        ylab = "day of year") 

# Add the mean to the boxplot
points(1, mean(df$time_of_day), col = "red", pch = 19)
text(1.09, mean(df$time_of_day), labels = paste(round(mean(df$time_of_day), 2), ' or ' , 
                                                round(mean(df$time_of_day) / 60, 0), ':', 
                                                round(mean(df$time_of_day) %% 60, 0), sep = ""), col = "red")

# Calculate quantiles
quantiles <- quantile(df$time_of_day, probs = c(0.25, 0.75))

# Add quantiles to the boxplot
segments(0.8, quantiles[1], 1.2, quantiles[1], col = "blue")
segments(0.8, quantiles[2], 1.2, quantiles[2], col = "blue")

# Annotate quantiles
text(1.28, quantiles[1], labels = paste(round(quantiles[1], 2), ' or ' , 
                                        round(quantiles[1] / 60, 0), ':', 
                                        round(quantiles[1] %% 60, 0), sep = ""), col = "blue")

text(1.28, quantiles[2], labels = paste(round(quantiles[2], 2), ' or ', 
                                        round(quantiles[2] / 60, 0), ':', 
                                        sprintf("%02d", as.integer(quantiles[2] %% 60)), sep = ""), col = "blue")

# Summary Statistics: 
## 1. Most busiest time is between 09:30 - 18:07
## 2. Mean time is 14:53.


#---------------------------------------------------------------------------------

## Delay mins

df$non_neg_delay <- ifelse(df$delay_mins < 0, 0, df$delay_mins)

boxplot(df$non_neg_delay, 
        main = "Distribution of the delay time (mins)",
        ylab = "delay (mins)") 


# Add the mean to the boxplot
points(1, mean(df$non_neg_delay), col = "red", pch = 19)
text(1.04, mean(df$non_neg_delay), labels = paste(round(mean(df$non_neg_delay), 2),  sep = ""), col = "red")

# determined minimum and maximum values
#min_value <- min(df$non_neg_delay, na.rm = TRUE)
max_value <- max(df$non_neg_delay, na.rm = TRUE)

# Add the minimum point
#points(1, min_value, col = "blue", pch = 19)
#text(1.05, min_value, labels = round(min_value, 2), col = "blue")

# Add the maximum point
points(1, max_value, col = "darkgreen", pch = 19)
text(1.05, max_value, labels = round(max_value, 2), col = "darkgreen")

quantiles <- quantile(df$non_neg_delay, probs = c(0.25, 0.75))

# Annotate quantiles
text(1.24, quantiles[1], labels = round(quantiles[1], 2), col = "blue")

text(1.24, quantiles[2], labels = round(quantiles[2], 2), col = "darkgreen")


# Summary Statistics: 
## 1. The delay time is distributed from -26.92 to 71.95
## 2. Mean delay time is 4.99.
## 3. 50% of the data is concentrated in to -0.27 minutes to 7.92 minutes.
## There are several outlair present in the data.

# For non-negative time delays:
## 1. The delay time is distributed from 0 to 71.95
## 2. Mean delay time is 5.71.
## 3. 50% of the data is concentrated in to 0 minutes to 7.92 minutes.
## There are several outlair present in the data.


#---------------------------------------------------------------------------------

## Bi-variate Analysis

#---------------------------------------------------------------------------------

## Direction column vs delay time
boxplot(non_neg_delay ~  direction, data = df, family='binomial')


#---------------------------------------------------------------------------------

## Line Name column vs delay time

df_nnd_line <- df %>%
  group_by(line_name) %>%
  summarise(
    count = n(),
    mean_delay = mean(non_neg_delay, na.rm = TRUE)) %>% 
  arrange(desc(mean_delay))

head(df_nnd_line)

# Calculate the average count
average_count <- mean(df_nnd_line$count)

# Filter out lines with count less than the average count
df_nnd_line_aa <- df_nnd_line %>%
  filter(count >= average_count)

cat("There are a total number of ", nrow(df_nnd_line_aa), 
    " obersvation which has frequncy above to the avg. frequency", sep = "")

# Set the factor levels of line_name in the order of the dataframe
df_nnd_line_aa$line_name <- factor(df_nnd_line_aa$line_name, levels = df_nnd_line_aa$line_name)

# Identify the maximum and minimum mean_delay
max_delay <- df_nnd_line_aa[which.max(df_nnd_line_aa$mean_delay), ]
min_delay <- df_nnd_line_aa[which.min(df_nnd_line_aa$mean_delay), ]


ggplot(df_nnd_line_aa, aes(x = line_name, y = mean_delay, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(data = max_delay, aes(label = round(mean_delay, 2), y = mean_delay + 0.5), hjust= 0.15, vjust = 1) +
  geom_text(data = min_delay, aes(label = round(mean_delay, 2), y = mean_delay + 0.5), vjust = 1) +
  labs(x = "Line Name", y = "Mean Delay", fill = "Count") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Filter out lines with count greater than the average count
df_nnd_line_ba <- df_nnd_line %>%
  filter(count < average_count)

cat("There are a total number of ", nrow(df_nnd_line_ba), 
    " obersvation which has frequncy above to the avg. frequency", sep = "")

# Set the factor levels of line_name in the order of the dataframe
df_nnd_line_ba$line_name <- factor(df_nnd_line_ba$line_name, levels = df_nnd_line_ba$line_name)


# Identify the maximum and minimum mean_delay
max_delay <- df_nnd_line_ba[which.max(df_nnd_line_ba$mean_delay), ]
min_delay <- df_nnd_line_ba[which.min(df_nnd_line_ba$mean_delay), ]

# Assuming df_nnd_line is already created as per your description
ggplot(df_nnd_line_ba, aes(x = line_name, y = mean_delay, fill = count)) +
  geom_bar(stat = "identity") +
  geom_text(data = max_delay, aes(label = round(mean_delay, 2), y = mean_delay + 1.0), hjust= 0.15, vjust = 1) +
  geom_text(data = min_delay, aes(label = round(mean_delay, 2), y = mean_delay + 1.0), vjust = 1) +
  labs(x = "Line Name", y = "Mean Delay", fill = "Count") +
  scale_fill_gradient(low = "blue", high = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Summary Statistics: 
## 1. Frequent bus lines have less variation in the average delay time (11.96 - 1.1).
## 2. Where the less frequent bus that have less frequency than the average frequency 
####  have high variation in their average delay time (43.52 - 0).

#---------------------------------------------------------------------------------

## Weekend column vs delay time
boxplot(non_neg_delay ~  weekend_status, data = df, family='binomial')


## day of the year column vs delay time
boxplot(non_neg_delay ~  day_of_year, data = df, family='binomial')


## time of the day column vs delay time

df$hour_of_day <- floor(df$time_of_day / 60)

boxplot(non_neg_delay ~  hour_of_day, data = df, family='binomial')


boxplot(non_neg_delay ~  arrivial_app, data = df, family='binomial')


# Final Discussion to pick the features:
## 1. direction
## 2. line_name
## 3. org_name
## 4. dest_name
# Couldn't pick the latitude and longitude of both start and and station as we are picking their name
## 5. vech_name
# vehicle latitude and longitude also not necessary in our task
# also not considering the arrival approximation related features
## 6. weekend_status
## 7. day_of_year
## 8. time_of_day

df$org_lat <- NULL
df$org_long <- NULL
df$dest_lat <- NULL
df$dest_long <- NULL
df$vech_lat <- NULL
df$vech_long <- NULL
df$next_point_name <- NULL
df$arrivial_app <- NULL
df$dist_from_stop <- NULL
df$delay_mins <- NULL
df$hour_of_day <- NULL

names(df)

# train-test partition (80/20)

# Set seed for reproducibility
set.seed(123)

# Calculate the size for the 80% split
split_size <- floor(0.8 * nrow(df))

# Sample rows for the first dataframe (80%)
sampled_indices <- sample(seq_len(nrow(df)), size = split_size)

# Create the two dataframes
train_df <- df[sampled_indices, ]  # 80% of the data
test_df <- df[-sampled_indices, ] # Remaining 20% of the data

# save the final dataset
write.csv(train_df, file = paste(root_rahat_dir, "dataset/nyc_ds_train.csv", sep=""), row.names = FALSE)
write.csv(test_df, file = paste(root_rahat_dir, "dataset/nyc_ds_test.csv", sep=""), row.names = FALSE)
