# clear environment
rm(list = ls(all.names = TRUE))

# add the library
library(dplyr)
library(ggplot2)

# Load the dataset
df <- read.csv("dataset/nyc_traffic_sample.csv")

head(df)

# Calculate the percentage of nan in each columns
na_count <- colMeans(is.na(df)) * 100

# Print or inspect the percentage of N/A values in each column
print(na_count)

# only pick the percentage which has more than 0%
selected_columns <- na_count[na_count > 0]

# Convert 'column' to factor with levels based on existing order
plot_data$column <- factor(plot_data$column, levels = plot_data$column)

# Bar plot using ggplot without legend and with the original order
ggplot(plot_data, aes(x = column, y = percentage, fill = column)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Missing Values in Columns",
       y = "Percentage", x = "Columns") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5, position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Nan is any columns of the dataframe
total_rows_with_nulls <- sum(!complete.cases(df))

# Print the result
cat("Total number of rows with any null values:", total_rows_with_nulls, "\n")

