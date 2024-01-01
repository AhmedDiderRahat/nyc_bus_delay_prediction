#---------------------------------------------------------------------------------
# Title: Exploratory data analysis
# Author: Rahat, A.D
# Date: 2023-12-23
# Description: This scripts prepare all the ML models
#---------------------------------------------------------------------------------

# clear environment
rm(list = ls(all.names = TRUE))

# add the library
library(dplyr)
library(ggplot2)
library(lubridate)

root_rahat_dir <- "Desktop/ADR/ML-2/projects/nyc_bus_delay_prediction/"

# read data in csv file
train_df <- read.csv(paste(root_rahat_dir, "dataset/nyc_ds_train.csv", sep=""))

head(train_df)

# Target Encoding (Mean Encoding):

# Line name conversion 
line_df <- train_df %>%
  group_by(line_name) %>%
  summarise(mean_value = mean(non_neg_delay, na.rm = TRUE))


# origin name conversion 
origin_df <- train_df %>%
  group_by(org_name) %>%
  summarise(mean_value = mean(non_neg_delay, na.rm = TRUE))


# destination name conversion 
destination_df <- train_df %>%
  group_by(dest_name) %>%
  summarise(mean_value = mean(non_neg_delay, na.rm = TRUE))



# vehicle name conversion 
vehiclel_df <- train_df %>%
  group_by(vech_name) %>%
  summarise(mean_value = mean(non_neg_delay, na.rm = TRUE))


# directly copy the direction column
trainX <- data.frame(direction = train_df$direction)


# add the numeric values of line_name to the dataframe
trainX$line_name <- merge(train_df, line_df, by = "line_name")[, "mean_value"]


# add the numeric values of org_name to the dataframe
trainX$org_name <- merge(train_df, origin_df, by = "org_name")[, "mean_value"]


# add the numeric values of dest_name to the dataframe
trainX$dest_name <- merge(train_df, destination_df, by = "dest_name")[, "mean_value"]


# add the numeric values of vech_name to the dataframe
trainX$vech_name <- merge(train_df, vehiclel_df, by = "vech_name")[, "mean_value"]

# coppy other numeric columns

trainX[c("weekend_status", 
         "day_of_year",
         "time_of_day")] <- train_df[c("weekend_status",
                                         "day_of_year", 
                                         "time_of_day")]

head(trainX)


# directly copy the predicted column
trainY <- data.frame(non_neg_delay = train_df$non_neg_delay)


# Apply linear regression


# simple linear regression using all fetaures
lm1 <- lm(trainY$non_neg_delay ~ ., data = trainX)

summary(lm1)

# model summary:
# 1. Residual standard error: 8.244
# 2. here, there are org_name, and time_of_day are two variable which have significance greater than 5%



# read data in csv file
test_df <- read.csv(paste(root_rahat_dir, "dataset/nyc_ds_test.csv", sep=""))


# Target Encoding (Mean Encoding) for test data:

# directly copy the direction column
testX <- data.frame(direction = test_df$direction)

head(testX)

# Convert the line name values
mean_df_line <- mean(line_df$mean_value)
mean_df_line


# Example of creating a named vector for mapping
line_name_to_value <- setNames(line_df$mean_value, line_df$line_name)

# Create test_line as a vector
test_line <- sapply(test_df$line_name, function(name) {
  if (!(name %in% line_df$line_name)) {
    return(mean_df_line)
  } else {
    # Get the value from line_df corresponding to the name
    # Assuming you want some other value associated with this line name
    value_for_name <- line_df$mean_value[line_df$line_name == name]
    return(value_for_name)
  }
})

testX$line_name <- test_line

head(testX)


# Convert the origin name name
mean_df_org <- mean(origin_df$mean_value)


# Create org_name as a vector
test_origin <- sapply(test_df$org_name, function(name) {
  if (!(name %in% origin_df$org_name)) {
    return(mean_df_org)
  } else {
    value_for_name <- origin_df$mean_value[origin_df$org_name == name]
    return(value_for_name)
  }
})

testX$org_name <- test_origin


# Convert the destination name
mean_df_dest <- mean(destination_df$mean_value)


# Create org_name as a vector
test_dest <- sapply(test_df$dest_name, function(name) {
  if (!(name %in% destination_df$dest_name)) {
    return(mean_df_dest)
  } else {
    value_for_name <- destination_df$mean_value[destination_df$dest_name == name]
    return(value_for_name)
  }
})

testX$dest_name <- test_dest


# Convert the vehicle name
mean_df_vech <- mean(vehiclel_df$mean_value)


# Create org_name as a vector
test_vech <- sapply(test_df$vech_name, function(name) {
  if (!(name %in% vehiclel_df$vech_name)) {
    return(mean_df_vech)
  } else {
    value_for_name <- vehiclel_df$mean_value[vehiclel_df$vech_name == name]
    return(value_for_name)
  }
})

testX$vech_name <- test_dest


head(testX)


# add the numeric values of vech_name to the dataframe
trainX$vech_name <- merge(train_df, vehiclel_df, by = "vech_name")[, "mean_value"]

# coppy other numeric columns

trainX[c("weekend_status", 
         "day_of_year",
         "time_of_day")] <- train_df[c("weekend_status",
                                       "day_of_year", 
                                       "time_of_day")]

head(trainX)

# coppy other numeric columns

testX[c("weekend_status", 
         "day_of_year",
         "time_of_day")] <- test_df[c("weekend_status",
                                       "day_of_year", 
                                       "time_of_day")]



# directly copy the predicted column
testY <- data.frame(non_neg_delay = test_df$non_neg_delay)

########################## ------------------------------------- ##########################


set.seed(123) # Setting seed for reproducibility
cv_lasso <- cv.glmnet(x = as.matrix(trainX), y = trainY$non_neg_delay, alpha = 1)

optimal_lamda <- cv_lasso$lambda.min


lasso_model_optimal <- glmnet(as.matrix(trainX), trainY$non_neg_delay, alpha = 1, lambda = optimal_lamda)

coef(lasso_model_optimal)

predictions <- predict(lasso_model_optimal, newx = as.matrix(trainX), s = "lambda.min")

# calculate rmse
(rmse <- sqrt(mean((trainY$non_neg_delay - predictions)^2)))


test_predictions <- predict(lasso_model_optimal, newx = as.matrix(testX), s = "lambda.min")
# calculate rmse
(rmse <- sqrt(mean((test_df$non_neg_delay - test_predictions)^2)))
