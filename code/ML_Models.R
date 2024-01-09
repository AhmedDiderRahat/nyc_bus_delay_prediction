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
library(glmnet)


root_rahat_dir <- "Desktop/ADR/ML-2/projects/nyc_bus_delay_prediction/"

# read data in csv file
train_df <- read.csv(paste("", "dataset/nyc_ds_train.csv", sep=""))

head(train_df)

# names of all columns
names(train_df)

str(train_df)

# unique columns of each column
unique_counts <- sapply(train_df, function(x) length(unique(x)))

as.data.frame((unique_counts))

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


# read data in csv file
test_df <- read.csv(paste("", "dataset/nyc_ds_test.csv", sep=""))


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

# copy other numeric columns
testX[c("weekend_status", 
         "day_of_year",
         "time_of_day")] <- test_df[c("weekend_status",
                                       "day_of_year", 
                                       "time_of_day")]

head(testX)


# directly copy the predicted column
testY <- data.frame(non_neg_delay = test_df$non_neg_delay)



########################## ML-MODEL ##########################

# Lasso Regression:

# Fit Lasso model
set.seed(123)
lasso_model <- glmnet(as.matrix(trainX), trainY$non_neg_delay, alpha=1)

predictY_lasso <- predict(lasso_model, as.matrix(trainX))
train_rmse_lasso <- sqrt(mean((predictY_lasso - trainY$non_neg_delay)^2))

cat("Accuracy of the Lasso Model in train data:", train_rmse_lasso, "\n")

# calculate RMSE for test set
predictY_lasso <- predict(lasso_model, as.matrix(testX))
test_rmse_lasso <- sqrt(mean((predictY_lasso - testY$non_neg_delay)^2))

cat("Accuracy of the Lasso Model in test data:", test_rmse_lasso, "\n")

# calculate the avg. delay of train and test data
mean_delay_train <- mean(trainY$non_neg_delay)
mean_delay_test <- mean(testY$non_neg_delay)

var_delay_train <- var(trainY$non_neg_delay)
var_delay_test <- var(testY$non_neg_delay)

cat("Average Delay of train:", mean_delay_train, "\n")
cat("Average Delay of test:", mean_delay_test, "\n")

cat("Variance of the delay in train data:", var_delay_train, "\n")
cat("Variance of the delay in test data:", var_delay_test, "\n")

# Set up the plotting area to have 1 row and 2 columns
par(mfrow = c(1, 2))

# Create the first boxplot for the training data
boxplot(trainY$non_neg_delay, main = "Training Data", ylab = "Non-Neg Delay", col = "blue")

# Create the second boxplot for the test data
boxplot(testY$non_neg_delay, main = "Test Data", ylab = "Non-Neg Delay", col = "red")

# Reset the plotting parameters to default (optional, but good practice)
par(mfrow = c(1, 1))

percentage_calculation <- function(variables) {
  
  data <- variables
  
  # Calculate quartiles and IQR
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  IQR <- Q3 - Q1
  
  # Identify outliers
  outliers <- data < (Q1 - 1.5 * IQR) | data > (Q3 + 1.5 * IQR)
  
  # Calculate the percentage of outliers
  percentage_outliers <- sum(outliers) / length(data) * 100
  
  return(percentage_outliers)
}

cat("Percentage of outliers for train data: ", percentage_calculation(trainY$non_neg_delay), "%\n")
cat("Percentage of outliers for test data: ", percentage_calculation(testY$non_neg_delay), "%\n")


# Hyper Parameter turning of Lasso Model

# Setting seed for reproducibility
set.seed(123)
cv_lasso <- cv.glmnet(x = as.matrix(trainX), y = trainY$non_neg_delay, alpha = 1)

(optimal_lamda <- cv_lasso$lambda.min)

summary(cv_lasso$lambda.min)

if(!is.null(cv_lasso$foldid)) {
  num_folds <- length(unique(cv_lasso$foldid))
  print(num_folds)
} else {
  print("Default 10 folds used")
}


lasso_model_optimal <- glmnet(as.matrix(trainX), trainY$non_neg_delay, alpha = 1, lambda = optimal_lamda)

# Extract coefficients
coefficients <- coef(lasso_model_optimal, s = optimal_lamda)
print(coefficients)

# Deviance explained
deviance_explained <- lasso_model_optimal$dev.ratio
print(deviance_explained)

# Counting the number of non-zero coefficients
non_zero_coefs <- sum(coefficients != 0)
print(non_zero_coefs)

# Lasso result analysis 
predictY_optimal_lasso <- predict(lasso_model_optimal, newx = as.matrix(trainX), s = "lambda.min")

# calculate rmse
train_optimal_rmse <- sqrt(mean((trainY$non_neg_delay - predictY_optimal_lasso)^2))
cat("Accuracy of the Optimal Lasso Model in train data:", train_optimal_rmse, "\n")


# for test data
predictY_optimal_lasso <- predict(lasso_model_optimal, newx = as.matrix(testX), s = "lambda.min")
# calculate rmse
test_optimal_rmse <- sqrt(mean((test_df$non_neg_delay - predictY_optimal_lasso)^2))
cat("Accuracy of the Optimal Lasso Model in test data:", test_optimal_rmse, "\n")




### Generative additive model (GAM):

# install packgae
# install.packages("mgcv")


library("mgcv")

# combine the trainX and trainY data together. 
train_data <- cbind(trainX, non_neg_delay = trainY$non_neg_delay)


# fit a general GAM model with smoothing spine
gam_ss <- gam(non_neg_delay ~ 
                s(log(time_of_day), by = factor(weekend_status), k = 5) + 
                s(org_name, by=factor(direction), k = 5) + 
                s(dest_name, by=factor(direction), k = 5) + 
                s(vech_name, k = 5) + 
                s(line_name, k = 5) + 
                s(day_of_year, k = 5),
              data = train_data,
              method = "REML")

summary(gam_ss)


# fit a GAM model with s(), te(), ti()
gam_te <- gam(non_neg_delay ~ 
                ti(log(time_of_day), by = factor(weekend_status), k = 5) + 
                te(org_name, dest_name, by = factor(direction), k = c(5, 5)) + 
                te(vech_name, line_name, k = c(5, 5)) +  
                s(day_of_year, k = 5),
              data = train_data,
              method = "REML")


summary(gam_te)

# compare two model
anova(gam_te, gam_ss)

plot(gam_ss)


# calculate the RMSE of model
rmse_of_gam <- function(model, dataX, dataY){
  predictY <- predict(model, newdata=dataX)
  
  rmse <- sqrt(mean((dataY - predictY)^2))
  
  return(rmse)
}

cat("Accuracy of the Smooting Spine on train data:", rmse_of_gam(gam_ss, trainX, trainY$non_neg_delay), "\n")
cat("Accuracy of the Smooting Spine on test data:", rmse_of_gam(gam_ss, testX, testY$non_neg_delay), "\n")



# hyper parameter tuning
# install.packages("caret", dep = TRUE)
library(caret)

calculate_rmse <- function(data, k) {
  # Fit the GAM model
  model <- gam(non_neg_delay ~ 
                 s(log(time_of_day), by = factor(weekend_status), k = k) + 
                 s(org_name, by=factor(direction), k = k) + 
                 s(dest_name, by=factor(direction), k = k) + 
                 s(vech_name, k = k) + 
                 s(line_name, k = k) + 
                 s(day_of_year, k = k) + 
                 weekend_status + direction, 
               data = data,
               method = "REML")
  
  # Predictions and RMSE calculation
  predictions <- predict(model, newdata = data)
  rmse <- sqrt(mean((data$non_neg_delay - predictions)^2))
  return(rmse)
}


k_values <- c(5, 6, 7, 8, 9, 10, 11, 12)

set.seed(123)
folds <- createFolds(train_data$non_neg_delay, k = 10)
rmse_results <- matrix(NA, nrow = length(k_values), ncol = length(folds))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  for (j in seq_along(folds)) {
    fold <- folds[[j]]
    data_train <- train_data[-fold, ]
    rmse_results[i, j] <- calculate_rmse(data_train, k)
  }
}

average_rmse <- rowMeans(rmse_results, na.rm = TRUE)

# Assuming k_values and average_rmse are your vectors of data
plot(k_values,
     average_rmse, 
     type = "b", 
     pch = 19, 
     col = "blue", 
     xlab = "K Values", 
     ylab = "Average RMSE", 
     main = "Plot of K Values vs. Average RMSE", 
     sub = "Green point indicates the lowest RMSE")

# Identify the index of the lowest RMSE
min_rmse_index <- which.min(average_rmse)

# Add a green point at the lowest RMSE
points(k_values[min_rmse_index], 
       average_rmse[min_rmse_index], 
       pch = 19, 
       col = "darkgreen")


# Label the lowest RMSE value
text(k_values[min_rmse_index], 
     average_rmse[min_rmse_index], 
     labels = paste(round(average_rmse[min_rmse_index], 5)), 
     pos = 3, 
     cex = 1, 
     col = "darkgreen")


# Identify the index of the highest RMSE
max_rmse_index <- which.max(average_rmse)

# Add a red point at the highest RMSE
points(k_values[max_rmse_index], 
       average_rmse[max_rmse_index], 
       pch = 19, 
       col = "red")


# Label the lowest RMSE value
text(k_values[max_rmse_index], 
     average_rmse[max_rmse_index], 
     labels = paste(round(average_rmse[max_rmse_index], 5)), 
     pos = 1, 
     cex = 1, 
     col = "darkred")



# fit with best parameter of K
(best_k_index <- which.min(average_rmse))
(best_k <- k_values[best_k_index])

final_model <- gam(non_neg_delay ~ 
      s(log(time_of_day), by = factor(weekend_status), k = best_k) + 
      s(org_name, by=factor(direction), k = best_k) + 
      s(dest_name, by=factor(direction), k = best_k) + 
      s(vech_name, k = best_k) + 
      s(line_name, k = best_k) + 
      s(day_of_year, k = best_k) + 
      weekend_status + direction, 
    data = train_data,
    method = "REML")

summary(final_model)


# RMSE calculation of final
cat("Accuracy of the final GAM on train data:", rmse_of_gam(final_model, trainX, trainY$non_neg_delay), "\n")
cat("Accuracy of the final GAM on test data:", rmse_of_gam(final_model, testX, testY$non_neg_delay), "\n")


