# add the library
library(dplyr)
library(purrr)

# set the directory
setwd("/Users/ahmed/Desktop/ADR/ML-2/projects/nyc_bus_delay_prediction/")

# Read the 4 original data
directory_path <- "dataset/original_data/"

df <- list.files(directory_path, pattern = "\\.csv$", 
                 full.names = TRUE) %>% map_df(read.csv)


cat("Total Number of rows in original dataset:", nrow(df), "\n")

# sample set size
N = 2000

# Set a random seed for reproducibility
set.seed(786)

# Randomly pickup N rows
sample <- df %>% sample_n(N, replace = FALSE)

head(sample)

nrow(sample)

write.csv(sample, file = "dataset/nyc_traffic_sample.csv", row.names = FALSE)
