# R package loading
library(readr)
library(tidyverse)
library(janitor)
library(ggpubr)
library(openxlsx)
options(warn = -1)
# Data Reading
df <- read_csv('df_add_order.csv')
df <- clean_names(df)
# Convert the collection_date column to the date format
df$collection_date <- as.Date(df$collection_date, format = "%m/%d/%Y")
# Filter data with a sampling date of January 1, 2022 or more
df <- subset(df,collection_date >= as.Date("2022-01-01"))
df <- subset(df, collection_date <= as.Date("2025-04-30"))
# Extract the year and month based on collection_date
df$date <- format(df$collection_date, "%Y-%m")
# Get the unique combination of order and bird_species
unique_combinations <- df %>%
  distinct(order, bird_species) %>%
  arrange(order, bird_species)
write.xlsx(unique_combinations,'unique_com.xlsx')





