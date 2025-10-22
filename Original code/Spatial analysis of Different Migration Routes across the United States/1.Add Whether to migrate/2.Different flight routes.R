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
# Read migration data
df_mig <- read.xlsx('unique_com.xlsx')
# Perform left join: match the migratory column from df_mig to df
df_with_migratory <- df %>%
  # Use left join to keep all rows from df (left table)
  left_join(
    # Right table, select only the key for matching and the column to add
    df_mig %>% select(order, bird_species, magritory),
    # Specify the common keys for matching
    by = c("order", "bird_species")
  )
# write.xlsx(df_with_migratory,'df_with_migratory.xlsx')


