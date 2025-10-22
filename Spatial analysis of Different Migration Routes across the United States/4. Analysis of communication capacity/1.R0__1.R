# R package loading
library(readr)
library(tidyverse)
library(ggpubr)
library(openxlsx)
library(patchwork)
library(cowplot)
library(R0)
options(warn = -1)
# Read flight route data
flyway <- read.xlsx('wild_bird_with_Migration.xlsx',detectDates = T)
# Filter data with a sampling date of January 1, 2022 or more
flyway <- subset(flyway,collection_date >= as.Date("2022-01-01"))
flyway <- subset(flyway, collection_date <= as.Date("2025-04-30"))
# Add a new column of year and week numbers
flyway <- flyway %>%
  mutate(year = year(collection_date),
         week = week(collection_date)) %>% 
  arrange(year, week) %>% 
  mutate(
    year_week = paste0(
      year, 
      "-", 
      ifelse(week < 10, paste0("0", week), week)
    )
  )
# Define all possible dimensions
all_flyways <- c("Pacific", "Central", "Mississippi", "Atlantic") 
all_magritory_states <- c("YES", "NO") 
# Group by three dimensions: year_week, Flyway, and magritory
df_summary_detailed <- flyway %>% 
  group_by(year_week, Flyway, magritory) %>% 
  summarise(record_count = n(), .groups = 'drop')
# Create a complete skeleton containing all combinations of year_week, Flyway, and magritory
full_skeleton_detailed <- expand_grid(
  year_week = unique(flyway$year_week),
  Flyway = all_flyways,
  # Add magritory dimension
  magritory = all_magritory_states 
)
# Merge statistical results and fill NA with 0
df_summary_fixed_range_detailed <- full_skeleton_detailed %>%
  left_join(
    df_summary_detailed, 
    # Matching keys include magritory
    by = c("year_week", "Flyway", "magritory") 
  ) %>%
  # Fill NA values from left_join (i.e., weeks with no records) with 0
  mutate(record_count = replace_na(record_count, 0))

######### Filter Pacific route data ##########################
############# Overall
Pacific_df <- subset(df_summary_fixed_range_detailed,Flyway == 'Pacific')
# Merge by year-week
Pacific_df_total <- Pacific_df %>% 
  group_by(year_week) %>% 
  summarise(total = sum(record_count))
# Copy data frame for manipulation
df_r0 <- Pacific_df_total
# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 
# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$total[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}
# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0 <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  )
# Remove NA rows where lookup_count cannot be calculated
df_r0_clean <- na.omit(df_r0)
# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)
# Estimate R0 and its confidence interval
result <- est.R0.EG(
  na.omit(df_r0_clean$total), 
  GT = gtn
)
print(result)

###################################### Migratory birds
Pacific_df_yes <- subset(df_summary_fixed_range_detailed,Flyway == 'Pacific' & magritory == 'YES')
Pacific_df_yes <- Pacific_df_yes %>% 
  select(year_week,record_count)

# Use Pacific_df_yes data
df_r0 <- Pacific_df_yes
# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)
# Estimate R0 and its confidence interval (using exponential growth method)
result_yes <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_yes)

###################################### Non-migratory birds
Pacific_df_no <- subset(df_summary_fixed_range_detailed,Flyway == 'Pacific' & magritory == 'NO')
Pacific_df_no <- Pacific_df_no %>% 
  select(year_week,record_count)

# Use Pacific_df_no data
df_r0 <- Pacific_df_no

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval
result_no <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_no)

######### Filter Central route data ##########################
############# Overall
Central_df <- subset(df_summary_fixed_range_detailed,Flyway == 'Central')
# Merge by year-week
Central_df_total <- Central_df %>% 
  group_by(year_week) %>% 
  summarise(total = sum(record_count))

# Use Central_df_total data
df_r0 <- Central_df_total

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$total[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)
# Estimate R0 and its confidence interval (using exponential growth method)
result_central <- est.R0.EG(
  na.omit(df_r0_clean$total), 
  GT = gtn
)
print(result_central)

###################################### Migratory birds
Central_df_yes <- subset(df_summary_fixed_range_detailed,Flyway == 'Central' & magritory == 'YES')
Central_df_yes <- Central_df_yes %>% 
  select(year_week,record_count)

# Use Central_df_yes data
df_r0 <- Central_df_yes

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  
  # Inner loop: starting from current week (i), find the time when cases appear in the next week
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 # Accumulate interval weeks
    }
    
    # Once cases are found (sum_count > 0), record the required interval weeks and exit inner loop
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  # Remove NA rows where lookup_count cannot be calculated
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval (using exponential growth method)
result_central_yes <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_central_yes)

###################################### Non-migratory birds
Central_df_no <- subset(df_summary_fixed_range_detailed,Flyway == 'Central' & magritory == 'NO')
Central_df_no <- Central_df_no %>% 
  select(year_week,record_count)

# Use Central_df_no data
df_r0 <- Central_df_no

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 # Accumulate interval weeks
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  # Remove NA rows where lookup_count cannot be calculated
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)
# Estimate R0 and its confidence interval (using exponential growth method)
result_central_no <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_central_no)

######### Filter Mississippi route data ##########################
############# Overall
Mississippi_df <- subset(df_summary_fixed_range_detailed,Flyway == 'Mississippi')
# Merge by year-week
Mississippi_df_total <- Mississippi_df %>% 
  group_by(year_week) %>% 
  summarise(total = sum(record_count))

# Use Mississippi_df_total data
df_r0 <- Mississippi_df_total

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$total[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)
# Estimate R0 and its confidence interval
result_mississippi_total <- est.R0.EG(
  na.omit(df_r0_clean$total), 
  GT = gtn
)
print(result_mississippi_total)

###################################### Migratory birds
Mississippi_df_yes <- subset(df_summary_fixed_range_detailed,Flyway == 'Mississippi' & magritory == 'YES')
Mississippi_df_yes <- Mississippi_df_yes %>% 
  select(year_week,record_count)

# Use Mississippi_df_yes data
df_r0 <- Mississippi_df_yes

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval
result_mississippi_yes <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_mississippi_yes)

###################################### Non-migratory birds
Mississippi_df_no <- subset(df_summary_fixed_range_detailed,Flyway == 'Mississippi' & magritory == 'NO')
Mississippi_df_no <- Mississippi_df_no %>% 
  select(year_week,record_count)

# Use Mississippi_df_no data
df_r0 <- Mississippi_df_no

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval
result_mississippi_no <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_mississippi_no)

######### Filter Atlantic route data ##########################
############# Overall
Atlantic_df <- subset(df_summary_fixed_range_detailed,Flyway == 'Atlantic')
# Merge by year-week
Atlantic_df_total <- Atlantic_df %>% 
  group_by(year_week) %>% 
  summarise(total = sum(record_count))
# Use Atlantic_df_total data
df_r0 <- Atlantic_df_total

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$total[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval (using exponential growth method)
result_atlantic_total <- est.R0.EG(
  na.omit(df_r0_clean$total), 
  GT = gtn
)
print(result_atlantic_total)

###################################### Migratory birds
Atlantic_df_yes <- subset(df_summary_fixed_range_detailed,Flyway == 'Atlantic' & magritory == 'YES')
Atlantic_df_yes <- Atlantic_df_yes %>% 
  select(year_week,record_count)

# Use Atlantic_df_yes data
df_r0 <- Atlantic_df_yes

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution (GT Distribution)
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval (using exponential growth method)
result_atlantic_yes <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_atlantic_yes)

###################################### Non-migratory birds
Atlantic_df_no <- subset(df_summary_fixed_range_detailed,Flyway == 'Atlantic' & magritory == 'NO')
Atlantic_df_no <- Atlantic_df_no %>% 
  select(year_week,record_count)

# Use Atlantic_df_no data
df_r0 <- Atlantic_df_no

# Initialize lookup_count column
df_r0 <- df_r0 %>%
  mutate(lookup_count = NA_real_) 

# Use for loop to calculate generation time (GT)
for (i in 1:nrow(df_r0)) {
  sum_count <- 0
  rows_needed <- 0
  for (j in i:nrow(df_r0)) {
    sum_count <- sum_count + df_r0$record_count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1 
    }
    if (sum_count > 0) {
      df_r0$lookup_count[i] <- rows_needed
      break
    }
  }
}

# Convert interval weeks to actual generation time (Generation Time), i.e., + 1 
df_r0_clean <- df_r0 %>%
  mutate(
    lookup_count = lookup_count + 1
  ) %>%
  na.omit() 

# Estimate generation time distribution
gtn <- est.GT(serial.interval = df_r0_clean$lookup_count)

# Estimate R0 and its confidence interval
result_atlantic_no <- est.R0.EG(
  na.omit(df_r0_clean$record_count), 
  GT = gtn
)
print(result_atlantic_no)


