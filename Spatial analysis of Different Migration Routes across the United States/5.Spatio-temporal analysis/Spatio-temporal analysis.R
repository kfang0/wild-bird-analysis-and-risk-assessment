# R package loading
library(readr)
library(tidyverse)
library(ggpubr)
library(openxlsx)
library(patchwork)
library(cowplot)
library(usmap)
library(sf)
library(spdep)
library(tidyrgeoda)
options(warn = -1)
# Read flight route data
flyway <- read.xlsx('wild_bird_with_Migration.xlsx',detectDates = T)
# Filter data with a sampling date of January 1, 2022 or more
flyway <- subset(flyway,collection_date >= as.Date("2022-01-01"))
flyway <- subset(flyway, collection_date <= as.Date("2025-04-30"))
flyway <- flyway %>%
  arrange(date)
flyway <- flyway %>%
  mutate(
    date_ym = paste0(date, "-01"),
    date_object = ymd(date_ym),
    month = month(date_object) 
  ) %>%
  mutate(
    season = case_when(
      # Spring: March, April, May
      month %in% c(3, 4, 5) ~ "Spring",
      # Summer: June, July, August
      month %in% c(6, 7, 8) ~ "Summer",
      # Autumn: September, October, November
      month %in% c(9, 10, 11) ~ "Autumn",
      # Winter: December, January, February
      month %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ "Unknown" 
    )
  ) %>%
  select(-date_ym, -date_object, -month)
##########################################
### US-wide Moran's Index calculation
df_us <- flyway %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)
# Calculate LISA map
lisa01 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  labs(title = 'LISA:All in US')+
  theme_bw() +
  theme(legend.position = 'left',    
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>%
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
data_combined[52,] <- NA
data_combined$cluster_type[52] <- 'Hot Spot - 99% Confidence'
# Plotting code
gi01 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "Gi_Bin",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  labs(title = 'Gi_Bin:All in US')+
  theme_bw() +
  theme(
    text = element_text(family = 'serif'),
    legend.position = 'right',
    plot.title = element_text(size = 9, family = 'serif')
  )

## US-wide Spring Moran's Index
df_us_spring <- subset(flyway,season == 'Spring')
df_us_spring <- df_us_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa02 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  labs(title = 'LISA:All in Spring')+
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi02 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )


## US-wide Summer Moran's Index
df_us_summer <- subset(flyway,season == 'Summer')
df_us_summer <- df_us_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa03 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi03 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

## US-wide Autumn Moran's Index
df_us_autumn <- subset(flyway,season == 'Autumn')
df_us_autumn <- df_us_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa04 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi04 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )


## US-wide Winter Moran's Index
df_us_winter <- subset(flyway,season == 'Winter')
df_us_winter <- df_us_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa05 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi05 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide migratory birds Moran's Index
df_us_magritory <- subset(flyway,magritory == 'YES')
df_us_magritory <- df_us_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa06 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in US')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi06 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Migratory in US')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide migratory birds Spring Moran's Index
df_us_magritory_spring <- subset(flyway,magritory == 'YES' & season == 'Spring')
df_us_magritory_spring <- df_us_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa07 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi07 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide migratory birds Summer Moran's Index
df_us_magritory_summer <- subset(flyway,magritory == 'YES' & season == 'Summer')
df_us_magritory_summer <- df_us_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa08 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi08 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide migratory birds Autumn Moran's Index
df_us_magritory_autumn <- subset(flyway,magritory == 'YES' & season == 'Autumn')
df_us_magritory_autumn <- df_us_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa09 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi09 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide migratory birds Winter Moran's Index
df_us_magritory_winter <- subset(flyway,magritory == 'YES' & season == 'Winter')
df_us_magritory_winter <- df_us_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa10 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all,2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi10 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide non-migratory birds Moran's Index
df_us_non_magritory <- subset(flyway,magritory == 'NO')
df_us_non_magritory <- df_us_non_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_non_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa11 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  labs(title = 'LISA:Non-Migratory in US')+
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi11 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  labs(title = 'Gi_Bin:Non-Migratory in US')+
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
### Calculate US-wide non-migratory birds Spring Moran's Index
df_us_non_magritory_spring <- subset(flyway,magritory == 'NO' & season == 'Spring')
df_us_non_magritory_spring <- df_us_non_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_non_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa12 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi12 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )


### Calculate US-wide non-migratory birds Summer Moran's Index
df_us_non_magritory_summer <- subset(flyway,magritory == 'NO' & season == 'Summer')
df_us_non_magritory_summer <- df_us_non_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_non_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa13 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi13 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide non-migratory birds Autumn Moran's Index
df_us_non_magritory_autumn <- subset(flyway,magritory == 'NO' & season == 'Autumn')
df_us_non_magritory_autumn <- df_us_non_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_non_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa14 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi14 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

### Calculate US-wide non-migratory birds Winter Moran's Index
df_us_non_magritory_winter <- subset(flyway,magritory == 'NO' & season == 'Winter')
df_us_non_magritory_winter <- df_us_non_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Merge Data
data_all <- merge(us_states,df_us_non_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa15 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi15 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gi_Bin:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 9, family = 'serif')
  )

################### Combine graphs
# US-wide graph combination
lisa01 + lisa02 + lisa03 + lisa04 + lisa05 + 
  lisa06 + lisa07 + lisa08 + lisa09 + lisa10 +
  lisa11 + lisa12 + lisa13 + lisa14 + lisa15+
  gi01 + gi02 + gi03 + gi04 + gi05 + 
  gi06 + gi07 + gi08 + gi09 + gi10 +
  gi11 + gi12 + gi13 + gi14 + gi15+
  plot_layout( ncol = 5, guides = "collect")

# ggsave('us.pdf',width = 15,height = 12,dpi = 600)

#######################################################
############## Pacific route
Pacific_road <- c(
  "Washington", "Oregon", "Idaho", "Montana", "California",
  "Nevada", "Utah", "Arizona", "Alaska", "Hawaii"
)
## Filter data for this route
df_pacific <- subset(flyway,Flyway == 'Pacific')
df_pacific <- df_pacific %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa_pacific <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) 
lisa_pacific[11:13,] <- NA
lisa_pacific$lisa[11:13] <- c("High-High", "Low-Low","High-Low")
lisa16 <- lisa_pacific %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Pacific Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all,1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all,1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all,1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
data_combined[11:15,] <- NA
data_combined$cluster_type[11:15]  <- c("Hot Spot - 99% Confidence","Hot Spot - 95% Confidence",
                                        "Hot Spot - 90% Confidence","Cold Spot - 99% Confidence",
                                        "Cold Spot - 95% Confidence","Cold Spot - 90% Confidence")
# Graph plotting
gi16 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "Gi_Bin",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Pacific Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route Spring Moran's Index
## Filter data for this route
df_pacific_spring <- subset(flyway,Flyway == 'Pacific' & season == 'Spring')
df_pacific_spring <- df_pacific_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa17 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi17 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route Summer Moran's Index
## Filter data for this route
df_pacific_summer <- subset(flyway,Flyway == 'Pacific' & season == 'Summer')
df_pacific_summer <- df_pacific_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa18 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi18 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route Autumn Moran's Index
## Filter data for this route
df_pacific_autumn <- subset(flyway,Flyway == 'Pacific' & season == 'Autumn')
df_pacific_autumn <- df_pacific_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa19 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi19 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route Winter Moran's Index
## Filter data for this route
df_pacific_winter <- subset(flyway,Flyway == 'Pacific' & season == 'Winter')
df_pacific_winter <- df_pacific_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa20 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi20 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route migratory birds
## Filter data for this route
df_pacific_magritory <- subset(flyway,Flyway == 'Pacific' & magritory == 'YES')
df_pacific_magritory <- df_pacific_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa21 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Pacific Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi21 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Pacific Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route migratory birds Spring Moran's Index
## Filter data for this route
df_pacific_magritory_spring <- subset(flyway,Flyway == 'Pacific' & magritory == 'YES' & season == 'Spring' )
df_pacific_magritory_spring <- df_pacific_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa22 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi22 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route migratory birds Summer Moran's Index
## Filter data for this route
df_pacific_magritory_summer <- subset(flyway,Flyway == 'Pacific' & magritory == 'YES' & season == 'Summer' )
df_pacific_magritory_summer <- df_pacific_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa23 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi23 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route migratory birds Autumn Moran's Index
## Filter data for this route
df_pacific_magritory_autumn <- subset(flyway,Flyway == 'Pacific' & magritory == 'YES' & season == 'Autumn' )
df_pacific_magritory_autumn <- df_pacific_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa24 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi24 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route migratory birds Winter Moran's Index
## Filter data for this route
df_pacific_magritory_winter <- subset(flyway,Flyway == 'Pacific' & magritory == 'YES' & season == 'Winter' )
df_pacific_magritory_winter <- df_pacific_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa25 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi25 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route non-migratory birds
## Filter data for this route
df_pacific_non_magritory <- subset(flyway,Flyway == 'Pacific' & magritory == 'NO')
df_pacific_non_magritory <- df_pacific_non_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_non_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa26 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Pacific Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi26 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Pacific Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route non-migratory birds Spring Moran's Index
## Filter data for this route
df_pacific_non_magritory_spring <- subset(flyway,Flyway == 'Pacific' & magritory == 'NO' & season == 'Spring')
df_pacific_non_magritory_spring <- df_pacific_non_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_non_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa27 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi27 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Pacific route non-migratory birds Summer Moran's Index
## Filter data for this route
df_pacific_non_magritory_summer <- subset(flyway,Flyway == 'Pacific' & magritory == 'NO' & season == 'Summer')
df_pacific_non_magritory_summer <- df_pacific_non_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_non_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa28 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi28 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
############## Pacific route non-migratory birds Autumn Moran's Index
## Filter data for this route
df_pacific_non_magritory_autumn <- subset(flyway,Flyway == 'Pacific' & magritory == 'NO' & season == 'Autumn')
df_pacific_non_magritory_autumn <- df_pacific_non_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_non_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa29 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi29 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
############## Pacific route non-migratory birds Winter Moran's Index
## Filter data for this route
df_pacific_non_magritory_winter <- subset(flyway,Flyway == 'Pacific' & magritory == 'NO' & season == 'Winter')
df_pacific_non_magritory_winter <- df_pacific_non_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Pacific_road)
# Merge Data
data_all <- merge(us_states,df_pacific_non_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa30 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi30 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

################### Merge graphs
# Pacific Road graph merging
lisa16 + lisa17 + lisa18 + lisa19 + lisa20 + 
  lisa21 + lisa22 + lisa23 + lisa24 + lisa25 +
  lisa26 + lisa27 + lisa28 + lisa29 + lisa30+
  plot_layout( ncol = 5,guides = 'collect')

# ggsave('pacific_LISA.pdf',width = 15,height = 12,dpi = 600)

gi16 + gi17 + gi18 + gi19 + gi20 + 
  gi21 + gi22 + gi23 + gi24 + gi25 +
  gi26 + gi27 + gi28 + gi29 + gi30+
  plot_layout( ncol = 5,guides = 'collect')

# ggsave('pacific_Gen_Bi.pdf',width = 15,height = 12,dpi = 600)

#######################################################
############## Central route
Central_road <- c(
  "Wyoming", "Colorado", "New Mexico", "Texas", "Oklahoma",
  "Kansas", "Nebraska", "South Dakota", "North Dakota"
)
## Filter data for this route
df_central <- subset(flyway,Flyway == 'Central')
df_central <- df_central %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa_central <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) 
lisa_central[10:12,] <- NA
lisa_central$lisa[10:12] <- c("High-High","Low-Low", "Low-High")
lisa31 <- lisa_central %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Central Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
data_combined[10:13,] <- NA
data_combined$cluster_type[10:13] <- c("Hot Spot - 99% Confidence","Hot Spot - 95% Confidence",
                                       "Cold Spot - 99% Confidence", "Cold Spot - 90% Confidence")
# Graph plotting
gi31 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "Gen_Bi",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Central Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route Spring Moran's Index
## Filter data for this route
df_central_spring <- subset(flyway,Flyway == 'Central' & season == 'Spring')
df_central_spring <- df_central_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa32 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi32 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route Summer Moran's Index
## Filter data for this route
df_central_summer <- subset(flyway,Flyway == 'Central' & season == 'Summer')
df_central_summer <- df_central_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa33 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi33 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route Autumn Moran's Index
## Filter data for this route
df_central_autumn <- subset(flyway,Flyway == 'Central' & season == 'Autumn')
df_central_autumn <- df_central_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa34 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi34 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route Winter Moran's Index
## Filter data for this route
df_central_winter <- subset(flyway,Flyway == 'Central' & season == 'Winter')
df_central_winter <- df_central_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa35 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi35 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )


############## Central route migratory birds
## Filter data for this route
df_central_magritory <- subset(flyway,Flyway == 'Central' & magritory == 'YES')
df_central_magritory <- df_central_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa36 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Central Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi36 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Central Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route migratory birds Spring Moran's Index
## Filter data for this route
df_central_magritory_spring <- subset(flyway,Flyway == 'Central' & magritory == 'YES' & season == 'Spring')
df_central_magritory_spring <- df_central_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa37 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi37 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route migratory birds Summer Moran's Index
## Filter data for this route
df_central_magritory_summer <- subset(flyway,Flyway == 'Central' & magritory == 'YES' & season == 'Summer')
df_central_magritory_summer <- df_central_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)


# Calculate LISA map
lisa38 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi38 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route migratory birds Autumn Moran's Index
## Filter data for this route
df_central_magritory_autumn <- subset(flyway,Flyway == 'Central' & magritory == 'YES' & season == 'Autumn')
df_central_magritory_autumn <- df_central_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa39 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi39 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route migratory birds Winter Moran's Index
## Filter data for this route
df_central_magritory_winter <- subset(flyway,Flyway == 'Central' & magritory == 'YES' & season == 'Winter')
df_central_magritory_winter <- df_central_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa40 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi40 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route non-migratory birds
## Filter data for this route
df_central_non_magritory <- subset(flyway,Flyway == 'Central' & magritory == 'NO')
df_central_non_magritory <- df_central_non_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_non_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa41 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Central Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi41 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Central Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route non-migratory birds Spring Moran's Index
## Filter data for this route
df_central_non_magritory_spring <- subset(flyway,Flyway == 'Central' & magritory == 'NO' & season == 'Spring')
df_central_non_magritory_spring <- df_central_non_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_non_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa42 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi42 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route non-migratory birds Summer Moran's Index
## Filter data for this route
df_central_non_magritory_summer <- subset(flyway,Flyway == 'Central' & magritory == 'NO' & season == 'Summer')
df_central_non_magritory_summer <- df_central_non_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_non_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <- k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k ))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa43 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi43 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
############## Central route non-migratory birds Autumn Moran's Index
## Filter data for this route
df_central_non_magritory_autumn <- subset(flyway,Flyway == 'Central' & magritory == 'NO' & season == 'Autumn')
df_central_non_magritory_autumn <- df_central_non_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_non_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa44 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi44 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Central route non-migratory birds Winter Moran's Index
## Filter data for this route
df_central_non_magritory_winter <- subset(flyway,Flyway == 'Central' & magritory == 'NO' & season == 'Winter')
df_central_non_magritory_winter <- df_central_non_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Central_road)
# Merge Data
data_all <- merge(us_states,df_central_non_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa45 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi45 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

################### Combine graphs
# Central Road graph combination
lisa31 + lisa32 + lisa33 + lisa34 + lisa35 + 
  lisa36 + lisa37 + lisa38 + lisa39 + lisa40 +
  lisa41 + lisa42 + lisa43 + lisa44 + lisa45+
  plot_layout( ncol = 5,guides = 'collect')

# ggsave('Central_LISA.pdf',width = 15,height = 12,dpi = 600)

gi31 + gi32 + gi33 + gi34 + gi35 + 
  gi36 + gi37 + gi38 + gi39 + gi40 +
  gi41 + gi42 + gi43 + gi44 + gi45+
  plot_layout( ncol = 5,guides = 'collect')

# ggsave('Central_Gen_Bi.pdf',width = 15,height = 12,dpi = 600)

#######################################################
############## Mississippi route
Mississippi_road <- c(
  "Minnesota", "Wisconsin", "Michigan", "Iowa", "Missouri",
  "Arkansas", "Louisiana", "Mississippi", "Alabama", "Tennessee",
  "Kentucky", "Ohio", "Indiana", "Illinois"
)
## Filter data for this route
df_mississippi <- subset(flyway,Flyway == 'Mississippi')
df_mississippi <- df_mississippi %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa_Mississippi <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) 

lisa_Mississippi[15:16,] <- NA
lisa_Mississippi$lisa[15:16] <- c("High-Low","Low-High")

lisa46 <- lisa_Mississippi %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Mississippi Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
data_combined[15:17,] <- NA
data_combined$cluster_type[15:17] <- c("Hot Spot - 95% Confidence","Hot Spot - 90% Confidence",
                                       "Cold Spot - 90% Confidence")
# Graph plotting
gi46 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "Gen_Bi",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Mississippi Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route Spring Moran's Index
## Filter data for this route
df_mississippi_spring <- subset(flyway,Flyway == 'Mississippi' & season == 'Spring')
df_mississippi_spring <- df_mississippi_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa47 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi47 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route Summer Moran's Index
## Filter data for this route
df_mississippi_summer <- subset(flyway,Flyway == 'Mississippi' & season == 'Summer')
df_mississippi_summer <- df_mississippi_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa48 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi48 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route Autumn Moran's Index
## Filter data for this route
df_mississippi_autumn <- subset(flyway,Flyway == 'Mississippi' & season == 'Autumn')
df_mississippi_autumn <- df_mississippi_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa49 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi49 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route Winter Moran's Index
## Filter data for this route
df_mississippi_winter <- subset(flyway,Flyway == 'Mississippi' & season == 'Winter')
df_mississippi_winter <- df_mississippi_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa50 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi50 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route migratory birds
## Filter data for this route
df_mississippi_magritory <- subset(flyway,Flyway == 'Mississippi' & magritory == 'YES')
df_mississippi_magritory <- df_mississippi_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa51 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Mississippi Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi51 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Mississippi Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route migratory birds Spring Moran's Index
## Filter data for this route
df_mississippi_magritory_spring <- subset(flyway,Flyway == 'Mississippi' & magritory == 'YES' & season == 'Spring')
df_mississippi_magritory_spring <- df_mississippi_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa52 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi52 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route migratory birds Summer Moran's Index
## Filter data for this route
df_mississippi_magritory_summer <- subset(flyway,Flyway == 'Mississippi' & magritory == 'YES' & season == 'Summer')
df_mississippi_magritory_summer <- df_mississippi_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa53 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi53 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route migratory birds Autumn Moran's Index
## Filter data for this route
df_mississippi_magritory_autumn <- subset(flyway,Flyway == 'Mississippi' & magritory == 'YES' & season == 'Autumn')
df_mississippi_magritory_autumn <- df_mississippi_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa54 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,4),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 4),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi54 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route migratory birds Winter Moran's Index
## Filter data for this route
df_mississippi_magritory_winter <- subset(flyway,Flyway == 'Mississippi' & magritory == 'YES' & season == 'Winter')
df_mississippi_magritory_winter <- df_mississippi_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa55 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi55 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route non-migratory birds
## Filter data for this route
df_mississippi_non_magritory <- subset(flyway,Flyway == 'Mississippi' & magritory == 'NO')
df_mississippi_non_magritory <- df_mississippi_non_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_non_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa56 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Mississippi Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi56 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in \n Mississippi Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route non-migratory birds Spring Moran's Index
## Filter data for this route 
df_mississippi_non_magritory_spring <- subset(flyway,Flyway == 'Mississippi' & magritory == 'NO' & season == 'Spring')
df_mississippi_non_magritory_spring <- df_mississippi_non_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_non_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa57 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi57 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route non-migratory birds Summer Moran's Index
## Filter data for this route 
df_mississippi_non_magritory_summer <- subset(flyway,Flyway == 'Mississippi' & magritory == 'NO' & season == 'Summer')
df_mississippi_non_magritory_summer <- df_mississippi_non_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_non_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa58 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi58 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route non-migratory birds Autumn Moran's Index
## Filter data for this route 
df_mississippi_non_magritory_autumn <- subset(flyway,Flyway == 'Mississippi' & magritory == 'NO' & season == 'Autumn')
df_mississippi_non_magritory_autumn <- df_mississippi_non_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_non_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa59 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi59 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Mississippi route non-migratory birds Winter Moran's Index
## Filter data for this route 
df_mississippi_non_magritory_winter <- subset(flyway,Flyway == 'Mississippi' & magritory == 'NO' & season == 'Winter')
df_mississippi_non_magritory_winter <- df_mississippi_non_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Mississippi_road)
# Merge Data
data_all <- merge(us_states,df_mississippi_non_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa60 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi60 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

################### Combine graphs
# Mississippi Road graph combination
lisa46 + lisa47 + lisa48 + lisa49 + lisa50 + 
  lisa51 + lisa52 + lisa53 + lisa54 + lisa55 +
  lisa56 + lisa57 + lisa58 + lisa59 + lisa60+
  plot_layout(ncol = 5,guides = 'collect')

# ggsave('Mississippi_LISA.pdf',width = 15,height = 12,dpi = 600)

gi46 + gi47 + gi48 + gi49 + gi50 + 
  gi51 + gi52 + gi53 + gi54 + gi55 +
  gi56 + gi57 + gi58 + gi59 + gi60+
  plot_layout(ncol = 5,guides = 'collect')

# ggsave('Mississippi_Gen_Bi.pdf',width = 15,height = 12,dpi = 600)

#######################################################
############## Atlantic route
Atlantic_road <- c(
  "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut",
  "New Jersey", "Delaware", "Maryland", "West Virginia", "Maine",
  "New York", "Pennsylvania", "Virginia", "North Carolina", "South Carolina",
  "Georgia", "Florida", "District of Columbia"
)
## Filter data for this route
df_atlantic <- subset(flyway,Flyway == 'Atlantic')
df_atlantic <- df_atlantic %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa_Atlantic <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) 
lisa_Atlantic[19,] <- NA
lisa_Atlantic$lisa[19] <- c("High-High")  

lisa61 <- lisa_Atlantic %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Atlantic Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
data_combined[19:20,] <- NA
data_combined$cluster_type[19:20] <- c("Hot Spot - 99% Confidence",
                                       "Hot Spot - 90% Confidence")
# Graph plotting
gi61 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "Gen_Bi",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Atlantic Road')+
  theme(legend.position = 'right',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route Spring Moran's Index
## Filter data for this route
df_atlantic_spring <- subset(flyway,Flyway == 'Atlantic' & season == 'Spring')
df_atlantic_spring <- df_atlantic_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa62 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi62 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route Summer Moran's Index
## Filter data for this route
df_atlantic_summer <- subset(flyway,Flyway == 'Atlantic' & season == 'Summer')
df_atlantic_summer <- df_atlantic_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa63 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi63 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route Autumn Moran's Index
## Filter data for this route
df_atlantic_autumn <- subset(flyway,Flyway == 'Atlantic' & season == 'Autumn')
df_atlantic_autumn <- df_atlantic_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa64 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi64 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route Winter Moran's Index
## Filter data for this route
df_atlantic_winter <- subset(flyway,Flyway == 'Atlantic' & season == 'Winter')
df_atlantic_winter <- df_atlantic_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa65 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi65 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:All in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route migratory birds
## Filter data for this route
df_atlantic_magritory <- subset(flyway,Flyway == 'Atlantic' & magritory == 'YES')
df_atlantic_magritory <- df_atlantic_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa66 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,3),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Atlantic Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 3),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi66 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in \n Atlantic Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )


############## Atlantic route migratory birds Spring Moran's Index
## Filter data for this route
df_atlantic_magritory_spring <- subset(flyway,Flyway == 'Atlantic' & magritory == 'YES' & season == 'Spring')
df_atlantic_magritory_spring <- df_atlantic_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa67 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi67 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route migratory birds Summer Moran's Index
## Filter data for this route
df_atlantic_magritory_summer <- subset(flyway,Flyway == 'Atlantic' & magritory == 'YES' & season == 'Summer')
df_atlantic_magritory_summer <- df_atlantic_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa68 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi68 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route migratory birds Autumn Moran's Index
## Filter data for this route
df_atlantic_magritory_autumn <- subset(flyway,Flyway == 'Atlantic' & magritory == 'YES' & season == 'Autumn')
df_atlantic_magritory_autumn <- df_atlantic_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa69 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi69 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
############## Atlantic route migratory birds Winter Moran's Index
## Filter data for this route
df_atlantic_magritory_winter <- subset(flyway,Flyway == 'Atlantic' & magritory == 'YES' & season == 'Winter')
df_atlantic_magritory_winter <- df_atlantic_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa70 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi70 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Migratory in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route non-migratory birds
## Filter data for this route
df_atlantic_non_magritory <- subset(flyway,Flyway == 'Atlantic' & magritory == 'NO')
df_atlantic_non_magritory <- df_atlantic_non_magritory %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_non_magritory, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa71 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory in \n Atlantic Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi71 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory in \n Atlantic Road')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route non-migratory birds Spring Moran's Index
## Filter data for this route
df_atlantic_non_magritory_spring <- subset(flyway,Flyway == 'Atlantic' & magritory == 'NO' & season == 'Spring')
df_atlantic_non_magritory_spring <- df_atlantic_non_magritory_spring %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_non_magritory_spring, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa72 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,5),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory \n in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 5),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi72 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory \n in Spring')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route non-migratory birds Summer Moran's Index
## Filter data for this route
df_atlantic_non_magritory_summer <- subset(flyway,Flyway == 'Atlantic' & magritory == 'NO' & season == 'Summer')
df_atlantic_non_magritory_summer <- df_atlantic_non_magritory_summer %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_non_magritory_summer, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa73 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,1),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory \n in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 1),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi73 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory \n in Summer')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route non-migratory birds Autumn Moran's Index
## Filter data for this route
df_atlantic_non_magritory_autumn <- subset(flyway,Flyway == 'Atlantic' & magritory == 'NO' & season == 'Autumn')
df_atlantic_non_magritory_autumn <- df_atlantic_non_magritory_autumn %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_non_magritory_autumn, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa74 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory \n in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi74 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory \n in Autumn')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

############## Atlantic route non-migratory birds Winter Moran's Index
## Filter data for this route
df_atlantic_non_magritory_winter <- subset(flyway,Flyway == 'Atlantic' & magritory == 'NO' & season == 'Winter')
df_atlantic_non_magritory_winter <- df_atlantic_non_magritory_winter %>% 
  group_by(state) %>% 
  summarise(record_count = n())
# Load US states map data
us_states <- us_map(regions = "states")
# Keep only data for this route
us_states <- subset(us_states,full %in% Atlantic_road)
# Merge Data
data_all <- merge(us_states,df_atlantic_non_magritory_winter, by.x = "full", by.y = "state", all.x = TRUE)
data_all$record_count[is.na(data_all$record_count)] <- 0
# KNN requires coordinates. We must first get the centroid (center point) for each state.
coords <- st_coordinates(st_centroid(usmap_transform(data_all)))
k_values <- 1:5
moran_results <- numeric(length(k_values))
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_nb <- knn2nb(knearneigh(coords, k = k))
  knn_lw <- nb2listw(knn_nb, style = "W")
  moran_i <- moran.test(data_all$record_count, knn_lw)$estimate[1]
  moran_results[i] <- moran_i
}
optimal_k <-  k_values[which.max(abs(moran_results))]
cat("Optimal k is:", optimal_k, "\n") 
# Build the adjacency list using the optimal k
knn_nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
# Convert the adjacency list to a weight matrix
lw_final_knn <- nb2listw(knn_nb_final, style = "W")
# Calculation of the Moran Index
moran_test_knn <- moran.test(data_all$record_count, lw_final_knn)
print(moran_test_knn)

# Calculate LISA map
lisa75 <- data_all %>% 
  mutate(lisa = st_local_moran(.,'record_count',
                               wt = tidyrgeoda::st_knn_weights(data_all,2),
                               significance_cutoff = 0.05)) %>% 
  select(lisa) %>% 
  ggplot() +
  geom_sf(aes(fill = lisa), lwd = .1, color = 'grey') +
  scale_fill_manual(
    values = c(
      "High-High" = "#E41A1C",  
      "Low-Low" = "#377EB8",     
      "High-Low" = "#984EA3",    
      "Low-High" = "#FF7F00",    
      "Not significant" = "#F7F7F7" 
    ),
    name = "LISA type" 
  ) +
  theme_bw() +
  labs(title = 'LISA:Non-Migratory \n in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )
# Calculate hot spots and cold spots
data_combined <- data_all %>%
  mutate(
    lg_01 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.01),
    lg_05 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.05),
    lg_10 = st_local_g(., 'record_count',
                       wt = tidyrgeoda::st_knn_weights(data_all, 2),
                       significance_cutoff = 0.1)
  ) %>% 
  mutate(
    cluster_type = case_when(
      # Hot spot classification - by significance priority
      lg_01 == "High-High" ~ "Hot Spot - 99% Confidence",
      lg_05 == "High-High" ~ "Hot Spot - 95% Confidence",
      lg_10 == "High-High" ~ "Hot Spot - 90% Confidence",
      # Cold spot classification - by significance priority
      lg_01 == "Low-Low" ~ "Cold Spot - 99% Confidence",
      lg_05 == "Low-Low" ~ "Cold Spot - 95% Confidence",
      lg_10 == "Low-Low" ~ "Cold Spot - 90% Confidence",
      # Not significant
      TRUE ~ "Not Significant"
    )
  )

# Ensure all classifications exist as factor levels
data_combined <- data_combined %>%
  mutate(
    cluster_type = factor(cluster_type, 
                          levels = c("Cold Spot - 99% Confidence", 
                                     "Cold Spot - 95% Confidence", 
                                     "Cold Spot - 90% Confidence",
                                     "Not Significant",
                                     "Hot Spot - 90% Confidence",
                                     "Hot Spot - 95% Confidence", 
                                     "Hot Spot - 99% Confidence"))
  )
# Graph plotting
gi75 <- ggplot(data_combined) +
  geom_sf(aes(fill = cluster_type), color = 'grey', lwd = 0.1) +
  scale_fill_manual(
    name = "",
    values = c("Hot Spot - 99% Confidence" = "#D62F27",    
               "Hot Spot - 95% Confidence" = "#ED7551",    
               "Hot Spot - 90% Confidence" = "#FAB984",    
               "Cold Spot - 99% Confidence" = "#000080",   
               "Cold Spot - 95% Confidence" = "#4169E1",   
               "Cold Spot - 90% Confidence" = "#87CEFA",   
               "Not Significant" = "#F7F7F2"),             
    breaks = c("Cold Spot - 99% Confidence", 
               "Cold Spot - 95% Confidence", 
               "Cold Spot - 90% Confidence",
               "Not Significant",
               "Hot Spot - 90% Confidence",
               "Hot Spot - 95% Confidence", 
               "Hot Spot - 99% Confidence")
  ) +
  theme_bw() +
  labs(title = 'Gen_Bi:Non-Migratory \n in Winter')+
  theme(legend.position = 'none',
        text = element_text(family = 'serif'),
        plot.title = element_text(size = 11, family = 'serif')
  )

################### Combine graphs
# Atlantic Road graph combination
lisa61 + lisa62 + lisa63 + lisa64 + lisa65 + 
  lisa66 + lisa67 + lisa68 + lisa69 + lisa70 +
  lisa71 + lisa72 + lisa73 + lisa74 + lisa75+
  plot_layout(ncol = 5,guides = 'collect')

# ggsave('Atlantic_LISA.pdf',width = 15,height = 12,dpi = 600)

gi61 + gi62 + gi63 + gi64 + gi65 + 
  gi66 + gi67 + gi68 + gi69 + gi70 +
  gi71 + gi72 + gi73 + gi74 + gi75+
  plot_layout(ncol = 5,guides = 'collect')

# ggsave('Atlantic_Gen_Bi.pdf',width = 15,height = 12,dpi = 600)

