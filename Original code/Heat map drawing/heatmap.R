# Load the R package
library(readr)
library(tidyverse)
library(janitor)
library(ggsci)
library(RColorBrewer)
library(usmap)
library(patchwork)
# Data Reading
df <- read_csv('hpai-wild-birds.csv')
df <- clean_names(df)
# Convert the collection_date column to the date format
df$collection_date <- as.Date(df$collection_date, format = "%m/%d/%Y")
# Filter data with a sampling date of January 1, 2022 or more
df <- subset(df,collection_date >= as.Date("2022-01-01"))
df <- subset(df, collection_date <= as.Date("2025-04-30"))
# Extract the year and month based on collection_date
df$date <- format(df$collection_date, "%Y")

# Statistics of Record Counts by Year, Month, and State
df_summary <- df %>%
  group_by(date,state) %>%
  summarise(record_count = n()) 
# Summarize the total record count for each state
df_all <- df_summary %>%
  group_by(state) %>% 
  summarise(record_count = sum(record_count,na.rm = T))

# Plotting the Heatmap in total
p1 <- plot_usmap(data = df_all, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- Total")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

####################################### Plotting the Heatmap by Year #######################################
df_2022 <- subset(df_summary, date == "2022")
p2 <- plot_usmap(data = df_2022, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- 2022")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

df_2023 <- subset(df_summary, date == "2023")
p3 <- plot_usmap(data = df_2023, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- 2023")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

df_2024 <- subset(df_summary, date == "2024")
p4 <- plot_usmap(data = df_2024, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- 2024")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white") 

df_2025 <- subset(df_summary, date == "2025")
p5 <- plot_usmap(data = df_2025, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- 2025")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

############################################# Plotting the Heatmap by Season #############################################

# Data Reading
df <- read_csv('hpai-wild-birds.csv')
df <- clean_names(df)
# Convert the collection_date column to the date format
df$collection_date <- as.Date(df$collection_date, format = "%m/%d/%Y")
# Filter data with a sampling date of January 1, 2022 or more
df <- subset(df,collection_date >= as.Date("2022-01-01"))
df <- subset(df, collection_date <= as.Date("2025-04-30"))
# Extract the year and month based on collection_date
df$date <- format(df$collection_date, "%Y-%m")

# Count the number of records
df_summary <- df %>%
  group_by(date,state) %>% 
  summarise(record_count = n()) 

# Filter the spring data and draw the graph
df_spring <- df %>%
  filter(month(collection_date) %in% c(3, 4, 5)) %>% 
  group_by(date,state) %>%
  summarise(record_count = n())
df_spring1 <- df_spring %>%
  group_by(state) %>% 
  summarise(record_count = sum(record_count,na.rm = T))
p6 <- plot_usmap(data = df_spring1, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- Spring")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

# Filter the summer data and draw the graph
df_summer <- df %>%
  filter(month(collection_date) %in% c(6,7,8)) %>% 
  group_by(date,state) %>% 
  summarise(record_count = n())
df_summer1 <- df_summer  %>%
  group_by(state) %>% 
  summarise(record_count = sum(record_count,na.rm = T))
p7 <- plot_usmap(data = df_summer1, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- Summer")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

# Filter the autumn data and draw the graph
df_autumn <- df %>%
  filter(month(collection_date) %in% c(9,10,11)) %>% 
  group_by(date,state) %>%
  summarise(record_count = n())

df_autumn1 <- df_autumn  %>%
  group_by(state) %>% 
  summarise(record_count = sum(record_count,na.rm = T))
p8 <- plot_usmap(data = df_autumn1, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- Autumn")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

# Filter the winter data and draw the graph
df_winter <- df %>%
  filter(month(collection_date) %in% c(12,1,2)) %>% 
  group_by(date,state) %>% 
  summarise(record_count = n())
df_winter1 <- df_winter  %>%
  group_by(state) %>% 
  summarise(record_count = sum(record_count,na.rm = T))
p9 <- plot_usmap(data = df_winter1, values = "record_count", regions = "states")+
  theme_bw()+
  labs(fill = "Wild bird (Number of cases) -- Winter")+
  theme(legend.position = 'bottom')+
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = unit(5, "cm") 
    )
  )+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "Spectral")), na.value = "white")

# Combine all plots into a single layout
p1+p2+p3+p4+p5+p6+p7+p8+p9+plot_layout(ncol = 3, nrow = 3)+
  theme(plot.title = element_text(hjust = 0.5, size = 20))
