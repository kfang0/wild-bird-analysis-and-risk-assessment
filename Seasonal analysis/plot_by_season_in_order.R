# R package loading
library(readr)
library(tidyverse)
library(janitor)
library(ggsci)
library(pcutils)
library(cowplot)
library(ggpubr)
library(dunn.test)
library(tidyplots)
library(forecast)
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
# Convert the date column to a factor type
df$order <- ifelse(df$order == "Accipitriformes","Accipitriformes",
                           ifelse(df$order == "Anseriformes","Anseriformes",
                                  ifelse(df$order == "Strigiformes","Strigiformes",
                                         ifelse(df$order == "Passeriformes","Passeriformes",
                                                ifelse(df$order == "Charadriiformes","Charadriiformes","Others")))))
# Count the number of records by date and order
df_summary <- df %>%
  group_by(date,order) %>% 
  summarise(record_count = n()) %>%
  ungroup() %>%  
  complete(date, order, fill = list(record_count = 0))

# Count the number of records by date
df_summary_total <- df %>%
  group_by(date) %>%  
  summarise(record_count = n()) 

############################ Draw a bar chart in total ##########################
p1 <- ggplot(df_summary_total, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCBE7A",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),method = 'gam',se = F) +
  labs(
    title = 'A',
    x = "Year-Month",         
    y = "Number of cases"             
  ) +
  scale_x_discrete(breaks = df_summary_total$date[seq(1, nrow(df_summary_total), by = 2)]) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,1100)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )


######################################## Draw a bar chart by order #######################
df_summary_Anseriformes <- df_summary %>%
  filter(order == "Anseriformes")
p2 <- ggplot(df_summary_Anseriformes, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCAE59",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),method = 'gam',se = F) +
  scale_x_discrete(breaks = df_summary_Anseriformes$date[seq(1, nrow(df_summary_Anseriformes), by = 2)]) +
  labs(
    title = 'B',
    x = "Year-Month",         
    y = "Number of cases"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,1000)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

df_summary_Accipitriformes <- df_summary %>%
  filter(order == "Accipitriformes")
p3 <- ggplot(df_summary_Accipitriformes, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCBE7A",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),se = F,method = 'loess') +
  scale_x_discrete(breaks = df_summary_Accipitriformes$date[seq(1, nrow(df_summary_Accipitriformes), by = 2)]) +
  labs(
    title = 'C',
    x = "Year-Month",         
    y = "Number of cases"
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,280),
                     breaks = seq(0,280,40)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

df_summary_Strigiformes <- df_summary %>%
  filter(order == "Strigiformes")
p4 <- ggplot(df_summary_Strigiformes, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCBE7A",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),se = F,method = 'loess') +
  scale_x_discrete(breaks = df_summary_Strigiformes$date[seq(1, nrow(df_summary_Strigiformes), by = 2)]) +
  labs(
    title = 'D',
    x = "Year-Month",         
    y = "Number of cases"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,120)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

df_summary_Passeriformes <- df_summary %>%
  filter(order == "Passeriformes")
p5 <- ggplot(df_summary_Passeriformes, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCBE7A",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),se = F,method = 'loess') +
  scale_x_discrete(breaks = df_summary_Strigiformes$date[seq(1, nrow(df_summary_Strigiformes), by = 2)]) +
  labs(
    title = 'E',
    x = "Year-Month",         
    y = "Number of cases"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,50)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

df_summary_Charadriiformes <- df_summary %>%
  filter(order == "Charadriiformes")
p6 <- ggplot(df_summary_Charadriiformes, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#FCBE7A",width = .9,alpha = 0.8) +
  geom_smooth(aes(x = seq_along(date)),se = F,method = 'loess') +
  scale_x_discrete(breaks = df_summary_Strigiformes$date[seq(1, nrow(df_summary_Strigiformes), by = 2)]) +
  labs(
    title = 'F',
    x = "Year-Month",         
    y = "Number of cases"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,40)) +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16,angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

# Combine the plots into a grid layout
plot_grid(p1, p2, p3,p4, p5, p6,
  ncol = 2
)
# # # save the plot as a PDF file
# ggsave("barplot_by_order.pdf",
#        width = 15, height = 10, dpi = 600,device = cairo_pdf,family = 'Times New Roman')

################################# STL time Series decomposition ######################################
# Establish a time series for total
ts_data <- ts(df_summary_total$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p1 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'A')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

# Establish a time series for Anseriformes
ts_data_Anseriformes <- ts(df_summary_Anseriformes$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data_Anseriformes, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p2 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'B')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )
# Establish a time series for Accipitriformes
ts_data_Accipitriformes <- ts(df_summary_Accipitriformes$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data_Accipitriformes, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p3 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'C')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )
# Establish a time series for Strigiformes
ts_data_Strigiformes <- ts(df_summary_Strigiformes$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data_Strigiformes, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p4 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'D')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

# Establish a time series for Passeriformes
ts_data_Passeriformes <- ts(df_summary_Passeriformes$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data_Passeriformes, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p5 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'E')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

# Establish a time series for Charadriiformes
ts_data_Charadriiformes <- ts(df_summary_Charadriiformes$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_data_Charadriiformes, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
p6 <- autoplot(stl_decomp) +
  geom_line(size = 0.8) +
  labs(title = 'F')+
  scale_x_continuous(
    breaks = seq(2022, 2026, by = 0.25), 
    labels = function(x) {
      year <- floor(x)
      month <- ((x - year) * 12) + 1
      sprintf("%04d-%02d", year, month)
    }
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    text = element_text(size = 16, family = "serif"),  
    plot.title = element_text(size = 24, face = "bold"),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 16),   
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 16), 
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16)   
  )

# Combine the plots into a grid layout
plot_grid(p1, p2, p3,p4, p5, p6,
          ncol = 2
)
# # save the plot as a PDF file
# ggsave("stl_in_us.pdf",
#        width = 18, height = 18, dpi = 600,device = cairo_pdf,family = 'Times New Roman')


############################ Draw a boxplot by season ##########################
# Add season information to the summary data
df_summary_total <- df_summary_total %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)), 
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",   # Spring: March - May
      month %in% c(6, 7, 8) ~ "Summer",   # Summer: June - August
      month %in% c(9, 10, 11) ~ "Autumn", # Autumn: September - November
      month %in% c(12, 1, 2) ~ "Winter"   # Winter: December - February
    )
  )
# Convert season to a factor with specified levels
df_summary_total$season <- factor(df_summary_total$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_total$record_count, 
                                    g = df_summary_total$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)


########################## Create the boxplot for total records by season ##########################
p1 <- df_summary_total %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Total)",x = '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )


################################################### Create boxplots for each order by season ##########################
df_summary_Anseriformes <- df_summary_Anseriformes %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)), 
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",   
      month %in% c(6, 7, 8) ~ "Summer",   
      month %in% c(9, 10, 11) ~ "Autumn", 
      month %in% c(12, 1, 2) ~ "Winter"   
    )
  )
df_summary_Anseriformes$season <- factor(df_summary_Anseriformes$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_Anseriformes$record_count, 
                                    g = df_summary_Anseriformes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p2 <- df_summary_Anseriformes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Anseriformes)",x= '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )


df_summary_Accipitriformes <- df_summary_Accipitriformes %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)), 
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",   
      month %in% c(6, 7, 8) ~ "Summer",   
      month %in% c(9, 10, 11) ~ "Autumn", 
      month %in% c(12, 1, 2) ~ "Winter"   
    )
  )

df_summary_Accipitriformes$season <- factor(df_summary_Accipitriformes$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_Accipitriformes$record_count, 
                                    g = df_summary_Accipitriformes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p3 <- df_summary_Accipitriformes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Accipitriformes)",x = '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )


df_summary_Strigiformes <- df_summary_Strigiformes %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)), 
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",   
      month %in% c(6, 7, 8) ~ "Summer",   
      month %in% c(9, 10, 11) ~ "Autumn", 
      month %in% c(12, 1, 2) ~ "Winter"  
    )
  )
df_summary_Strigiformes$season <- factor(df_summary_Strigiformes$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_Strigiformes$record_count, 
                                    g = df_summary_Strigiformes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p4 <- df_summary_Strigiformes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Strigiformes)",x = '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )


df_summary_Passeriformes <- df_summary_Passeriformes %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)), 
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",   
      month %in% c(6, 7, 8) ~ "Summer",   
      month %in% c(9, 10, 11) ~ "Autumn", 
      month %in% c(12, 1, 2) ~ "Winter"   
    )
  )
df_summary_Passeriformes$season <- factor(df_summary_Passeriformes$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_Passeriformes$record_count, 
                                    g = df_summary_Passeriformes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p5 <- df_summary_Passeriformes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Passeriformes)",x= '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )


df_summary_Charadriiformes <- df_summary_Charadriiformes %>%
  mutate(
    month = as.numeric(substr(date, 6, 7)),
    season = case_when(
      month %in% c(3, 4, 5) ~ "Spring",  
      month %in% c(6, 7, 8) ~ "Summer",   
      month %in% c(9, 10, 11) ~ "Autumn",
      month %in% c(12, 1, 2) ~ "Winter"   
    )
  )
df_summary_Charadriiformes$season <- factor(df_summary_Charadriiformes$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 

# Perform the Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = df_summary_Charadriiformes$record_count, 
                                    g = df_summary_Charadriiformes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p6 <- df_summary_Charadriiformes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',
                  hide_info = T)+
  labs(y = "Count (Charadriiformes)",x = '')+
  theme(
    legend.position = 'none',
    text = element_text(size = 8, family = "serif"),  
    axis.title = element_text(size = 8),  
    axis.text = element_text(size = 8),   
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8)   
  )

# Combine the boxplots into a grid layout
plot_grid(p1, p2, p3,p4, p5, p6,
          labels = c("A", "B", "C", "D", "E", "F"),
          label_size = 12)
# ggsave("season.pdf",
#        width = 8, height = 6, dpi = 600,device = cairo_pdf,family = 'Times New Roman')
# 





