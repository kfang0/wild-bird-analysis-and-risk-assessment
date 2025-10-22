# R package loading
library(readr)
library(tidyverse)
library(ggpubr)
library(openxlsx)
library(patchwork)
library(cowplot)
library(dunn.test)
library(tidyplots)
library(forecast)
options(warn = -1)
# Read flyway data
flyway <- read.xlsx('wild_bird_with_Migration.xlsx',detectDates = T)
# Filter data with a sampling date of January 1, 2022 or more
flyway <- subset(flyway,collection_date >= as.Date("2022-01-01"))
flyway <- subset(flyway, collection_date <= as.Date("2025-04-30"))
# Group count by date and migration route
df_summary <- flyway %>% 
  group_by(date,Flyway) %>% 
  summarise(record_count = n()) %>%
  ungroup() %>%  
  complete(date, fill = list(record_count = 0))

# Create complete month sequence
start_date <- "2022-01"
end_date <- "2025-04"
all_flyways <- c("Pacific", "Central", "Mississippi", "Atlantic") 
# Generate complete month sequence 
date_sequence <- seq(as.Date("2022-01-01"), as.Date("2025-04-01"), by = "month")
date_strings <- format(date_sequence, "%Y-%m") # Convert back to YYYY-MM format string
# Create complete skeleton containing all target dates and all Flyways
full_skeleton <- expand_grid(
  date = date_strings,
  Flyway = all_flyways
)
# Merge statistical results
df_summary_fixed_range <- full_skeleton %>%
  left_join(
    flyway %>% 
      group_by(date, Flyway) %>% 
      summarise(record_count = n(), .groups = 'drop'), 
    by = c("date", "Flyway")
  ) %>%
  # Fill NA values produced by left_join with 0
  mutate(record_count = replace_na(record_count, 0))

# Filter Pacific data
Pacific_df <- subset(df_summary_fixed_range,Flyway == 'Pacific')
p1 <- ggplot(Pacific_df, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#34778C",width = .9,alpha = 0.8) +
  scale_x_discrete(
    breaks = Pacific_df$date[seq(1, length(Pacific_df$date), by = 2)]
  )+
  labs(
    x = "Year-Month",         
    y = "Number of cases(Pacific Flyway)"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,400)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    text = element_text(family = 'serif')
  )

# Filter Central data
Central_df <- subset(df_summary_fixed_range,Flyway == 'Central')
p2 <- ggplot(Central_df, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#DDAC57",width = .9,alpha = 0.8) +
  scale_x_discrete(
    breaks = Central_df$date[seq(1, length(Central_df$date), by = 2)]
  )+
  labs(
    x = "Year-Month",         
    y = "Number of cases(Central Flyway)"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,300)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    text = element_text(family = 'serif')
  )

# Filter Mississippi data
Mississippi_df <- subset(df_summary_fixed_range,Flyway == 'Mississippi')
p3 <- ggplot(Mississippi_df, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#DD6E51",width = .9,alpha = 0.8) +
  scale_x_discrete(
    breaks = Mississippi_df$date[seq(1, length(Mississippi_df$date), by = 2)]
  )+
  labs(
    x = "Year-Month",         
    y = "Number of cases(Mississippi Flyway)"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,500)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    text = element_text(family = 'serif')
  )

# Filter Atlantic data
Atlantic_df <- subset(df_summary_fixed_range,Flyway == 'Atlantic')
p4 <- ggplot(Atlantic_df, aes(x = date, y = record_count)) +
  geom_bar(stat = "identity", fill = "#45465C",width = .9,alpha = 0.8) +
  scale_x_discrete(
    breaks = Atlantic_df$date[seq(1, length(Atlantic_df$date), by = 2)]
  )+
  labs(
    x = "Year-Month",         
    y = "Number of cases(Atlantic Flyway)"             
  ) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),limits = c(0,300)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) ,
    text = element_text(family = 'serif')
  )

# Combine the plots into a grid layout
plot_grid(p1, p2, p3,p4,
          ncol = 2,
          labels = c("A", "B", "C", "D"),
          label_size = 14
)
# ggsave("barplot_by_flyway.pdf",
#        width = 15, height = 10, dpi = 600)

########################### STL decomposition in different road
# Pacific Road
# Establish a time series for total
ts_Pacific <- ts(Pacific_df$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_Pacific, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
plot(stl_decomp,lwd = 1.5)

# Central Road
# Establish a time series for total
ts_Central <- ts(Central_df$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_Central, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
plot(stl_decomp,lwd = 1.5)

# Mississippi Road
# Establish a time series for total
ts_Mississippi <- ts(Mississippi_df$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_Mississippi, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
plot(stl_decomp,lwd = 1.5)

# Atlantic Road
# Establish a time series for total
ts_Atlantic <- ts(Atlantic_df$record_count, start = c(2022, 1), frequency = 12) 
# STL Decomposition
stl_decomp <- stl(ts_Atlantic, s.window = "periodic",robust = T) 
stl_decomp
# Visualize the decomposition results
plot(stl_decomp,lwd = 1.5)

#######################################################
## Group count by migration route and whether it's a migratory bird
df_magritory <- flyway %>% 
  group_by(date,Flyway,magritory) %>% 
  summarise(record_count = n()) %>%
  ungroup() %>%  
  complete(date, fill = list(record_count = 0))
df_magritory_seasonal <- df_magritory %>%
  # Extract month: extract MM part from "YYYY-MM" format date string and convert to numeric
  mutate(
    month = as.numeric(str_extract(date, "(?<=-)\\d{2}$"))
  ) %>%
  # Divide seasons: use case_when to create season column based on month
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
      TRUE ~ NA_character_
    )
  )
# Convert season to a factor with specified levels
df_magritory_seasonal$season <- factor(df_magritory_seasonal$season, levels = c("Spring", "Summer", "Autumn", "Winter")) 
df_magritory_seasonal <- na.omit(df_magritory_seasonal)
comparisons <- list(
  c("Spring", "Summer"),
  c("Spring", "Autumn"),
  c("Spring", "Winter"),
  c("Summer", "Autumn"),
  c("Summer", "Winter"),
  c("Autumn", "Winter")
)
season_colors <- c(
  "Spring" = "#0D8992",  # Spring
  "Summer" = "#A74035",  # Summer
  "Autumn" = "#FF5722",  # Autumn
  "Winter" = "#33313F"   # Winter
)

# Filter Pacific route
magritory_pacific <- subset(df_magritory_seasonal,(Flyway == 'Pacific'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_pacific$record_count, 
                                    g = magritory_pacific$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)
p1 <- magritory_pacific %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Pacific in total)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))


# Filter Pacific route with YES
magritory_pacific_yes <- subset(df_magritory_seasonal,(Flyway == 'Pacific') & (magritory == 'YES'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_pacific_yes$record_count, 
                                    g = magritory_pacific_yes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p2 <- magritory_pacific_yes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Pacific in magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Pacific route with NO
magritory_pacific_no <- subset(df_magritory_seasonal,(Flyway == 'Pacific') & (magritory == 'NO'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_pacific_no$record_count, 
                                    g = magritory_pacific_no$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)
p3 <- magritory_pacific_no %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Pacific in non-magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Central route
magritory_central <- subset(df_magritory_seasonal,(Flyway == 'Central'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_central$record_count, 
                                    g = magritory_central$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p4 <- magritory_central %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Central in total)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Central route with YES
magritory_central_yes <- subset(df_magritory_seasonal,(Flyway == 'Central') & (magritory == 'YES'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_central_yes$record_count, 
                                    g = magritory_central_yes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p5 <- magritory_central_yes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Central in magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Central route with NO
magritory_central_no <- subset(df_magritory_seasonal,(Flyway == 'Central') & (magritory == 'NO'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_central_no$record_count, 
                                    g = magritory_central_no$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p6 <- magritory_central_no %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Central in non-magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Mississippi route
magritory_mississippi <- subset(df_magritory_seasonal,(Flyway == 'Mississippi'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_mississippi$record_count, 
                                    g = magritory_mississippi$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p7 <- magritory_mississippi %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Mississippi in total)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Mississippi route with YES
magritory_mississippi_yes <- subset(df_magritory_seasonal,(Flyway == 'Mississippi') & (magritory == 'YES'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_mississippi_yes$record_count, 
                                    g = magritory_mississippi_yes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p8 <- magritory_mississippi_yes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Mississippi in magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Mississippi route with NO
magritory_mississippi_no <- subset(df_magritory_seasonal,(Flyway == 'Mississippi') & (magritory == 'NO'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_mississippi_no$record_count, 
                                    g = magritory_mississippi_no$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p9 <- magritory_mississippi_no %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Mississippi in magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Atlantic route
magritory_atlantic <- subset(df_magritory_seasonal,(Flyway == 'Atlantic'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_atlantic$record_count, 
                                    g = magritory_atlantic$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p10 <- magritory_atlantic %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Atlantic in total)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Atlantic route with YES
magritory_atlantic_yes <- subset(df_magritory_seasonal,(Flyway == 'Atlantic') & (magritory == 'YES'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_atlantic_yes$record_count, 
                                    g = magritory_atlantic_yes$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p11 <- magritory_atlantic_yes %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of cases (Atlantic in magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

# Filter Atlantic route with NO
magritory_atlantic_no <- subset(df_magritory_seasonal,(Flyway == 'Atlantic') & (magritory == 'NO'))
# Perform Kruskal-Wallis test and use Bonferroni correction
dunn_result_bonferroni <- dunn.test(x = magritory_atlantic_no$record_count, 
                                    g = magritory_atlantic_no$season, 
                                    method = "bonferroni", 
                                    altp = TRUE)

p12 <- magritory_atlantic_no %>% 
  tidyplot(x = season,y = record_count,color = season) %>% 
  reorder_x_axis_labels("Spring",'Summer','Autumn','Winter') %>% 
  add_boxplot() %>% 
  add_test_pvalue(method = 'dunn_test',p.adjust.method = 'bonferroni',hide_info = T)+
  scale_color_manual(values = season_colors) +
  scale_fill_manual(values = season_colors) +
  labs(y = "Number of case (Atlantic in non-magritory birds)")+
  theme(legend.position = "none",
        text = element_text(family = 'serif',size = 16))

plot_grid(p1,p4,p7,p10,p2,p5,p8,p11,p3,p6,p9,p12,
          labels = c("A-1", "B-1", "C-1", "D-1", "A-2", "B-2","C-2","D-2","A-3","B-3","C-3","D-3"),
          label_size = 10,ncol = 4)



# ggsave("season_flyway.pdf",
#        width = 10, height = 8, dpi = 600)






