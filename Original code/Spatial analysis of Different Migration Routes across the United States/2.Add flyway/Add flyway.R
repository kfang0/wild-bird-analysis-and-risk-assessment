# R package loading
library(readr)
library(tidyverse)
library(janitor)
library(ggpubr)
library(openxlsx)
# Read flyway data
flyway <- read.xlsx('flyway.xlsx')
names(flyway)[3] <- 'state'
names(flyway)[1] <- 'Flyway'
# Read wild bird data
wild_bird <- read.xlsx('df_with_migratory.xlsx',detectDates = T)
wild_bird_matched <- merge(
  x = wild_bird, 
  y = flyway, 
  by.x = "state", 
  by.y = "state", 
  all.x = TRUE
) %>% 
  select(-中文名称)

# write.xlsx(wild_bird_matched,'wild_bird_with_Migration.xlsx')
