library(tidyverse)
library(readxl)
library(openxlsx)
library(networkD3)
library(htmlwidgets)
library(webshot)
library(showtext)
showtext_auto()
options(warn = -1)
# Data reading
df <- read_xls('gisaid_epiflu_isolates.xls')
# Filter for high pathogenicity
df <- subset(df,Pathogenicity == 'HPAI')
# Filter bird data
# List of Host types to filter
bird_hosts <- c(
  "Avian", "Guineafowl", "Falcon", "Passerine", "Gull", "Other avian", "Eagle", "Swan",
  "Anser rossii", "Anser caerulescens", "Sterna hirundo", "Buteo lineatus", "Haliaeetus leucocephalus",
  "Somateria mollissima", "Pheasant", "Peafowl", "Lophodytes cucullatus", "Mallard", "Pigeon", "Crow",
  "Meleagris gallopavo", "Shearwater", "Wild bird", "Aythya affinis", "Buteo jamaicensis",
  "Larus smithsonianus", "Falco peregrinus", "Larus marinus", "Branta canadensis", "Cormorant",
  "Calidris alba", "Cairina moschata", "Larus", "Larus delawarensis"
)
# Filter data
df_bird <- df %>% 
  filter(Host %in% bird_hosts)
df_bird$Subtype <- gsub("^A\\s*/\\s*", "", df_bird$Subtype)
# Remove parentheses and all content inside them (including HTML tags)
df_bird$Genotype <- gsub("\\s*\\(.*?\\)", "", df_bird$Genotype)
# Use sub() to extract the first 4 digits
df_bird$Year <- as.numeric(sub("^(\\d{4}).*", "\\1", df_bird$Collection_Date))
# Split by "/" and take the 2nd element
df_bird$Host_from_Name <- sapply(str_split(df_bird$Isolate_Name, "/"), `[`, 2)
# Remove names that don't meet requirements
# Define list of hosts to exclude
exclude_hosts <- c(
  "chicken", "duck", "turkey", "goose", "domestic duck", "domestic goose", 
  "poultry", "avian", "Raptor", "Wild bird", "Wild-Bird", "Poultry", 
  "Backyard bird", "Avian", "bottlenose dolphin", "fisher", "wild bird"
)

# Exclude specified host types
df_bird<- df_bird %>%
  filter(!Host_from_Name %in% exclude_hosts)

# Read targeted data
df_order <- read.xlsx('order name.xlsx')
# Merge order information into df_bird_filtered
df_bird <- df_bird %>%
  left_join(df_order, by = c("Host_from_Name" = "species"))
df_bird$order <- str_trim(df_bird$order)
# Remove data containing "Minor", "Notassigned" and "Not assigned"
df_bird_clean <- df_bird %>%
  filter(!grepl("Minor|Notassigned|Not assigned", Genotype))

# Aggregate data
df_alluvial <- df_bird_clean %>%
  count(order, Genotype, Year)
df_alluvial$Year <- factor(df_alluvial$Year, levels = c(2022, 2023, 2024, 2025))

# Prepare data
# Year -> Genotype
links1 <- df_alluvial %>%
  group_by(Year, Genotype) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  mutate(Year = paste0("Year_", Year)) %>%
  rename(source = Year, target = Genotype)

# Genotype -> Order  
links2 <- df_alluvial %>%
  group_by(Genotype, order) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  rename(source = Genotype, target = order)

# Merge links
all_links <- bind_rows(links1, links2)

# Key fix: only sort years, keep others in original order
# Arrange Years in chronological order
year_nodes <- c('Year_2022', 'Year_2023', 'Year_2024', 'Year_2025')

# Genotype in order of appearance in data (no sorting)
genotype_nodes <- unique(df_alluvial$Genotype)

# Order in order of appearance in data (no sorting)
order_nodes <- unique(df_alluvial$order)

# Create node dataframe, strictly in left-to-right order
nodes <- data.frame(
  name = c(year_nodes, genotype_nodes, order_nodes),
  stringsAsFactors = FALSE
)

# Convert indices
all_links$source <- match(all_links$source, nodes$name) - 1
all_links$target <- match(all_links$target, nodes$name) - 1

# Create enhanced Sankey diagram
p <- sankeyNetwork(
  Links = all_links, 
  Nodes = nodes,
  Source = "source", 
  Target = "target", 
  Value = "value",
  NodeID = "name", 
  fontSize = 12,          
  nodeWidth = 25,          
  sinksRight = FALSE,
  width = 1200,            
  height = 700,           
  margin = list(top = 50, right = 50, bottom = 50, left = 50),
  iterations = 0
)

# First save as HTML file
saveNetwork(p, file = "sankey_plot.html", selfcontained = TRUE)

# Set DPI to 600, export as PDF
webshot("sankey_plot.html", "sankey_plot.pdf", 
        vwidth = 1200,    
        vheight = 700,    
        zoom = 5,          
        delay = 3)         
















