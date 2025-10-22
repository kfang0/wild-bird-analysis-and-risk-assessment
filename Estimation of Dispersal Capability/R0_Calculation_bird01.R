# Load the R package
library(readr)
library(tidyverse)
library(janitor)
library(cowplot)
library(R0)
options(warn = -1)
# Data Reading
df <- read_csv('hpai-wild-birds.csv')
df <- clean_names(df)
# Convert the collection_date column to the date format
df$collection_date <- as.Date(df$collection_date, format = "%m/%d/%Y")
# Filter data with a sampling date of January 1, 2022 or more
df <- subset(df,collection_date >= as.Date("2022-01-01"))
df <- subset(df, collection_date <= as.Date("2025-04-30"))

# Add a new column of year and week numbers
df <- df %>%
  mutate(year = year(collection_date),
         week = week(collection_date)) %>% 
  arrange(year, week) 

# Convert the value of the bird_species column to lowercase
df <- df %>%
  mutate(bird_species = tolower(bird_species))

# Mapping Relationship of ornithales classification
bird_orders <- list(
  "anseriformes" = c("wigeon", "gadwall", "mallard", "pintail", "shoveler", "teal", 
                     "duck", "goose", "swan", "merganser", "eider", "scaup", 
                     "brant", "whistling duck", "screamer","canvasback","common goldeneye","bufflehead","baer's pochard",
                     "white-winged scoter","black scoter","smew","surf scoter"),
  "accipitriformes" = c("eagle", "hawk", "kite", "harrier", "condor", "osprey","black vulture",
                        "king vulture","vulture"),
  "strigiformes" = c("owl"),
  "pelecaniformes" = c("pelican", "ibis", "spoonbill", "heron", "egret"),
  "charadriiformes" = c("gull", "tern", "jaeger", "skimmer", "plover", "turnstone", 
                        "sandpiper", "dunlin", "willet", "kittiwake", "razorbill", 
                        "murre", "skimmer", "phalarope", "stilt","sanderling"),
  "gruiformes" = c("crane", "coot", "rail"),
  "galliformes" = c("turkey", "pheasant", "quail", "grouse"),
  "columbiformes" = c("dove", "pigeon"),
  "cuculiformes" = c("cuckoo", "roadrunner"),
  "caprimulgiformes" = c("nighthawk", "poorwill"),
  "apodiformes" = c("swift", "hummingbird"),
  "coraciiformes" = c("kingfisher", "roller", "kookaburra"),
  "piciformes" = c("woodpecker", "flicker", "sapsucker"),
  "passeriformes" = c("crow", "raven", "jay", "magpie", "sparrow", "finch",
                      "blackbird", "grackle", "starling", "warbler", "tanager", 
                      "swallow", "martin", "thrush", "mockingbird", "catbird",
                      "thrasher", "wren", "nuthatch", "creeper", "titmouse", "chickadee",
                      "kinglet", "gnatcatcher", "flycatcher", "phoebe",
                      "pewee", "vireo", "shrike", "waxwing", "starling", "oriole", 
                      "meadowlark",  "cowbird", 
                      "tanager", "cardinal", "grosbeak", "bunting",
                      "towhee", "sparrow", "junco", "longspur", "snow bunting","american robin"),
  "podicipediformes" = c("grebe"),
  "gaviiformes" = c("loon"),
  "falconiformes" = c("falcon", "caracara", "kestrel","merlin"),
  "phoenicopteriformes" = c("flamingo"),
  "procellariiformes" = c("fulmar", "shearwater"),
  "rheiformes" = c("rhea"),
  "casuariiformes" = c("emu"),
  "psittaciformes" = c("lorikeet", "parrot"),
  "suliformes" = c("cormorant","gannet"),
  "ciconiiformes" = c("stork")
)

# Add the order column
df <- df %>%
  mutate(order = case_when(
    str_detect(bird_species, paste(bird_orders$anseriformes, collapse = "|")) ~ "Anseriformes",
    str_detect(bird_species, paste(bird_orders$accipitriformes, collapse = "|")) ~ "Accipitriformes",
    str_detect(bird_species, paste(bird_orders$strigiformes, collapse = "|")) ~ "Strigiformes",
    str_detect(bird_species, paste(bird_orders$pelecaniformes, collapse = "|")) ~ "Pelecaniformes",
    str_detect(bird_species, paste(bird_orders$charadriiformes, collapse = "|")) ~ "Charadriiformes",
    str_detect(bird_species, paste(bird_orders$gruiformes, collapse = "|")) ~ "Gruiformes",
    str_detect(bird_species, paste(bird_orders$galliformes, collapse = "|")) ~ "Galliformes",
    str_detect(bird_species, paste(bird_orders$columbiformes, collapse = "|")) ~ "Columbiformes",
    str_detect(bird_species, paste(bird_orders$cuculiformes, collapse = "|")) ~ "Cuculiformes",
    str_detect(bird_species, paste(bird_orders$caprimulgiformes, collapse = "|")) ~ "Caprimulgiformes",
    str_detect(bird_species, paste(bird_orders$apodiformes, collapse = "|")) ~ "Apodiformes",
    str_detect(bird_species, paste(bird_orders$coraciiformes, collapse = "|")) ~ "Coraciiformes",
    str_detect(bird_species, paste(bird_orders$piciformes, collapse = "|")) ~ "Piciformes",
    str_detect(bird_species, paste(bird_orders$passeriformes, collapse = "|")) ~ "Passeriformes",
    str_detect(bird_species, paste(bird_orders$podicipediformes, collapse = "|")) ~ "Podicipediformes",
    str_detect(bird_species, paste(bird_orders$gaviiformes, collapse = "|")) ~ "Gaviiformes",
    str_detect(bird_species, paste(bird_orders$falconiformes, collapse = "|")) ~ "Falconiformes",
    str_detect(bird_species, paste(bird_orders$phoenicopteriformes, collapse = "|")) ~ "Phoenicopteriformes",
    str_detect(bird_species, paste(bird_orders$procellariiformes, collapse = "|")) ~ "Procellariiformes",
    str_detect(bird_species, paste(bird_orders$rheiformes, collapse = "|")) ~ "Rheiformes",
    str_detect(bird_species, paste(bird_orders$casuariiformes, collapse = "|")) ~ "Casuariiformes",
    str_detect(bird_species, paste(bird_orders$psittaciformes, collapse = "|")) ~ "Psittaciformes",
    str_detect(bird_species, paste(bird_orders$suliformes, collapse = "|")) ~ "Suliformes",
    str_detect(bird_species, paste(bird_orders$ciconiiformes, collapse = "|")) ~ "Ciconiiformes",
    TRUE ~ "Unidentified"
  ))

#################### # Calculate R0 and its confidence interval ##########################

## Filter Anseriformes
df_anseriformes <- df %>%
  filter(order == "Anseriformes") %>% 
  group_by(year, week) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  complete(
    year = 2022:2025,
    week = 1:52,
    fill = list(count = 0)
  ) %>%
  arrange(year, week)  %>% 
  filter(!(year == 2025 & week > 14)) %>%   
  arrange(year, week)
df_anseriformes <- df_anseriformes %>%
  mutate(lookup_count = NA)  

for (i in 1:nrow(df_anseriformes)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_anseriformes)) {
    sum_count <- sum_count + df_anseriformes$count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    
    if (sum_count > 0) {
      df_anseriformes$lookup_count[i] <- rows_needed
      break
    }
  }
}
df_anseriformes$lookup_count <- df_anseriformes$lookup_count + 1
df_anseriformes <- na.omit(df_anseriformes)
# Calculate R0 and its confidence interval
gtn <- est.GT(serial.interval = df_anseriformes$lookup_count)
result <- est.R0.EG(na.omit(df_anseriformes$count),                
                    GT = gtn)
result

## Filter Accipitriformes
df_Accipitriformes <- df %>%
  filter(order == "Accipitriformes") %>% 
  group_by(year, week) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  complete(
    year = 2022:2025,
    week = 1:52,
    fill = list(count = 0)
  ) %>%
  arrange(year, week) %>% 
  filter(!(year == 2025 & week > 14)) %>%   
  arrange(year, week)

df_Accipitriformes <- df_Accipitriformes %>%
  mutate(lookup_count = NA)  

for (i in 1:nrow(df_Accipitriformes)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_Accipitriformes)) {
    sum_count <- sum_count + df_Accipitriformes$count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    
    if (sum_count > 0) {
      df_Accipitriformes$lookup_count[i] <- rows_needed
      break
    }
  }
}
df_Accipitriformes$lookup_count <- df_Accipitriformes$lookup_count+1
df_Accipitriformes <- na.omit(df_Accipitriformes)
gtn <- est.GT(serial.interval = df_Accipitriformes$lookup_count)
result <- est.R0.EG(df_Accipitriformes$count,                
  GT = gtn        
)
result

##############
## Filter Strigiformes
df_Strigiformes <- df %>%
  filter(order == "Strigiformes") %>% 
  group_by(year,week) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  complete(
    year = 2022:2025,
    week = 1:52,
    fill = list(count = 0)
  ) %>%
  arrange(year, week) %>% 
  filter(!(year == 2025 & week > 14)) %>%   
  arrange(year, week)

df_Strigiformes <- df_Strigiformes %>%
  mutate(lookup_count = NA) 

for (i in 1:nrow(df_Strigiformes)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_Strigiformes)) {
    sum_count <- sum_count + df_Strigiformes$count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    
    if (sum_count > 0) {
      df_Strigiformes$lookup_count[i] <- rows_needed
      break
    }
  }
}

df_Strigiformes$lookup_count <- df_Strigiformes$lookup_count + 1
df_Strigiformes <- na.omit(df_Strigiformes)
# Calculate R0 and its confidence interval
gtn <- est.GT(serial.interval = df_Strigiformes$lookup_count)
result <- est.R0.EG(df_Strigiformes$count,                
                    GT = gtn        
)
result

#################################
## Filter  Passeriformes
df_Passeriformes <- df %>%
  filter(order == "Passeriformes") %>% 
  group_by(year,week) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  complete(
    year = 2022:2025,
    week = 1:52,
    fill = list(count = 0)
  ) %>%
  arrange(year, week) %>% 
  filter(!(year == 2025 & week > 14)) %>%   
  arrange(year, week)

df_Passeriformes <- df_Passeriformes %>%
  mutate(lookup_count = NA) 

for (i in 1:nrow(df_Passeriformes)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_Passeriformes)) {
    sum_count <- sum_count + df_Passeriformes$count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    
    if (sum_count > 0) {
      df_Passeriformes$lookup_count[i] <- rows_needed
      break
    }
  }
}
df_Passeriformes$lookup_count <- df_Passeriformes$lookup_count + 1
df_Passeriformes <- na.omit(df_Passeriformes)
# Calculate R0 and its confidence interval
gtn <- est.GT(serial.interval = df_Passeriformes$lookup_count)
result <- est.R0.EG(df_Passeriformes$count,                
                    GT = gtn        
)
result

##############
## Filter  Charadriiformes
df_Charadriiformes <- df %>%
  filter(order == "Charadriiformes") %>% 
  group_by(year,week) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  complete(
    year = 2022:2025,
    week = 1:52,
    fill = list(count = 0)
  ) %>%
  arrange(year, week) %>% 
  filter(!(year == 2025 & week > 14)) %>%   
  arrange(year, week)
df_Charadriiformes <- df_Charadriiformes %>%
  mutate(lookup_count = NA)  

for (i in 1:nrow(df_Charadriiformes)) {
  sum_count <- 0
  rows_needed <- 0
  
  for (j in i:nrow(df_Charadriiformes)) {
    sum_count <- sum_count + df_Charadriiformes$count[j]
    
    if (j > i) {
      rows_needed <- rows_needed + 1
    }
    
    if (sum_count > 0) {
      df_Charadriiformes$lookup_count[i] <- rows_needed
      break
    }
  }
}
df_Charadriiformes$lookup_count <- df_Charadriiformes$lookup_count + 1
df_Charadriiformes <- na.omit(df_Charadriiformes)
# Calculate R0 and its confidence interval
gtn <- est.GT(serial.interval = df_Charadriiformes$lookup_count)
result <- est.R0.EG(df_Charadriiformes$count,                
                    GT = gtn        
)
result

######################################## Establish a data frame ###########################################
# Establish a data frame for the propagation capability based on the results of each 
# item obtained from the above code operation
reproduction_numbers <- data.frame(
  order = c("Anseriformes", "Accipitriformes", "Strigiformes", "Passeriformes", "Charadriiformes"),
  R = c(0.992136, 1.655384, 3.164162, 1.02942, 1.279073),
  lower_CI = c(0.9901296, 1.571331, 2.71091, 1.019543, 1.197978),
  upper_CI = c(0.9941433, 1.747938, 3.726068, 1.039483, 1.369301),
  method = rep("Exponential Growth", 5)
)

