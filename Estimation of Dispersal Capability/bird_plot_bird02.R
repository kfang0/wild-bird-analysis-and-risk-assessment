# R package loading
library(readr)
library(tidyverse)
library(janitor)
library(ggsci)
library(RColorBrewer)
library(cowplot)
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
df$date <- format(df$collection_date, "%Y-%m")
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


# Group according to the order column and calculate the number of each category
df_order <- df %>%
  group_by(order) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

# Add a new column to distinguish the first five from the others
df_order$color_group <- c("Top 5", "Top 5", "Top 5", "Top 5", "Top 5", rep("Others", nrow(df_order) - 5))
# Draw a horizontal bar chart
p1 <- ggplot(data = df_order, aes(x = reorder(order, count), y = log(count), fill = color_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Top 5" = "#b83b5e", "Others" = "lightgray")) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 10, 2)) +
  coord_flip() +
  labs(x = "Order", y = "Log - count",title = "A") +
  theme_classic()+
  theme(legend.position = 'none')

df_order_date <- df %>%
  group_by(date, order) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

df_order_date$order <- ifelse(df_order_date$order == "Anseriformes", "Anseriformes",
                              ifelse(df_order_date$order == "Accipitriformes", "Accipitriformes",
                                     ifelse(df_order_date$order == "Strigiformes", "Strigiformes",
                                            ifelse(df_order_date$order == "Passeriformes", "Passeriformes",
                                                   ifelse(df_order_date$order == "Charadriiformes", "Charadriiformes","Others")))))

df_order_date$order <- factor(df_order_date$order, 
                              levels = c("Anseriformes", "Accipitriformes", "Strigiformes", 
                                         "Passeriformes", "Charadriiformes", "Others"))
   
df_order_date <- df_order_date %>% 
  arrange(date, order)
unique_dates <- unique(df_order_date$date)

# Draw a composition ratio bar chart
p2 <- ggplot(data = df_order_date, aes(x = date, y = count, fill = order,group = order)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_bmj() +
  scale_y_continuous(labels = scales::percent_format(),expand = c(0,0)) +
  labs(x = "Year-Month", y = "Proportion", fill = " ",title = "B") +
  scale_x_discrete(breaks = unique_dates[seq(1, length(unique_dates), by = 3)])+
  theme_classic() +
  theme(
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))+
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(0.8, "lines"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
  ) 
                                                        
########################################################################################################################
R0_data <- data.frame(
  order = c("Anseriformes", "Accipitriformes", "Strigiformes", "Passeriformes", "Charadriiformes"),
  R0 = c(0.992136, 1.655384, 3.164162, 1.02942, 1.279073),
  lower_bound = c(0.9901296, 1.571331, 2.71091, 1.019543, 1.197978),
  upper_bound = c(0.9941433, 1.747938, 3.726068, 1.039483, 1.369301),
  method = rep("Exponential Growth", 5)
)

R0_data$order <- factor(R0_data$order, 
                         levels = c("Anseriformes", "Accipitriformes", "Strigiformes", 
                                    "Passeriformes", "Charadriiformes"))
# Draw a point chart with error bars
p3 <- ggplot(R0_data, aes(x = order, y = R0,color = order)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.1, size = 0.8) +  
  geom_point(size = 2, shape = 15) +  
  scale_color_jama()+
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
  labs(x = "Order",
       y = "Basic reproductive number",
       title = "C") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 
# Combine the three plots into one figure
left_column <- plot_grid(p1, p3, ncol = 1, align = 'v', axis = 'lr') 
plot_grid(left_column, p2, ncol = 2,
                        rel_widths = c(1.2, 1)) 

ggsave('bird.pdf',width = 15,height = 8,dpi = 600,device = cairo_pdf,family = 'Times New Roman')











                                                         
