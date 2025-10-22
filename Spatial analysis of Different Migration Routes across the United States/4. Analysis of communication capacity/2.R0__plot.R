# R package loading
library(readr)
library(tidyverse)
library(ggpubr)
library(openxlsx)
library(patchwork)
library(cowplot)
library(scales)
# read R0
df <- read.xlsx('R0.xlsx')
# Ensure 'status' and 'flyway' are factors for proper plotting order
df_plot <- df %>%
  mutate(
    # Order 'status' on the Y-axis: total should be first
    status = factor(status, 
                    levels = c("total", "magritory", "no_magritory")),
    
    # Order 'flyway' for the facets (Optional: ensures Pacific comes first)
    flyway = factor(flyway, 
                    levels = c("Pacific", "Central", "Mississippi", "Atlantic"))
  ) %>%
  # Rename for cleaner plot labels
  mutate(
    status_label = case_when(
      status == "total" ~ "Overall",
      status == "magritory" ~ "Migratory",
      status == "no_magritory" ~ "Non-Migratory"
    )
  )

# Draw graphs and confidence intervals
p1 <- df_plot %>% 
  filter(flyway == 'Pacific') %>% 
  ggplot(aes(x = r0, y = status_label, color = status_label)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper), 
    height = 0.15, 
    linewidth = 1
  ) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6, base = 10) 
  ) +
  labs(
    x = expression("Basic Reproduction Number (" * R[0] * ") [Log Scale]"),
    y = " ",
  ) +
  scale_color_manual(
    values = c("Overall" = "#1f77b4", "Migratory" = "#ff7f0e", "Non-Migratory" = "#2ca02c")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    strip.background = element_rect(fill = "gray90"),
    text = element_text(family = 'serif', size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p2 <- df_plot %>% 
  filter(flyway == 'Central') %>% 
  ggplot(aes(x = r0, y = status_label, color = status_label)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper), 
    height = 0.15, 
    linewidth = 1
  ) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6, base = 10) 
  ) +
  labs(
    x = expression("Basic Reproduction Number (" * R[0] * ") [Log Scale]"),
    y = " ",
  ) +
  scale_color_manual(
    values = c("Overall" = "#1f77b4", "Migratory" = "#ff7f0e", "Non-Migratory" = "#2ca02c")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    strip.background = element_rect(fill = "gray90"),
    text = element_text(family = 'serif', size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p3 <- df_plot %>% 
  filter(flyway == 'Mississippi') %>% 
  ggplot(aes(x = r0, y = status_label, color = status_label)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper), 
    height = 0.15, 
    linewidth = 1
  ) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6, base = 10) 
  ) +
  labs(
    x = expression("Basic Reproduction Number (" * R[0] * ") [Log Scale]"),
    y = " ",
  ) +
  scale_color_manual(
    values = c("Overall" = "#1f77b4", "Migratory" = "#ff7f0e", "Non-Migratory" = "#2ca02c")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    strip.background = element_rect(fill = "gray90"),
    text = element_text(family = 'serif', size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

p4 <- df_plot %>% 
  filter(flyway == 'Atlantic') %>% 
  ggplot(aes(x = r0, y = status_label, color = status_label)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper), 
    height = 0.15, 
    linewidth = 1
  ) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  scale_x_log10(
    breaks = scales::log_breaks(n = 6, base = 10) 
  ) +
  labs(
    x = expression("Basic Reproduction Number (" * R[0] * ") [Log Scale]"),
    y = " ",
  ) +
  scale_color_manual(
    values = c("Overall" = "#1f77b4", "Migratory" = "#ff7f0e", "Non-Migratory" = "#2ca02c")
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    strip.background = element_rect(fill = "gray90"),
    text = element_text(family = 'serif', size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plot_grid(p1,p2,p3,p4,
          labels = c('A','B','C','D'))

# ggsave("basic_r0.pdf",
#        width = 12, height = 8, dpi = 600)








