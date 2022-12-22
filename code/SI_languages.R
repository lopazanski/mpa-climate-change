# SI Table Languages
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
# New df only including stat (not text detail)
review_stat <- review[review$type == "stat",]

# Make Table -------------------------------------------------------------------
review_stat %>% 
  filter(type == "stat" & q_code == "metadata_language") %>% 
  group_by(entry) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(proportion = round(count/sum(count), 3)) %>% 
  arrange(-count)%>% 
  mutate(entry = str_to_sentence(entry)) %>% 
  gt() %>% 
  tab_header(title = md("**Table X.** Languages included in management plan review")) %>% 
  cols_label(entry = "Language",
             count = "Number of Management Plans",
             proportion = "Proportion") %>% 
  cols_align(columns = 2:3, "center") %>% 
  opt_table_font(font = list(google_font("Arial"))) %>% 
  tab_options(
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    column_labels.border.top.width = px(3),
    heading.align = "left",
    data_row.padding = px(3)) %>% 
  gtsave(., filename = file.path("figs", "SI_TabX_languages.png"))