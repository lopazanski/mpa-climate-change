# Export extracted climate change strateges

# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
# Processed document review
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

q_climate <- c("cliamte_sink", "climate_limitations", "climate_action", "climate_plan",
               starts_with("obj_cc"), "assess_climate", starts_with("design_cc"),
               "design_adaptive_climate", starts_with("monitor_exp"), contains("cc"))

# Select only details
review_detail <- review %>% 
  filter(type == "detail") %>% 
  filter(str_detect(q_code, "cc") | str_detect(q_code, "climate")) %>% 
  filter(!(is.na(entry))) %>% 
  filter(!(q_code %in% c("climate_mention", "climate_action")))

write.csv(review_detail, file.path("figs", "Table S5 All Extracted Explicit Climate Change Information.csv"))
         