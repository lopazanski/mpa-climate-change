# Create SI Table Ocean Basin Stats
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Packages
library(tidyverse)
library(sf)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds")) 


# Build Data -------------------------------------------------------------------
# Drop geometry
area <- area_sf %>% sf::st_drop_geometry()

# New df for only the MPA areas that we found plans for
area_found <- area[area$search == "F",]


# Make Table -------------------------------------------------------------------

## Major ocean basins (region) ----
area_found %>%
  group_by(region) %>%
  summarize(n_zones = n(),
            n_plans = n_distinct(plan_id)) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = md("**Table X.** Review coverage across major ocean basins")) %>% 
  cols_label(region = "Ocean Basin",
             n_zones = "Number of MPAs",
             n_plans = "Number of Management Plans") %>% 
  opt_table_font(font = list(google_font("Arial"))) %>% 
  tab_options(
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    heading.align = "left",
    column_labels.border.top.width = px(3),
    data_row.padding = px(3)) %>% 
  gt::gtsave(., filename = file.path("figs", "SI_TabX_ocean_basins.png"))
