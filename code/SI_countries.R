# Create SI Table Country Stats
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
area_found %>%
  group_by(country) %>%
  summarize(n_areas = n(),
            n_plans = n_distinct(plan_id)) %>% 
  gt() %>% 
  tab_header(title = md("**Table X.** Countries included in management plan review")) %>% 
  cols_label(country = "Alpha-3 Country Code",
             n_areas = "MPAs",
             n_plans = "Management Plans")  %>% 
  cols_align(columns = 2:3, "center") %>% 
  opt_table_font(font = list(google_font("Arial"))) %>% 
  tab_options(
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    column_labels.border.top.width = px(3),
    heading.align = "left",
    data_row.padding = px(3)) %>% 
  gtsave(., filename = file.path("figs", "SI_TabX_countries.png"))

