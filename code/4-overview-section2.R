# Basic MPA Statistics
# 10 Aug 2022
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
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
# Drop geometry for simple pieces
area <- area_sf %>% sf::st_drop_geometry()

# New df for only the MPA areas that we found plans for
area_found <- area[area$search == "F",]

# New df only including stat (not text detail)
review_stat <- review[review$type == "stat",]

# Widen stats (every MP is a row)
review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id)


# Stats for Manuscript ---------------------------------------------------------

## General areas and plans included ----
area %>% 
  group_by() %>% 
  summarize(n_areas_search = n_distinct(mpa_id), # areas meeting critera
            n_plans = n_distinct(area$plan_id[area$search == "F"]), # num plans included
            n_areas_w_plans = n_distinct(area$mpa_id[area$search == "F"])) # areas covered by plans

## Number of plans with multiple areas ----
area_found  %>% 
  group_by(plan_id) %>% 
  summarize(n_areas = n_distinct(mpa_id)) %>% 
  filter(n_areas > 1) %>% 
  group_by() %>% 
  summarize(n_multiple_areas = n(), # n plans with multiple areas
            max_areas = max(n_areas)) # max n areas in one plan

## Geography ----
area_found %>%
  group_by() %>%
  summarize(n_countries = n_distinct(country),
            earliest_year = min(mp_year, na.rm = T),
            average_year = mean(mp_year, na.rm = T),
            pct_last_decade = n_distinct(plan_id[mp_year > 2009])/n_distinct(plan_id))


## Major ocean basins (region) ----
area_found %>%
  group_by(region) %>%
  summarize(n_zones = n(),
            n_plans = n_distinct(plan_id)) 

## Major subregions ----
# area_found %>%
#   group_by(region, subregion) %>%
#   summarize(n_zones = n(),
#             n_plans = n_distinct(plan_id)) %>% print(n = 33)


## Languages ----
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
    data_row.padding = px(3))

## Size Classes ----------------------------------------------------------------

### Add size class ----
area_calc <- area_sf %>%
  filter(mpa_id %in% area_found$mpa_id) %>%
  st_make_valid() %>%
  st_area() %>%
  as.numeric() %>%
  measurements::conv_unit(., "m2", "km2")

area_stats <- area_found %>%
  cbind(area_calc) %>%
  mutate(size_class = factor(case_when(area_calc > 100000 ~ "Very Large",
                                       between(area_calc, 100, 100000) ~ "Large",
                                       between(area_calc, 10, 100) ~ "Small",
                                       area_calc < 10 ~ "Very Small"),
                             levels = c("Very Small", "Small",
                                        "Large","Very Large"))) 
### Number of plans in each size class ----
area_stats %>% 
  group_by(plan_id) %>% 
  summarize(area_calc = sum(area_calc)) %>% 
  ungroup() %>% 
  mutate(size_class = factor(case_when(area_calc > 100000 ~ "Very Large",
                                       between(area_calc, 100, 100000) ~ "Large",
                                       between(area_calc, 10, 100) ~ "Small",
                                       area_calc < 10 ~ "Very Small"),
                             levels = c("Very Small", "Small",
                                        "Large","Very Large")))%>% 
  group_by(size_class) %>% 
  summarize(n = n())


### Total area covered by review ----
area_stats %>% 
  group_by(plan_id) %>% 
  summarize(total_plan_area = sum(area_calc)) %>% 
  group_by() %>% 
  summarize(total_revew_area = sum(total_plan_area))


