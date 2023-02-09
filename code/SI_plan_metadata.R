# Create main supplementary table with stats for each plan
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-geometry.Rds"))
area <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))
all_wide <- readRDS(file.path(data.dir, "all-stat-wide.Rds"))

# Build Data -------------------------------------------------------------------
# Any NAs in "all_wide" to zeroes
all_wide[is.na(all_wide)] <- 0
# New df for only the MPA areas that we found plans for
area_found <- area[area$search == "F",]

# New df only including stat (not text detail)
review_stat <- review[review$type == "stat",]

# Widen stats (every MP is a row)
review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id)

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

## Total area for each plan ----
plan_area <- area_stats %>% 
  group_by(plan_id) %>% 
  summarize(total_plan_area = sum(area_calc))

## Number of areas for each plan ----
plan_mpas <- area_found  %>% 
  group_by(plan_id) %>% 
  summarize(n_areas = n_distinct(mpa_id)) 

## Extract region for each plan ----
plan_region <- area_found %>% 
  select(plan_id, region) %>% 
  distinct()
#saveRDS(plan_region, file.path("data", "plan-region-id.Rds"))

# Make Table -------------------------------------------------------------------
plan_table <- area_found %>% 
  select(plan_id, name_plan, country_plan, mp_year) %>% 
  distinct() %>% 
  full_join(., plan_region) %>% 
  full_join(., plan_mpas) %>% 
  full_join(., plan_area) %>% 
  #full_join(., all_wide) %>% 
  arrange(plan_id)

plan_table %>% 
  gt() %>% 
  tab_header(title = md("**Table X.** Information for management plans included in review")) %>% 
  cols_label(plan_id = "Plan ID",
             name_plan = "Name",
             country_plan = "Country",
             mp_year = "Publication Year",
             region = "Region",
             n_areas = "# MPAs",
             total_plan_area = md("Total Managed Area (km<sup>2</sup>)")) %>% 
  #cols_align(columns = 2:3, "center") %>% 
  opt_table_font(font = list(google_font("Arial"))) %>% 
  tab_options(
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    column_labels.border.top.width = px(3),
    heading.align = "left",
    data_row.padding = px(3)) %>% 
  gtsave(., filename = file.path("figs", "SI_TabX_plans.docx"))

# Full Results
plan_all <- area_found %>% 
  select(plan_id, name_plan, country_plan, mp_year) %>% 
  distinct() %>% 
  full_join(., plan_region) %>% 
  full_join(., plan_mpas) %>% 
  full_join(., plan_area) %>% 
  full_join(., all_wide) %>% 
  arrange(plan_id)

write.csv(plan_all, file.path("figs", "SI_TabX_plans_all.csv"))
