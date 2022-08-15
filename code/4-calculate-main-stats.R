# Basic MPA Statistics
# 10 Aug 2022
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
area <- area_sf %>% sf::st_drop_geometry()
area_found <- area[area$search == "F",]
review_stat <- review[review$type == "stat",]

# Stats for Manuscript ---------------------------------------------------------
# General areas and plans included
area %>% 
  group_by() %>% 
  summarize(n_areas_search = n_distinct(mpa_id), # areas meeting critera
            n_plans = n_distinct(plan_id), # num plans included
            n_areas_w_plans = n_distinct(area$mpa_id[area$search == "F"])) # areas covered by plans

# Number of areas for each plan
area  %>% 
  filter(search == "F") %>% 
  group_by(plan_id) %>% 
  summarize(n_areas = n_distinct(mpa_id)) %>% 
  filter(n_areas > 1) %>% 
  group_by() %>% 
  summarize(n_multiple_areas = n(), # n plans with multiple areas
            max_areas = max(n_areas)) # max n areas in one plan

# Geography 
area %>%
  filter(search == "F") %>%
  group_by() %>%
  summarize(n_countries = n_distinct(country),
            earliest_year = min(mp_year, na.rm = T),
            average_year = mean(mp_year, na.rm = T),
            n_last_decade = n_distinct(plan_id[mp_year > 2009])/n_distinct(plan_id))


# Major ocean basins (region)
area_found %>%
  group_by(region) %>%
  summarize(n_zones = n(),
            n_plans = n_distinct(plan_id))

# Stats for each country
# country_stats <- area_found %>%
#   group_by(country) %>%
#   summarize(n_areas = n(),
#             n_plans = n_distinct(plan_id))

# Size Classes -----------------------------------------------------------------

# Add size class
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

ggplot() +
  geom_bar(data = area_stats,
           aes(x = size_class)) +
  labs(x = "MPA size class",
       y = "number of zones")

# Climate Change ---------------------------------------------------------------

review_stat %>% 
  filter(category == "climate") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

test <- review_stat %>% 
  filter(category == "climate") %>% 
  select(plan_id, q_code, entry) %>% 
  pivot_wider(names_from = q_code, values_from = entry) %>% 
  filter(climate_mention == "1" &
           climate_action == "0" &
           climate_plan == "0")
  group_by() %>% 
  summarize(n_only_mention = n_distinct(plan_id))
