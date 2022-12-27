# Process MPA Search List
# 29 June 2022
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)

# Directories
drive.dir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/MPA Climate Change - Conservation International/mpa-climate-change-review"
atlas.dir <- file.path(drive.dir, "atlas-data", "mpatlas_20201223_clean")
data.dir <- file.path("data", "raw-ish")
  # Called "raw-ish" because some very minimal things are done here.
  # Deep processing should occur in individual scripts (aka this one).
out.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
## MPA Atlas full data
atlas <- read_sf(dsn = atlas.dir,
                 layer = "mpatlas_20201223_clean")

## MPA Atlas search list metadata (all entries except geometry)
## Includes main three criteria and MPAs for 11 excluded plans
## Created in 0-process-atlas-overlap.R
atlas_list <- readRDS(file.path("data", "processed", "atlas_search_list.Rds"))

## Completed search information (all entries that were searched during all 
## project stages, therefore includes areas that are now excluded)
search <- readRDS(file.path(data.dir, "exported-mpa-search-list.Rds")) %>% 
  rename(name_search = name) %>% 
  rename(network_from_mp = network)


## Plan metadata
plan_data <- readRDS(file.path(data.dir, "exported-plan-metadata.Rds")) %>% 
  rename(name_plan = name,
         country_plan = country)

## Processed document review
review  <- readRDS(file.path("data", "processed", "processed-doc-review.Rds"))


# Build Data -------------------------------------------------------------------
## Filter atlas data for those in our search
atlas_sf

## Join atlas data with our search information
atlas_search <- left_join(atlas_list, search)

## Join atlas/search combo with plan metadata
atlas_all <- left_join(atlas_search, plan_data, by = "plan_id")

# Changes to Data --------------------------------------------------------------

# Update F-AI (found, already included) to just "found"
atlas_all$search[atlas_all$search == "F-AI"] = "F"

# Convert any plans removed from the processed document review as "NI"
data <- atlas_all %>% 
  mutate(search = if_else(!(plan_id %in% review$plan_id) & !(is.na(plan_id)), 
                          "NI", search))

# Export -----------------------------------------------------------------------
saveRDS(data %>% sf::st_drop_geometry(), file.path(out.dir, "processed-area-metadata.Rds"))

# # Get geometry for those in the search
# atlas_all_geo <- atlas %>% 
#   filter(mpa_id %in% data$mpa_id) %>% 
#   select(mpa_id)
# 
# saveRDS(atlas_all_geo, file.path(out.dir, "processed-area-geometry.Rds"))


