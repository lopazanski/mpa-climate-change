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

## MPA Atlas data
atlas <- read_sf(dsn = atlas.dir,
                 layer = "mpatlas_20201223_clean")

## Completed search information
search <- readRDS(file.path(data.dir, "exported-mpa-search-list.Rds")) %>% 
  as.data.frame()

## Plan metadata
plan_data <- readRDS(file.path(data.dir, "exported-plan-metadata.Rds")) %>% 
  rename(name_plan = name,
         country_plan = country)

## Processed document review
review  <- readRDS(file.path("data", "processed", "processed-doc-review.Rds"))

# Process Data -----------------------------------------------------------------
search_clean <- search %>% 
  rename(name_search = name) %>% 
  rename(network_from_mp = network)


# Build Data -------------------------------------------------------------------

## Filter atlas data for those in our search
atlas_subset <- atlas %>% 
  filter(mpa_id %in% search$mpa_id)
    # Note: There are 3 mpa_id's in the search that are no longer considered 
    # individual areas in MPA atlas with the updated version. This is because 
    # the initial search list was generated from an older version of the atlas. 
    # The 3 id's all correspond to GBR zones, which are represented by other 
    # mpa_id's and therefore okay to exclude.

## Join atlas data with our search information
atlas_search <- left_join(atlas_subset, search_clean)

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
saveRDS(data, file.path(out.dir, "processed-area-metadata.Rds"))
