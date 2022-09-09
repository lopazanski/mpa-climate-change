# Read and Export MPA Review Data from Drive
# Cori Lopazanski
# 24 June 2022

# Setup  ---------------------------------------------------------------------------

# Packages
library(tidyverse)
library(googlesheets4)

# Clear workspace
rm(list = ls())

# Directories
drive.dir <- "/Volumes/GoogleDrive/My Drive/Research/MPA Climate Change - Conservation International/mpa-climate-change-review"
data.dir <- here::here("data", "raw-ish")
      # Called "raw-ish" because some very minimal things are done here.
      # Deep processing should occur in individual scripts.
  
# MPA Plan Search Information -----------------------------------------------

## Read MPA List Data ----
orig_list <- read_sheet("https://docs.google.com/spreadsheets/d/1kARXHqNtNaDU1E3UxkspOThLWDzuIKaSHqjtFjcBH30/edit#gid=930877353",
                        sheet = "mpa-list", trim_ws = T, range = "A:F",
                        na = c("NA", ""),
                        col_types = "ncnccc")
extra_list <- read_sheet("https://docs.google.com/spreadsheets/d/1kARXHqNtNaDU1E3UxkspOThLWDzuIKaSHqjtFjcBH30/edit#gid=930877353",
                         sheet = "extra-op6", trim_ws = T, range = "A:F",
                         na = c("NA", ""),
                         col_types = "ncnccc")

## Make sure columns match ----
orig_simple <- orig_list %>% select(mpa_id, search, plan_id, notes, network, name)
extra_simple <- extra_list %>% select(mpa_id, search, plan_id, notes, network, name)

## Join Together ----
list <- rbind(orig_simple, extra_simple)

## Export as RDS ----
saveRDS(list, file = file.path(data.dir, "exported-mpa-search-list.Rds"))


# Plan Metadata -----------------------------------------------------------

## Read plan metadata sheet ----
plan_metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1lqxaoukY0-k0a5mu5-Zjx-dtHjnPIgYzYkIsnGCudFQ/edit#gid=452132598",
                       sheet = "mpa-info", trim_ws = T) 

## Simplify -----
plans <- plan_metadata %>% 
  select(plan_id:network)

## Export 
saveRDS(plans, file = file.path(data.dir, "exported-plan-metadata.Rds"))


# Data Collection -----------------------------------------------------------

## Read document review ----
review <- read_sheet("https://docs.google.com/spreadsheets/d/1lqxaoukY0-k0a5mu5-Zjx-dtHjnPIgYzYkIsnGCudFQ/edit#gid=452132598",
                     sheet = "additional-collection", 
                     trim_ws = T, na = c("NA", "NS", ""), col_types = "c",
                     .name_repair = "minimal")

review <- as.data.frame(review) 

## Export document review
saveRDS(review, file = file.path(data.dir, "exported-doc-review.Rds"))

