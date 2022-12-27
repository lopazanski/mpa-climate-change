# Create MPA Management Plan Search List
# Cori Lopazanski

# About  -----------------------------------------------------------------------

# The MPA Atlas database was used to construct a list of fully-highly protected 
# areas from which to search for public-facing management plans. 

# In August 2021, we received a newer version of the database and learned (via 
# email with Beth Pike) that the "fully/highly" list from the upcoming paper
# Grorud-Colvert et al., 2021) is only those that are coded as "All" no-take. 
# Reviews of that list (1056 zones) indicate that it may be too restrictive for 
# our purposes; 54 of the already-reviewed management plans would be excluded.

# Instead we will use restrictions that the MPA is an MPA, it is implemented,
# and "Part" or "All" no take. These still leave ~ 11 management plans that are
# excluded, which we will identify in this script to append to the final search
# list. 

# Setup  -----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)

# Directories
drive.dir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/MPA Climate Change - Conservation International/mpa-climate-change-review"
atlas.dir <- file.path(drive.dir, "atlas-data")
data.dir <- file.path("data", "raw-ish")
  # Called "raw-ish" because some very minimal things are done here.
  # Deep processing should occur in individual scripts (aka this one).
out.dir <- file.path("data", "processed")

# Create Simple Atlas Data -----------------------------------------------------
# MPA Atlas downloaded from emlab Google Drive on April-07-2020 
  # (original version used for this project)
# mpa_atlas <- read_sf(dsn = file.path(atlas.dir, "MPAtlas_20200407"), 
#                      layer = "mpatlas_20200407_FullCalcs") %>% janitor::clean_names()
# 
# # Received updated version from Juan Mayorga that he cleaned, Dec-23-2020:
# # Confirmed with Beth Pike in Aug 2021 that this version is still pretty up-to-date
# mpa_atlas2 <- read_sf(dsn = file.path(atlas.dir ,"mpatlas_20201223_clean"),
#                       layer = "mpatlas_20201223_clean") %>% janitor::clean_names()
# 
# # Convert both versions to dataframes to manipulate without geometries
# atlas_df <- as.data.frame(mpa_atlas) %>% select(-geometry)
# atlas2_df <- as.data.frame(mpa_atlas2) %>% select(-geometry)

# Export both dataframes for easier future work
# saveRDS(atlas_df, file.path(data.dir, "atlas_original_df.Rds"))
# saveRDS(atlas2_df, file.path(data.dir,"atlas_updated_df.Rds"))

# Read Simple Atlas Data -------------------------------------------------------
# Read in the simplified RDS created above 
# (don't need geometries and files are large)
atlas_df <- readRDS(file.path(data.dir, "atlas_original_df.Rds"))
atlas2_df <- readRDS(file.path(data.dir,"atlas_updated_df.Rds"))

all <- data.frame(plan_id = c(1:175))
                    
# Read Ultimate Search List ----------------------------------------------------
search <- readRDS(file.path(data.dir, "exported-mpa-search-list.Rds"))

# Build ------------------------------------------------------------------------

# 1. Create search list of MPAs from MPAtlas ----
atlas_search <- atlas2_df %>% #20933
  filter(implemente == 1)%>%  # Implemented (step matches old version) 17793 
  filter(no_take %in% c("Part", "All")) %>%  #  No-take 1665
  filter(is_mpa == 1) # Is MPA #1592

# 2. Identify plans that are excluded from above criteria ----
# Which MPA-IDs are "extra" from my list (not in atlas search above)
extra_mpas <- search %>% 
  filter(!(mpa_id %in% atlas_search$mpa_id)) #121

# Which plans are "extra" from my list based on those missing MPA-IDs?
included_plans <- search %>% 
  # Only keep mpa-id's from those in our search
  filter(mpa_id %in% atlas_search$mpa_id) %>% 
  # Select plan id's
  select(plan_id) %>% 
  # Unique remaining plan IDs
  unique()

all <- data.frame(plan_id = c(1:175))
excluded_plans <- all %>% 
  filter(!(plan_id %in% included_plans$plan_id)) # 11 

# 3. Inspect excluded plans ----
# Extract MPA-IDs for the 11 excluded plans
excluded_mpas <- search %>% 
  filter(plan_id %in% excluded_plans$plan_id)

# Extract full metadata for the 11 excluded plans
atlas_excluded <- atlas2_df %>% 
  filter(mpa_id %in% excluded_mpas$mpa_id)

# 4. Append excluded areas to atlas search list ----
# Add rows to atlas search list
atlas_search_append <- atlas_search %>% 
  rbind(atlas_excluded)

# Filter my list for those in the atlas search
my_final <- search %>% 
  filter(mpa_id %in% atlas_search_append$mpa_id)


# 5. Export ----
saveRDS(atlas_search_append, file.path(out.dir, "atlas_search_list.Rds"))
