# Create Figure 1. Map of climate language
# 10 Aug 2022
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(sf)
library(tmap)
library(spData)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))


# Build Data -------------------------------------------------------------------
# Get plan stats
climate_mention <- review %>% 
  as.data.frame() %>% 
  filter(q_code == "climate_mention" & type == "stat") %>% 
  select(plan_id, entry) %>% 
  mutate(climate_mention = case_when(entry == "1" ~ "Yes",
                                     entry == "0" ~ "No"))


# Settings to avoid using spherical geometry package when converting
sf_use_s2(FALSE)

# Convert multipolygons to centroids
mpa_centroids <- area_sf %>% 
  st_make_valid() %>% 
  st_centroid() 

mpa_df <- mpa_centroids %>% 
  left_join(., climate_mention, by = "plan_id") 


# Map  ------------------------------------------------------------------------

mpa_map <- 
  # Graticules provide reference for oceanic areas
  tm_graticules(col = "grey",
                alpha = 0.4) +
  
  # World map with country borders provide reference for coastal areas
  tm_shape(world) + 
  tm_fill("grey85") + 
  tm_borders("grey95", lwd = 1) +
  
  # Add MPA data 
  tm_shape(mpa_df) + 
  tm_dots(col = "climate_mention", # dot color refers to climate change variable
          title = "", 
          size = 0.15,
          alpha = 0.4, # semi-transparent to better distinguish overlap
          palette = c(Yes = "#1E88E5", # colorblind friendly red/blue
                      No = "#D81B60"),
          colorNA = "#818589", # NULL for transparent
          textNA = "No plan located",
          labels = c("Plan does not mention climate change", # legend labels
                     "Plan mentions climate change")) +
  
  # Format Legend
  tm_legend(position = c(0.03, 0.15), # bottom left
            bg.color = "white",
            bg.alpha = 0.5, # slightly transparent background
            frame = TRUE,
            frame.lwd = 0.7) 
  
  # Add and format title
  #tm_layout(main.title = "Climate change language in marine protected area management plans",
  #          main.title.position = c("center", "top"), 
  #          main.title.size = 1.3)

mpa_map
tmap_save(mpa_map, file.path("figs", "Fig1_Map.png"))

