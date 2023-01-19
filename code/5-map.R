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
library(maps)
library(ggExtra)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-geometry.Rds"))
area    <- readRDS(file.path(data.dir, "processed-area-metadata.Rds")) %>% 
  select(mpa_id, plan_id)
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

# Get lat/long from geometry
mpa_pts <- mpa_centroids %>% 
  st_coordinates() %>% 
  as.data.frame()

# Bind to a single data frame
mpa_df <- mpa_centroids %>% 
  cbind(mpa_pts) %>% 
  left_join(., area, by = "mpa_id") %>% 
  left_join(., climate_mention, by = "plan_id")

# Create axes labels
lat_labels <- c("60°S", "30°S", "0°", "30°N", "60°N")
lon_labels <- c("180°W", "150°W","120°W","90°W","60°W","30°W", "0°", "30°E", "60°E", "90°E", "120°E", "150°E","180°E")

# Map (tmap) ------------------------------------------------------------------------

# mpa_map <- 
#   # Graticules provide reference for oceanic areas
#   tm_graticules(col = "grey",
#                 alpha = 0.4) +
#   
#   # World map with country borders provide reference for coastal areas
#   tm_shape(world) + 
#   tm_fill("grey85") + 
#   tm_borders("grey95", lwd = 1) +
#   
#   # Add MPA data 
#   tm_shape(mpa_df) + 
#   tm_dots(col = "climate_mention", # dot color refers to climate change variable
#           title = "", 
#           size = 0.15,
#           alpha = 0.4, # semi-transparent to better distinguish overlap
#           palette = c(Yes = "#1E88E5", # colorblind friendly red/blue
#                       No = "#D81B60"),
#           colorNA = "#818589", # NULL for transparent
#           textNA = "No plan located",
#           labels = c("Plan does not mention climate change", # legend labels
#                      "Plan mentions climate change")) +
#   
#   # Format Legend
#   tm_legend(position = c(0.03, 0.15), # bottom left
#             bg.color = "white",
#             bg.alpha = 0.5, # slightly transparent background
#             frame = TRUE,
#             frame.lwd = 0.7) 
#   
# # Add and format title
#   #tm_layout(main.title = "Climate change language in marine protected area management plans",
#   #          main.title.position = c("center", "top"), 
#   #          main.title.size = 1.3)
# 
# mpa_map
# 
# t1 <- tmap_grob(mpa_map)
# t1
# 
# # Plot lat density
# g2 <- ggplot(mpa_df,  aes(y=Y, color=climate_mention)) +
#   geom_density(alpha=0.3) +
#   scale_color_manual(values = c("#D81B60", "#1E88E5")) +
#   scale_y_continuous(limits = c(-90, 90), expand = c(0,0), breaks = c(-60, -30, 0, 30, 60))+
#   # Limits
#   theme(legend.position = "none")
# 
# 
# g <- gridExtra::grid.arrange(t1, g2, ncol=2, heights = c(1, 0.9), 
#                              widths=c(0.9, 0.1))
# g


#tmap_save(mpa_map, file.path("figs", "Fig1_Map.png"))

### --------------------------------------------------------------------------------------
# Get world basemap
world_data <- map_data("world")

# Replace NAs with "plan not located"
mpa_df$climate_mention[is.na(mpa_df$climate_mention)] <- "Plan not located"

mpa_df <- mpa_df %>% 
  mutate(climate_mention = factor(climate_mention,
                                levels = c("No", "Yes", "No plan located")))

# Build map 
g1 <- ggplot() +
  # Basemap with dark countries and white outlines
  geom_polygon(data = world_data,
               aes(x = long, y = lat, group = group),
               fill = "grey85", color = "grey95", lwd = 0.1) +
  # Points for each MPA
  geom_point(data = mpa_df,
             aes(x = X, y = Y, col = climate_mention),
          alpha = 0.4, size = 2) + 
  # Colors with colorblind friendly red/blue
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#818589"),
                     labels = c("Plan does not mention climate change", # legend labels
                                "Plan mentions climate change",
                                "No plan located")) +
  # Set axes
  scale_x_continuous(limits = c(-185, 185), expand = c(0,0),
                     breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180),
                     labels = lon_labels)+
  scale_y_continuous(limits = c(-89, 89), expand = c(0,0),
                     breaks = c(-60, -30, 0, 30, 60),
                     labels = lat_labels) +
  # Formatting
  labs(color = NULL,
       x = NULL,
       y = NULL) +
  theme_bw()+ 
  theme(legend.position = c(0.15, 0.1),
        legend.text=element_text(size = 8),
        legend.key.size = unit(0.25, "cm"),
        legend.margin = margin(0,5,5,2),
        legend.background = element_rect(fill = alpha("white",0.4),
                                         color = "black")) 

g1

# Build density plot for marginal plot
g2 <- ggplot(mpa_df,  aes(y = Y, fill = climate_mention, 
                          color = climate_mention)) +
  geom_density(alpha=0.3) +
  scale_fill_manual(values = c("#D81B60", "#1E88E5", "#818589")) +
  scale_color_manual(values = c("#D81B60", "#1E88E5", "#818589")) +
  
  # Labels
  labs(x=NULL,
       y = NULL) +
  # Limits
  scale_y_continuous(limits = c(-89, 89), expand = c(0.01,0.01),
                     breaks = c(-60, -30, 0, 30, 60))+
  # Theme
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_blank())
g2


library(cowplot)
plot_grid(g1, g2, ncol = 2, align = "h", rel_widths = c(0.9, 0.1))
ggsave(file.path("figs", "Fig1_Map_with_Density.png"),
       width = 9, height = 4, dpi = 300)

# grob1 <- ggplotGrob(g1)
# grob2 <- ggplotGrob(g2)
# grid::grid.newpage()
# grid::grid.draw(cbind(grob1, grob2), widths = c(0.9, 0.1))
# 
# 
# grob2.widths <- grob2$widths[1:3]
# grob2$widths[1:3] <- grob2.widths*0.1
# 
# g <- cbind(grob1, grob2, size = "first")
# g$heights <- grid::unit.pmax(grob1$heights, grob2$heights)
# 
# 
# gridExtra::grid.arrange(grob1, grob2, widths = c(0.9, 0.1))
# 
# grid::grid.newpage()
# grid::grid.draw(g)
