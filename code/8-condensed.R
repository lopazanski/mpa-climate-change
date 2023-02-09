# Build Condensed Data
# "Awareness-Action-Climate"
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(cowplot)

# Directories
data.dir <- file.path("data", "processed")


# Read Data -------------------------------------------------------------------
all_plan      <- readRDS(file.path(data.dir, "all_stat_by_plan.Rds"))
plan_names <- readRDS(file.path("data", "raw-ish", "exported-plan-names.Rds"))
plan_region   <- readRDS(file.path(data.dir, "processed-area-metadata.Rds")) %>% 
  select(plan_id, region) %>% distinct() %>% 
  filter(plan_id %in% all_plan$plan_id)


# Build Data -------------------------------------------------------------------
# Format names
names <- plan_names %>% 
  select(plan_id, name_english) %>% 
  mutate(name_abbrev = str_replace(name_english, "Marine Protected Area", "MPA")) %>% 
  mutate(name_abbrev = str_replace(name_abbrev, "Marine Reserve", "MR")) %>% 
  mutate(name_abbrev = str_replace(name_abbrev, "National Park", "NP")) %>% 
  mutate(name_abbrev = str_replace(name_abbrev, "Marine Park", "MP")) %>% 
  mutate(name_abbrev = str_replace(name_abbrev, "Biosphere Reserve", "BR")) %>% 
  mutate(name_abbrev = str_replace(name_abbrev, "National Marine Sanctuary", "NMS"))
  
# Test region ratio
region_count <- plan_region %>% 
  group_by(region) %>% 
  count()
# 94 in left plot, 78 in right plot

# Classify level for each plan 
data <- all_plan %>% 
  group_by(plan_id, category, type) %>% 
  # Sum the total number of 1's for each indicator
  summarize(score = sum(entry, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = type, values_from = score) %>% 
  janitor::clean_names() %>% 
  # Factor for the level of inclusion based on nonzero score
  mutate(level = factor(case_when(climate_action > 0 ~ "Climate Action",
                                  recommended_action > 0 ~ "Recommended Action",
                                  general_awareness > 0 ~ "General Awareness"),
                        levels = c("General Awareness", "Recommended Action", "Climate Action", "Not Included")))

data_wide <- data %>% 
  select(plan_id, category, level) %>% 
  pivot_wider(names_from = category, values_from = level)



scores <- data %>% 
  group_by(plan_id) %>%
  # Total score for each level of inclusion
  summarize(total_climate = sum(climate_action, na.rm = T),
            total_rec = sum(recommended_action, na.rm = T),
            total_aware = sum(general_awareness, na.rm = T))

level_counts <- data %>% 
  group_by(plan_id) %>% 
  count(level) %>% 
  mutate(level_name = case_when(level == "Climate Action" ~ "climate_count",
                                level == "Recommended Action" ~ "rec_count",
                                level == "General Awareness" ~ "aware_count",
                                is.na(level) ~ "not_included")) %>% 
  rename(level_counts = n) %>% 
  select(plan_id, level_name, level_counts) %>% 
  pivot_wider(names_from = level_name, values_from = level_counts)
  
level_counts[is.na(level_counts)] <- 0

data3 <- data %>% 
  left_join(., names) %>% 
  left_join(., plan_region) %>% 
  left_join(., scores) %>% 
  left_join(., level_counts) %>% 
  mutate(name_ordered = factor(name_abbrev, 
                               levels = unique(name_abbrev[order(climate_count, rec_count, aware_count, total_climate, total_rec)]), 
                               ordered = T))
  
  #mutate(name_ordered = reorder(name_abbrev, level_counts))

data3$level[is.na(data3$level)] <- "Not Included"

# Plot -------------------------------------------------------------------
theme1 <- theme(axis.text.y = element_text(size = 6),
                axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust=1),
                axis.title  = element_blank(),
                axis.line.x = element_line(size = 0.5),
                # Legend
                legend.title = element_blank(),
                legend.text = element_text(size = 9),
                legend.key.size = unit(0.4, "cm"),
                legend.spacing.x = unit(0.1, "cm"),
                legend.background = element_rect(color = "black", size = 0.25),
                strip.text = element_text(face = "bold", size = 8),
                plot.title  = element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
# Legend
#legend.background = element_rect(fill=alpha('blue', 0)))
g1 <- ggplot(data = data3 %>% 
               filter(region %in% c("Atlantic", "Indian", "Mediterranean"))) +
  geom_tile(aes(x = category, y = name_ordered, fill = level),
            show.legend = F) +
  scale_fill_manual(values = c("#c9daf8", "#6d9eeb", "#1155CC", "white"),
                    na.translate = F) +
  ggh4x::facet_nested(region~., scales = "free", space = "free") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme1 +
  theme(plot.margin = margin(5, 5, 5, 5))

g1

g2 <- ggplot(data = data3 %>% 
               filter(!(region %in% c("Atlantic", "Indian", "Mediterranean")))) +
  geom_tile(aes(x = category, y = name_ordered, fill = level)) +
  scale_fill_manual(values = c("#c9daf8", "#6d9eeb", "#1155CC", "white"),
                    na.translate = T,) +
  ggh4x::facet_nested(region~., scales = "free", space = "free") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))  +
  theme1 +
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.margin = margin(0, 5, 5, 5),
        legend.key = element_rect(colour = "grey20"),
        legend.box.margin = margin(5, 130, -5,-10),
        plot.margin = margin(5, 5, 5, 5))


g2

plot_grid(g1, g2, ncol = 2, align = "h", axis = "b")
ggsave(file.path("figs", "Fig5_Stat_by_MPA.png"), width = 6.5, height = 8, dpi = 600)


# Read Data -------------------------------------------------------------------
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds")) 
plan_region <- readRDS(file.path("data", "plan-region-id.Rds"))

# Build Data -------------------------------------------------------------------
# Stat only df (no text detail)
review_stat <- review[review$type == "stat",]

# Replace NAs for design type with zeroes
review_stat$entry[review_stat$q_code == "design_type" & is.na(review_stat$entry)] <- 0

# Replace "planned" values with ones
review_stat <- review_stat %>% 
  mutate(entry = if_else(entry %in% c("Planned", 1), 1, 0))

# Widen stat df (each plan is a row)
review_stat_wide <- review_stat %>%
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) 

# Create df with plan and name
names <- review %>% 
  select(plan_id, name) %>% 
  distinct() %>% 
  mutate(name_short = gsub("\\s*\\([^\\)]+\\)", "", name)) %>% 
  full_join(plan_region, by = "plan_id")

# Create combined columns  -----------------------------------------------------
# Create combined threat awareness, strategy, climate change columns
# Binary for any of the listed threats, strategies, climate applications
new_threat <- review_stat_wide %>% 
  mutate(threat_any_combined = if_else(if_any(c(threat_any, threat_tourism, threat_compliance, 
                                                threat_invasive, threat_pollution, threat_habitat)), 1, 0),
         threat_strat_combined = if_else(if_any(c(threat_strat, threat_tourism_strat, threat_compliance_strat, 
                                                  threat_invasive_strat, threat_pollution_strat,  threat_habitat_strat)), 1, 0),
         threat_cc_combined = if_else(if_any(c(threat_cc,  threat_tourism_cc, threat_compliance_cc, 
                                               threat_invasive_cc, threat_pollution_cc, threat_habitat_cc)), 1, 0)) %>% 
  select(plan_id, threat_any_combined, threat_strat_combined, threat_cc_combined) %>% 
  pivot_longer(cols = 2:4, names_to = "q_code", values_to = "entry") %>% 
  mutate(category = factor("management", levels = c("metadata", "climate", "objectives","assessment",
                                                    "design", "adaptive design", "monitoring", "management")))

new_threat$category[new_threat$q_code == "threat_any_combined"] <- "assessment"

# Create combined assessment column (cc or otherwise):
new_assess <- review_stat_wide %>% 
  mutate(entry = if_else(if_any(c(assess_any, assess_climate)), 1, 0)) %>% 
  mutate(q_code = "assess_any_combined",
         category = factor("assessment", levels = c("metadata", "climate", "objectives","assessment",
                                                    "design", "adaptive design", "monitoring", "management"))) %>% 
  select(plan_id, category, q_code, entry)

# All plans have at least one management objective (default)
new_mgmt <- review_stat_wide %>% 
  select(plan_id) %>% 
  mutate(category = factor("objectives"),
         q_code = "obj_mgmt_any",
         entry = 1)

# Add new columns to review
all <- review_stat %>% 
  select(plan_id, category, q_code, entry) %>% 
  filter(!(category == "metadata"))  %>% 
  rbind(new_threat) %>% 
  rbind(new_assess) %>% 
  rbind(new_mgmt)

# Define Condensed Variables ---------------------------------------------------

top_vars <- c("climate_plan", "climate_action", "climate_mention",
              "obj_mgmt_any", "obj_cc_any", "obj_lt_any",
              "threat_any_combined", "assess_any_combined", "assess_climate",
              "design_adaptive_climate", "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
              "mgmt_adapt", "design_cc_any", "design_resilience", "design_any",
              "monitor_any", "monitor_sci", "monitor_soc", "monitor_exp_any",
              "threat_cc_combined", "threat_strat_combined")


# Define Awareness/Action/Climate Levels ---------------------------------------

# Climate-level variables
climate_vars <- c("climate_plan", "climate_action", 
                  "obj_cc_any", "assess_climate","design_adaptive_climate",
                  "design_cc_any","monitor_exp_any", "threat_cc_combined")

action_vars <- c("obj_lt_any", "assess_any_combined", "design_resilience",
                 "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
                 "monitor_sci", "monitor_soc", "threat_strat_combined")

aware_vars <- c("climate_mention","obj_mgmt_any", "design_any", "monitor_any", "mgmt_adapt", "threat_any_combined")


condensed <- all %>% 
  filter(q_code %in% top_vars) %>%
  # Drop the social monitoring and adaptive design types
  filter(!(q_code %in% c("monitor_soc", "design_adaptive_zoning", 
                         "design_adaptive_fully"))) %>% 
  mutate(level = case_when(q_code %in% climate_vars ~ "Climate Action",
                           q_code %in% action_vars ~ "Recommended Action",
                           q_code %in% aware_vars ~ "General Management Awareness")) %>% 
  mutate(level = factor(level, levels = c("General Management Awareness", "Recommended Action", "Climate Action")))

condensed$category[condensed$category == "adaptive design"] <- "design"


condensed_summary <- condensed %>% 
  group_by(plan_id, category) %>% 
  summarize(total = sum(entry)) %>% 
  pivot_wider(id_cols = plan_id,
              names_from = category, 
              values_from = total) %>% 
  ungroup() %>% 
  #mutate(assessment = if_else(assessment > 0, assessment + 1, 0)) %>% 
  #mutate(`adaptive design` = if_else(`adaptive design` > 0, `adaptive design` + 1, 0)) %>% 
  full_join(., names, by = "plan_id") %>% 
  select(-plan_id, -name) %>% 
  mutate(total = rowSums(across(climate:management))) %>% 
  mutate(name_short = reorder(name_short, total)) %>% 
  pivot_longer(cols = climate:management,
               names_to = "indicator",
               values_to = "entry") %>%
  filter(!(entry == 0)) %>% 
  mutate(entry = if_else(entry > 3, 3, entry)) %>% 
  mutate(entry = as.factor(entry)) %>% 
  mutate(indicator = factor(indicator, 
                            levels = c("climate","objectives", "assessment", "design", "monitoring", "management")))


# Plot -------
# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.text.y=element_text(size=4.5),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                axis.title=element_blank(),
                legend.text=element_text(size=6),
                legend.title=element_text(size=8),
                strip.text=element_text(size=4),
                plot.title=element_blank(),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
                # Legend
                #legend.background = element_rect(fill=alpha('blue', 0)))
ggplot() +
  geom_tile(data = condensed_summary,
            aes(x = indicator, y = name_short, fill = entry), show.legend = F) +
  scale_fill_manual(values = c("#c9daf8", "#6d9eeb", "#1155CC")) +
  ggh4x::facet_nested(region~., scales = "free", space = "free") +
  #labs(fill = "1 - awareness\n2 - action\n3 - climate action") +
  theme1

#ggsave("pacific_mpas.png", width = 6, height = 4, dpi = 300)

