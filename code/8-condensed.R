# Build Condensed Data
# "Awareness-Action-Climate"
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)


# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds")) 
plan_region <- readRDS(file.path("data", "plan-region-id.Rds"))

# Build Data -------------------------------------------------------------------
review_stat <- review[review$type == "stat",]
review_stat$entry[review_stat$q_code == "design_type" & is.na(review_stat$entry)] <- 0

review_stat <- review_stat %>% 
  mutate(entry = if_else(entry %in% c("Planned", 1), 1, 0))

review_stat_wide <- review_stat %>%
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) 

vis_dat(review_stat_wide)

# Plan ID & Name Dataframe
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
              "assess_any_combined", "assess_climate",
              "design_adaptive_climate", "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
              "mgmt_adapt", "design_cc_any", "design_resilience", "design_any",
              "monitor_any", "monitor_sci", "monitor_soc", "monitor_exp_any",
              "threat_cc_combined", "threat_strat_combined", "threat_any_combined")


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
                         "design_adaptive_fully", "threat_any_combined"))) %>% 
  mutate(level = case_when(q_code %in% climate_vars ~ "Climate Action",
                           q_code %in% action_vars ~ "Recommended Action",
                           q_code %in% aware_vars ~ "General Management Awareness")) %>% 
  mutate(level = factor(level, levels = c("General Management Awareness", "Recommended Action", "Climate Action")))

condensed_summary <- condensed %>% 
  group_by(plan_id, category) %>% 
  summarize(total = sum(entry)) %>% 
  pivot_wider(id_cols = plan_id,
              names_from = category, 
              values_from = total) %>% 
  ungroup() %>% 
  mutate(assessment = if_else(assessment > 0, assessment + 1, 0)) %>% 
  mutate(`adaptive design` = if_else(`adaptive design` > 0, `adaptive design` + 1, 0)) %>% 
  full_join(., names, by = "plan_id") %>% 
  select(-plan_id, -name) %>% 
  mutate(total = rowSums(across(climate:management))) %>% 
  mutate(name_short = reorder(name_short, total)) %>% 
  pivot_longer(cols = climate:management,
               names_to = "indicator",
               values_to = "entry") %>%
  filter(!(entry == 0)) %>% 
  mutate(entry = as.factor(entry)) %>% 
  mutate(indicator = factor(indicator, 
                            levels = c("climate","objectives", "assessment", "design", 
                                                  "adaptive design", "monitoring", "management")))


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
            aes(x = indicator, y = name_short, fill = entry)) +
  scale_fill_manual(values = c("#9ECAE1", "#4292C6", "#08519C")) +
  ggh4x::facet_nested(region~., scales = "free", space = "free") +
  labs(fill = "1 - awareness\n2 - action\n3 - climate action") +
  theme1

