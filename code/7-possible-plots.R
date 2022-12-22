# 6 Figure Tests
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
#area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
## Create df with stats only ----
review_stat <- review[review$type == "stat",]

## Corrections ----
# Replace NAs in "design type" with zeroes
review_stat$entry[review_stat$q_code == "design_type" & is.na(review_stat$entry)] <- 0

# Change "planned" to 1
review_stat <- review_stat %>% 
  mutate(entry = if_else(entry %in% c("Planned", 1), 1, 0))

# Change climate mitigation to management
review_stat$category[review_stat$q_code == "climate_mitigation"] <- "management"

## Widen ----
review_stat_wide <- review_stat %>%
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) 


## Condensed stats ----
# Condense threats across all categories:
new_threat <- review_stat_wide %>% 
  mutate(
    # Any awareness of threats
    threat_any_combined = if_else(if_any(c(threat_any, threat_compliance, threat_habitat, 
                                           threat_tourism, threat_invasive, threat_pollution)), 1, 0),
    # Any strategies for threats
    threat_strat_combined = if_else(if_any(c(threat_strat, threat_compliance_strat, threat_habitat_strat, 
                                             threat_tourism_strat, threat_invasive_strat, threat_pollution_strat)), 1, 0),
    # Any climate strategies for threats
    threat_cc_combined = if_else(if_any(c(threat_cc, threat_compliance_cc, threat_habitat_cc,  
                                          threat_tourism_cc, threat_invasive_cc, threat_pollution_cc)), 1, 0)) %>% 
  select(plan_id, threat_any_combined, threat_strat_combined, threat_cc_combined) %>% 
  pivot_longer(cols = 2:4, names_to = "q_code", values_to = "entry") %>% 
  mutate(category = factor("management", levels = c("metadata", "climate", "objectives","assessment",
                                                    "design", "monitoring", "management")))

# Recategorize "awareness of threats" as assessment
new_threat$category[new_threat$q_code == "threat_any_combined"] <- "assessment"

# Condense assessment column (cc or otherwise):
new_assess <- review_stat_wide %>% 
  mutate(entry = if_else(if_any(c(assess_any, assess_climate)), 1, 0)) %>% 
  mutate(q_code = "assess_any_combined",
         category = factor("assessment", levels = c("metadata", "climate", "objectives","assessment",
                                                    "design", "monitoring", "management"))) %>%
  select(plan_id, category, q_code, entry)

# Plans with both species and unit level conservation objectives
new_obj <- review_stat_wide %>% 
  mutate(entry = if_else(if_all(c(obj_sp, obj_unit)), 1, 0)) %>% 
  mutate(q_code = "obj_both_sp_unit",
         category = factor("objectives", levels = c("metadata", "climate", "objectives","assessment",
                                                    "design", "monitoring", "management"))) %>%
  select(plan_id, category, q_code, entry)

# All plans have at least one management objective
new_mgmt <- review_stat_wide %>% 
  select(plan_id) %>% 
  mutate(category = factor("objectives"),
         q_code = "obj_mgmt_any",
         entry = 1)

# Add new columns to review
all <- review_stat %>% 
  select(plan_id, category, q_code, entry) %>% 
  filter(!(category == "metadata"))  %>% 
  rbind(new_obj) %>% 
  rbind(new_threat) %>% 
  rbind(new_assess) %>% 
  rbind(new_mgmt)

# Caclulate stats for all
all_stats <- all %>% 
  group_by(category, q_code, entry) %>% 
  summarize(n = n(),
            pct = round(n/171*100, 1)) %>% 
  filter(entry == 1)

# List of top variables for plot
top_vars <- c("climate_plan", "climate_action", "climate_mention",
              "obj_mgmt_any", "obj_any", "obj_both_sp_unit", "obj_lt_any", "obj_cc_any", 
              "assess_any_combined", "assess_climate","threat_any_combined",
              "design_adaptive_climate", "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
              "mgmt_adapt", "design_cc_any", "design_resilience", "design_any",
              "monitor_any", "monitor_sci", "monitor_soc", "monitor_exp_any",
              "threat_cc_combined", "threat_strat_combined")

# Climate-level variables
climate_vars <- c("climate_plan", "climate_action",
                  "obj_cc_any",  
                  "assess_climate",
                  "design_adaptive_climate","design_cc_any",
                  "monitor_exp_any", 
                  "threat_cc_combined", "climate_mitigation")

action_vars <- c("obj_lt_any", "obj_both_sp_unit",
                 "assess_any_combined", 
                 "design_resilience", "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
                 "monitor_sci", "monitor_soc", 
                 "threat_strat_combined")

aware_vars <- c("climate_mention",
                "obj_mgmt_any", "obj_any",
                "threat_any_combined",
                "design_any", 
                "monitor_any", 
                "mgmt_adapt")


top_stats <- all %>% 
  filter(q_code %in% c(climate_vars, action_vars, aware_vars)) %>%
  mutate(level = case_when(q_code %in% climate_vars ~ "Climate Action",
                           q_code %in% action_vars ~ "Recommended Action",
                           q_code %in% aware_vars ~ "General Awareness"))%>% 
  mutate(level = factor(level, levels = c("General Awareness", "Recommended Action", "Climate Action"))) %>% 
  mutate(category = str_to_sentence(category)) %>% 
  mutate(category = factor(category, levels = c("Metadata", "Climate", "Objectives","Assessment",
                                                "Design", "Monitoring", "Management"))) %>% 
  group_by(category, level, q_code, entry) %>% 
  summarize(n = n(),
            pct = round(n/171*100, 1)) %>% 
  ungroup() %>% 
  mutate(q_code_long = recode_factor(q_code,
                              # Climate 
                                "climate_plan" = "Detailed climate change action plan",
                                "climate_action" = "At least one strategy explicitly\nconnected to climate change",
                                "climate_mention" = "Mentions climate change",
                              # Objectives
                                "obj_cc_any" = "Explicit climate change objective",
                                "obj_both_sp_unit" = "Conservation objectives include both\nspecies-level and unit-level features",
                                "obj_lt_any" = "Management objectives target\nlong-term conservation",
                                "obj_any" = "Any conservation objective",
                                "obj_mgmt_any" = "Any management objective",
                              # Assessments
                                "assess_climate" = "Explicit climate change vulnerability\nassessment (completed or planned)",
                                "assess_any_combined" = "Detailed assessment evaluating threats \nor stressors (completed or planned)",
                                "threat_any_combined" = "Identified potential threat or stressor",
                              # Design
                                "design_cc_any" = "Design explicitly considers\nclimate change",
                                "design_adaptive_climate" = "Adaptive design applied with\nclimate considerations",
                                "design_adaptive_fully" = "Potential changes to external boundaries",
                                "design_adaptive_zoning" = "Potential changes to internal zoning",
                                "design_adaptive_any" = "Any adaptive design (flexible\nzoning or boundaries)",
                                "design_resilience" = "Design criteria includes at least one\ngeneral resilience principle",
                                "design_any" = "Provides design criteria details",
                              # Monitoring
                                "monitor_exp_any" = "Explicit climate change monitoring",
                                "monitor_soc" = "Socioeconomic monitoring",
                                "monitor_sci" = "Scientific monitoring",
                                "monitor_any" = "Any objectives for monitoring",
                              # Management
                                "climate_mitigation" = "Additional strategies for climate mitigation",
                                "threat_cc_combined" = "Strategy to address interacting stressor\nexplicitly connected to climate change",
                                "threat_strat_combined" = "Strategy to address interacting stressor",
                                "mgmt_adapt" = "Plan discusses adaptive management"))

# Plot Data -------------------------------------------------------------------

## All ----
ggplot(data = top_stats %>% filter(entry == 1),
       aes(x = pct, y = q_code_long)) +
  geom_col(aes(fill = level)) +
  geom_text(aes(label = pct, color = level), 
            hjust = 1.15, size = 3, show.legend = F) +
  #facet_wrap(~category, scales = "free", ncol = 1) +
  #facet_grid(rows = "category", scales = "free", space = "free") +
  ggforce::facet_col(vars(category), scales = "free_y", space = "free", strip.position = "top") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_manual(values = c("#c9daf8", "#6d9eeb", "#1155CC")) +
  scale_color_manual(values = c("grey35", "white", "white")) +
  labs(x = "Percent of management plans", y = NULL, fill = NULL) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0, "mm"),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.box.margin = margin(0,-10,-10,-10),
        strip.text = element_text(hjust = 0, face = "bold", size = 9),
        plot.margin = margin(2, 10, 2, 2),
        #legend.box.background = element_rect(color = "grey55"),
        #legend.margin = margin(2,4,0,2),
        legend.position = "top")

ggsave(file.path("figs", "Fig2_Stats.png"), width = 6, height = 8, units = c("in"), dpi = 300)

ggplot(data = top_stats %>% filter(entry == 1),
       aes(x = pct, y = q_code_long)) +
  geom_col(aes(fill = level), show.legend = F) +
  geom_text(aes(label = pct), hjust = 1.2, size = 3, col = "white") +
  facet_grid(category ~ ., scales = "free", space = "free") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_manual(values = c("#c9daf8", "#6d9eeb", "#1155CC")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_blank())

ggsave(filename = "plot_nolabel.png", width = 3, height = 6, units = c("in"))

# Plot Data -------------------------------------------------------------------
# export
write.csv(all_stats, "stats.csv")

# Radar Plot -----
# Define basic for each of the categories
radar_vars <- c("climate_plan", "climate_action", "climate_mention",
                "obj_mgmt_any", "obj_cc_any", "obj_lt_any",
                "assess_any_combined", "assess_climate",
                "design_cc_any", "design_resilience", "design_any",
                "monitor_any", "monitor_sci", "monitor_exp_any",
                "threat_cc_combined", "threat_strat_combined", "threat_any_combined")

test <- all %>% 
  filter(q_code %in% radar_vars) %>% 
  filter(entry == 1) %>% 
  group_by(plan_id, category) %>% 
  summarize(score = n()) %>% 
  pivot_wider(names_from = category, values_from = score) %>% 
  column_to_rownames("plan_id")


