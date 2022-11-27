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
review_stat <- review[review$type == "stat",]
review_stat$entry[review_stat$q_code == "design_type" & is.na(review_stat$entry)] <- 0

review_stat <- review_stat %>% 
  mutate(entry = if_else(entry %in% c("Planned", 1), 1, 0))

review_stat_wide <- review_stat %>%
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) 


# Create combined threat awareness, strategy, climate change column: 
new_threat <- review_stat_wide %>% 
  mutate(threat_any_combined = if_else(if_any(c(threat_any, threat_compliance, threat_habitat, 
                                                threat_tourism, threat_invasive, threat_pollution)), 1, 0),
         threat_strat_combined = if_else(if_any(c(threat_strat, threat_compliance_strat, 
                                                  threat_habitat_strat, threat_tourism_strat, 
                                                  threat_invasive_strat, threat_pollution_strat)), 1, 0),
         threat_cc_combined = if_else(if_any(c(threat_cc, threat_compliance_cc, 
                                               threat_habitat_cc, threat_tourism_cc, 
                                               threat_invasive_cc, threat_pollution_cc)), 1, 0)) %>% 
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

# One management objective
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

all_stats <- all %>% 
  group_by(category, q_code, entry) %>% 
  summarize(n = n(),
            pct = round(n/171*100, 1)) 

# List of top variables for plot
top_vars <- c("climate_plan", "climate_action", "climate_mention",
              "obj_mgmt_any", "obj_cc_any", "obj_lt_any",
              "assess_any_combined", "assess_climate",
              "design_adaptive_climate", "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
              "mgmt_adapt", "design_cc_any", "design_resilience", "design_any",
              "monitor_any", "monitor_sci", "monitor_soc", "monitor_exp_any",
              "threat_cc_combined", "threat_strat_combined", "threat_any_combined")

# Climate-level variables
climate_vars <- c("climate_plan", "climate_action", 
                  "obj_cc_any", "assess_climate","design_adaptive_climate",
                  "design_cc_any","monitor_exp_any", "threat_cc_combined")

action_vars <- c("obj_lt_any", "assess_any_combined", "design_resilience",
                 "design_adaptive_fully", "design_adaptive_zoning", "design_adaptive_any",
                 "monitor_sci", "monitor_soc", "threat_strat_combined")

aware_vars <- c("climate_mention","obj_mgmt_any", "design_any", "monitor_any", "mgmt_adapt", "threat_any_combined")


top_stats <- all %>% 
  filter(q_code %in% top_vars) %>%
  mutate(level = case_when(q_code %in% climate_vars ~ "Climate Action",
                           q_code %in% action_vars ~ "Recommended Action",
                           q_code %in% aware_vars ~ "General Management Awareness"))%>% 
  mutate(level = factor(level, levels = c("General Management Awareness", "Recommended Action", "Climate Action"))) %>% 
  group_by(category, level, q_code, entry) %>% 
  summarize(n = n(),
            pct = round(n/171*100, 1)) %>% 
  ungroup() %>% 
  mutate(q_code_long = recode_factor(q_code,
                                "climate_plan" = "Detailed climate change action plan",
                                "climate_action" = "At least one strategy explicitly\nconnected to climate change",
                                "climate_mention" = "Mentions climate change",
                                "obj_cc_any" = "Explicit climate change objective",
                                "obj_lt_any" = "Long-term conservation management objective",
                                "obj_mgmt_any" = "Any management objective",
                                "assess_climate" = "Explicit climate change vulnerability assessment\n(completed or planned)",
                                "assess_any_combined" = "Assessment evaluating threats or stressors\n(completed or planned)",
                                "design_adaptive_climate" = "Adaptive design applied with climate considerations",
                                "design_adaptive_fully" = "Potential changes to external boundaries",
                                "design_adaptive_zoning" = "Potential changes to internal zoning",
                                "design_adaptive_any" = "Any adaptive design (flexible zoning or boundaries)",
                                "design_cc_any" = "Design explicitly considers climate change",
                                "design_resilience" = "Design criteria includes at least one\ngeneral resilience principle",
                                "design_any" = "Provides design criteria details",
                                "monitor_imp_any" = "Climate change monitoring (implicit)",
                                "monitor_exp_any" = "Climate change monitoring (explicit)",
                                "monitor_soc" = "Social monitoring",
                                "monitor_sci" = "Scientific monitoring",
                                "monitor_any" = "Any objectives for monitoring",
                                "threat_cc_combined" = "Strategy linked to climate change",
                                "threat_strat_combined" = "Strategy to address identified threat",
                                "threat_any_combined" = "Identified key threat",
                                "mgmt_adapt" = "Plan discusses adaptive management"))

# Plot Data -------------------------------------------------------------------
library(RColorBrewer)

brewer.pal(9, "Blues")

library(rcartocolor)
## All ----
ggplot(data = top_stats %>% filter(entry == 1),
       aes(x = pct, y = q_code_long)) +
  geom_col(aes(fill = level)) +
  geom_text(aes(label = pct), hjust = 1.2, size = 3, col = "white") +
  facet_grid(category ~ ., scales = "free", space = "free") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_manual(values = c("#9ECAE1", "#4292C6", "#08519C")) +
  labs(x = NULL, y = NULL, fill = NULL)

ggsave(filename = "plot.png", width = 9, height = 6, units = c("in"))

ggplot(data = top_stats %>% filter(entry == 1),
       aes(x = pct, y = q_code_long)) +
  geom_col(aes(fill = level), show.legend = F) +
  geom_text(aes(label = pct), hjust = 1.2, size = 3, col = "white") +
  facet_grid(category ~ ., scales = "free", space = "free") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_fill_manual(values = c("#9ECAE1", "#4292C6", "#08519C")) +
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

test[is.na(test)] <- 0

maxmin <- data.frame(climate = c(3, 0),
                     objectives = c(3, 0),
                     assessment = c(2, 0),
                     design = c(3, 0),
                     monitoring = c(3, 0),
                     management = c(3, 0))

rownames(maxmin) <- c("max", "min")

radar_df <- rbind(maxmin, test)

library(fmsb)

radarchart(radar_df, 
           pcol = scales::alpha("grey", 0.5))

library(GGally)
ggparcoord(radar_df)

ggplot() +
  geom_path(data = radar_df %>% 
                rownames_to_column("plan_id") %>% 
                pivot_longer(cols = climate:management, 
                             names_to = "category",
                             values_to = "entry") %>% 
              mutate(category = as.numeric(category)),
              aes(x = category, y = entry),
            position=position_jitter(w=0.02, h=0))
