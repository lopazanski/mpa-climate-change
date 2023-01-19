# Main Stats for Section 3
# Cori Lopazanski

# 3.1 General Climate
# 3.2 Objectives
# 3.3 Assessments
# 3.4 Design
# 3.5 Monitoring
# 3.6 Management/Threats

# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
# Processed document review
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Refs sheet contains detail about the plan length of time/expiration
refs <- readRDS(file.path("data", "raw-ish", "exported-doc-refs.Rds")) %>% 
  janitor::clean_names() %>% 
  filter(plan_id %in% review$plan_id) %>% 
  filter(document_type == "plan")

# Build Data -------------------------------------------------------------------
review_stat <- review[review$type == "stat",]

review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  mutate(across(.cols = c("climate_mention":"threat_habitat_cc"), .fns = as.numeric))

plan_time <- refs %>% 
  filter(document_type == "plan")

# 3.1 Climate Change -----------------------------------------------------------

climate_stat <- review_stat %>% 
  # Filter for main categories with explicit climate change questions
  filter(category == "climate" | 
           q_code == "obj_cc_any" | 
           q_code == "design_cc_any" | 
           q_code == "monitor_exp_any") %>% 
  # Widen so each explicit climate entry is a column
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  # Convert across columns so all are numeric
  mutate_if(is.character, as.numeric) %>%  # NAs okay
  # Sum across all columns 
  mutate(total_climate = rowSums(across(climate_mention:monitor_exp_any))) %>%
  # Create "only mention" column (total climate = 0)
  mutate(climate_mention_only = if_else(total_climate == 1, 1, 0)) %>% 
  # Create "in between" column (total climate != 0, but climate_plan = 0)
  mutate(climate_some = if_else(total_climate > 1 & climate_plan == 0, 1, 0)) %>% 
  # Select relevant columns
  select(climate_mention, climate_mention_only, climate_plan, climate_some) %>% 
  # Summarize each column
  colSums() %>% as.list() %>% as.data.frame() %>% 
  pivot_longer(everything(),
               names_to = "stat", values_to = "n_plans") %>% 
  mutate(pct = round(n_plans/172*100, 1))

climate_stat

# 3.2 Conservation Objectives --------------------------------------------------
# Conservation objectives
obj_stat <- review_stat_wide %>% 
  # Plans with both species and unit conservation objectives
  mutate(obj_both = if_else(obj_sp == 1 & obj_unit == 1, 1, 0)) %>% 
  # Any management objective 
  mutate(obj_mgmt_any = if_else(if_any(c(obj_any, obj_cc_any, obj_lt_any, monitor_any), 
                                       ~. == 1), 1, 0)) %>% 
  # Long-term management objective matching our categories 
  mutate(obj_mgmt_lt = if_else(if_any(c(obj_lt_function, obj_lt_resilience, 
                                        obj_lt_baseline, obj_lt_monitor, obj_lt_reference),
                                      ~. == 1), 1, 0)) %>% 
  # Long-term individual categories 
  mutate(obj_mgmt_eco = obj_lt_function,
         obj_mgmt_monitor = if_else(obj_lt_baseline == 1 | obj_lt_monitor == 1, 1, 0),
         obj_mgmt_res = obj_lt_resilience,
         obj_mgmt_ref = obj_lt_reference) %>% 
  # Additional general objective for climate change
  # (Promote adaptation or resilience to climate change)
  mutate(obj_cc_gen = if_else(obj_cc_function == 1 |
                                obj_cc_general == 1 | 
                                obj_cc_resilience == 1, 1, 0)) %>% 
  # Select columns for output
  select(obj_any, obj_sp, obj_unit, obj_both, 
         obj_mgmt_any, obj_mgmt_lt, 
         obj_mgmt_eco, obj_mgmt_monitor, obj_mgmt_res, obj_mgmt_ref,
         obj_cc_any, obj_cc_gen) %>% 
  # Summarize and create dataframe
  colSums() %>% as.list() %>% as.data.frame() %>% 
  pivot_longer(everything(), names_to = "stat", values_to = "n_plans") %>%
  # Calculate percentages
  mutate(pct = round(n_plans/172*100, 1))
  
obj_stat

# 3.3 Assessments --------------------------------------------------------------

# General totals
assess_stat <- review_stat_wide %>% 
  # Create "any threat identified" column
  mutate(assess_threat = if_else(if_any(c(threat_any, threat_compliance, 
                                          threat_habitat, threat_tourism, 
                                          threat_invasive, threat_pollution)), 1, 0)) %>% 
  # Future non-climate assessments
  mutate(assess_any_planned = if_else(is.na(assess_any), 1, 0)) %>% 
  # Both past and future non-climate assessments
  mutate(assess_any_both = if_else(assess_any %in% c("1",  NA), 1, 0)) %>% 
  # Future climate assessments
  mutate(assess_cc_planned = if_else(is.na(assess_climate), 1, 0)) %>% 
  # Both past and future climate assessments
  mutate(assess_cc_both = if_else(assess_climate %in% c("1", NA), 1, 0)) %>% 
  # All past and future assessments (both climate and non-climate) 
  mutate(assess_all = if_else(assess_any %in% c("1",  NA) | 
                                assess_climate %in% c("1", NA), 1, 0)) %>%
  select(assess_threat, assess_any, assess_any_planned, assess_any_both,
         assess_climate, assess_cc_planned, assess_cc_both,
         assess_all) %>% 
  pivot_longer(everything(), names_to = "stat", values_to = "entry") %>% 
  filter(entry == 1) %>% 
  group_by(stat, entry) %>% 
  summarize(n_plans = n()) %>%
  mutate(pct = round(n_plans/172*100, 1)) %>% 
  select(stat, n_plans, pct) 
  
assess_stat

# Total completed or planned and countries
review_stat_wide %>% 
  filter(assess_any %in% c("1",  NA) | assess_climate %in% c("1", NA)) %>% 
  group_by() %>% 
  summarize(n_plans = n_distinct(plan_id),
            pct_assess = round(n_plans/172*100,1),
            n_countries = n_distinct(country))


# 3.4 Design ---------------------------------------------------------------
# All design information
review_stat %>% 
  filter(category %in% c("design", "adaptive design") ) %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`)*100) 

design_stat <- review_stat_wide %>% 
  # Design type - only zoning
  mutate(design_type_zoning = if_else(design_type == 0.5, 1, 0)) %>% 
  # Design type - boundaries
  mutate(design_type_boundaries = if_else(design_type == 1, 1, 0)) %>% 
  # Resilience only without explicit climate change
  mutate(design_resilience_all = if_else(design_resilience %in% c(0.5, 1), 1, 0)) %>% 
  # All climate design
  mutate(design_cc_all = if_else(design_cc_any %in% c(0.5, 1), 1, 0)) %>% 
  # Planned future climate design
  mutate(design_cc_planned = if_else(design_cc_any == 0.5, 1, 0)) %>% 
  # Current climate design
  mutate(design_cc_current = if_else(design_cc_any == 1, 1, 0)) %>% 
  # Climate design types
  mutate(design_cc_resilient_all = if_else(design_cc_resilient %in% c(0.5, 1), 1, 0),
         design_cc_critical_all = if_else(design_cc_critical %in% c(0.5, 1), 1, 0),
         design_cc_connect_all = if_else(design_cc_connect %in% c(0.5, 1), 1, 0),
         design_cc_shifting_all = if_else(design_cc_shifting %in% c(0.5, 1), 1, 0),
         design_cc_refugia_all = if_else(design_cc_refugia %in% c(0.5, 1), 1, 0)) %>% 
  # Both adaptive zoning and adaptive boundaires
  mutate(design_adaptive_both = if_else(design_adaptive_fully == 1 & design_adaptive_zoning == 1, 1, 0)) %>% 
  select(design_any, design_type_zoning, design_type_boundaries,design_resilience_all, 
         design_cc_all, design_cc_planned, design_cc_current,
         design_cc_resilient_all, design_cc_critical_all, design_cc_connect_all,
         design_cc_shifting_all, design_cc_refugia_all,
         design_adaptive_any, design_adaptive_fully, design_adaptive_zoning, design_adaptive_both,
         design_adaptive_climate) %>% 
  colSums() %>% as.list() %>% as.data.frame() %>% 
  pivot_longer(everything(), names_to = "stat", values_to = "n_plans") %>%
  # Calculate percentages
  mutate(pct = round(n_plans/172*100, 1))

design_stat

# Climate adaptation details
# climate_design <- review %>% 
#   filter(plan_id %in% review_stat_wide$plan_id[review_stat_wide$design_cc_any == 1 | review_stat_wide$design_adaptive_climate == 1]) %>% 
#   filter(category %in% c("design", "adaptive design")) %>% 
#   #filter(!(is.na(entry))) %>% 
#   filter(!(entry == 0)) %>% 
#   filter(q_code == "design_adaptive_climate")

# 3.5 Monitoring ---------------------------------------------------------------

monitor_stat <- review_stat_wide %>% 
  # Monitoring in development (objectives but no actual monitoring)
  mutate(monitor_planned = if_else(monitor_any == 1 & 
                                     monitor_sci == 0 & 
                                     monitor_soc == 0, 1, 0)) %>% 
  # Only implicit monitoring 
  mutate(monitor_cc_implicit = if_else(monitor_imp_any == 1 & monitor_exp_any == 0, 1, 0)) %>% 
  # Combined implicit or explicit climate monitoring
  mutate(monitor_cc_all = if_else(monitor_imp_any == 1 | monitor_exp_any == 1, 1, 0)) %>% 
  # Combined planned or current climate change research
  mutate(monitor_cc_research = if_else(obj_cc_research == 1 | monitor_exp_research == 1, 1, 0)) %>% 
  select(monitor_any, monitor_planned, monitor_sci, monitor_soc,
         monitor_cc_explicit = monitor_exp_any, 
         monitor_cc_implicit,
         monitor_cc_all, monitor_cc_research) %>% 
  colSums(., na.rm = T) %>% as.list() %>% as.data.frame() %>% 
  pivot_longer(everything(), names_to = "stat", values_to = "n_plans") %>%
  # Calculate percentages
  mutate(pct = round(n_plans/172*100, 1))

monitor_stat

# 3.6 Management ---------------------------------------------------------------

## Plan Time ----
plan_time %>% 
  mutate(work_plan_binary = if_else(is.na(work_plan), 0, 1)) %>% 
  group_by() %>% 
  summarize(min_time = min(time_listed, na.rm = T),
            max_time = max(time_listed, na.rm = T),
            avg_time = mean(time_listed, na.rm = TRUE),
            sd_time = sd(time_listed, na.rm = TRUE),
            num_nas = sum(is.na(time_listed)),
            num_work_plan = sum(work_plan_binary, na.rm = T),
            pct_work_plan = num_work_plan/172)


mgmt_stat <- review_stat_wide %>% 
  # Across all threats - any identified
  mutate(threat_any = if_else(if_any(c(threat_any, threat_compliance, 
                                       threat_habitat, threat_tourism, 
                                       threat_invasive, threat_pollution)), 1, 0)) %>% 
  # Across all threats - any strategy to address
  mutate(threat_any_strat = if_else(if_any(c(threat_strat, threat_compliance_strat, 
                                             threat_habitat_strat, threat_tourism_strat, 
                                             threat_invasive_strat, threat_pollution_strat)), 1, 0)) %>% 
  # Across all threats - any climate considerations
  mutate(threat_any_cc = if_else(if_any(c(threat_cc, threat_compliance_cc, 
                                          threat_habitat_cc, threat_tourism_cc, 
                                          threat_invasive_cc, threat_pollution_cc)), 1, 0)) %>%
  # Discuss protection as mitigation or carbon sink
  mutate(mgmt_adapt_protection = if_else(climate_mitigation == 1 |
                                           climate_sink == 1, 1, 0)) %>% 
  select(mgmt_adapt, mgmt_adapt_protection, mgmt_adapt_green = obj_cc_green, 
         mgmt_adapt_limitation = climate_limitations,
         threat_any, threat_any_strat, threat_any_cc, 
         threat_compliance, threat_compliance_strat, threat_compliance_cc,
         threat_tourism, threat_tourism_strat, threat_tourism_cc,
         threat_pollution, threat_pollution_strat, threat_pollution_cc,
         threat_invasive, threat_invasive_strat, threat_invasive_cc,
         threat_habitat, threat_habitat_strat, threat_habitat_cc) %>%
  colSums(., na.rm = T) %>% as.list() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "stat", values_to = "n_plans") %>%
  # Calculate percentages
  mutate(pct = round(n_plans/172*100, 1)) %>% 
  # Categorize climate inclusion
  separate(stat, into = c("cat", "subcat", "type"), sep = "_", remove = F) %>% 
  mutate(type = factor(case_when(type == "cc" ~ "Climate Action",
                                 type == "strat" ~ "Recommended Action",
                                 is.na(type) ~ "General Awareness",
                                 type %in% c("protection", "green", "limitation") ~ "Climate Action"),
                       levels = c("Climate Action", "Recommended Action", "General Awareness"))) %>% 
  mutate(subcat = recode_factor(subcat,
                                "invasive" = "Invasive Species",
                                "tourism" = "Tourism",
                                "compliance" = "Lack of Compliance",
                                "pollution" = "Marine Debris or Pollution",
                                "habitat" = "Habitat Degradation"))

# Stats in the text  
mgmt_stat %>% 
  filter(subcat %in% c("any", "adapt")) %>% 
  select(stat, n_plans, pct)


