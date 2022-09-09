# 5-climate-details
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
review_stat <- review[review$type == "stat",]

review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  mutate_at(.vars = c("climate_mention":"threat_habitat_cc"), as.numeric())

# Climate Change ---------------------------------------------------------------

# All basic climate stats
review_stat %>% 
  filter(category == "climate") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

# Plans which only mention climate change with no other climate-relevant
# details (e.g. objectives, design, monitoring, etc.)
review_stat %>% 
  filter(category == "climate" |
           q_code == "obj_cc_any" |
           q_code == "design_cc_any" |
           q_code == "monitor_exp_any") %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(total_climate = rowSums(across(climate_mention:monitor_exp_any))) %>% 
  filter(total_climate > 0) %>% 
  group_by(total_climate) %>% 
  summarize(mention_score = n()) %>% 
  ungroup() %>% 
  mutate(mention_pct = mention_score/sum(mention_score)*100) %>% 
  filter(total_climate == 1)



# Conservation Objectives ---------------------------------------------------------------

# Conservation objectives
review_stat %>% 
  filter(category == "objectives") %>% 
  filter(q_code %in% c("obj_any", "obj_sp", "obj_unit")) %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

# Fine and course filter
review_stat_wide %>% 
  filter(obj_sp == "1" & obj_unit == "1") %>% 
  group_by() %>% 
  summarize(n_obj_both = n(),
            pct_171 = n()/171)

# Any climate objective
review_stat %>%
  filter(category == "objectives") %>%
  group_by(q_code, entry) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = entry, values_from = n) %>%
  mutate(pct_1 = `1`/(`1`+`0`)) %>% 
  filter(q_code == "obj_cc_any")

# General climate adaptation/resilience
review_stat_wide %>% 
  filter(obj_cc_function == 1 |
           obj_cc_general == 1 | 
           obj_cc_resilience == 1) %>% 
  group_by() %>% 
  summarize(gen_cc_obj = n())


review_stat %>%
  filter(category == "objectives") %>%
  group_by(q_code, entry) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = entry, values_from = n) %>%
  mutate(pct_1 = `1`/(`1`+`0`))

# Assessments ---------------------------------------------------------------
# General totals
review_stat %>% 
  filter(category == "assessment") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`+ Planned),
         pct_planned = Planned/(`1`+`0`+ Planned),
         pct_1_and_planned = (`1` + Planned)/(`1`+`0`+ Planned))

# Total completed or planned
review_stat_wide %>% 
  filter(assess_any %in% c("1", "Planned") | assess_climate %in% c("1", "Planned")) %>% 
  group_by() %>% 
  summarize(n_plans_assessment = n_distinct(plan_id),
            n_countries_assessment = n_distinct(country))


# Design ---------------------------------------------------------------
# Any design information
review_stat %>% 
  filter(category == "design") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`)) %>% 
  filter(q_code %in% c("design_any", "design_resilience"))

# Design for resilience


# Monitoring ---------------------------------------------------------------

review_stat %>% 
  filter(category == "monitoring") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

review_stat %>% 
  filter(category == "management") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

# Research
review_stat %>% 
  filter(q_code %in% c("obj_cc_research", "monitor_exp_research"))

