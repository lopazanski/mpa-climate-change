# 5-climate-details
# Cori Lopazanski


# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
area_sf <- readRDS(file.path(data.dir, "processed-area-metadata.Rds"))
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))

# Build Data -------------------------------------------------------------------
review_stat <- review[review$type == "stat",]

review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  mutate(across(.cols = c("climate_mention":"threat_habitat_cc"), .fns = as.numeric))

# Climate Change ---------------------------------------------------------------

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
  #filter(total_climate > 0) %>%  
  group_by(total_climate) %>% 
  summarize(mention_score = n()) %>% 
  ungroup() %>% 
  mutate(mention_pct = mention_score/sum(mention_score)*100) %>% 
  filter(total_climate == 1)

# All basic climate stats
review_stat %>% 
  filter(category == "climate") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`)*100)


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

# All objectives stats
review_stat %>%
  filter(category == "objectives") %>%
  group_by(q_code, entry) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = entry, values_from = n) %>%
  mutate(pct_1 = `1`/(`1`+`0`))

# Management objectives stats by plan
mgmt_obj <- review_stat_wide %>% 
  select(plan_id, obj_lt_any, obj_lt_function,  obj_lt_resilience,
         obj_lt_baseline, obj_lt_monitor,
         obj_lt_reference) %>% 
  mutate(mgmt_eco = obj_lt_function,
         mgmt_monitor = if_else(obj_lt_baseline == 1 | obj_lt_monitor == 1, 1, 0),
         mgmt_res = obj_lt_resilience,
         mgmt_ref = obj_lt_reference) %>% 
  select(plan_id, mgmt_eco, mgmt_monitor, mgmt_res, mgmt_ref) %>%
  mutate(mgmt_any = rowSums(across(mgmt_eco:mgmt_ref))) 

nrow(mgmt_obj[mgmt_obj$mgmt_any == 0,])

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
  filter(assess_any %in% c("1",  NA) | assess_climate %in% c("1", NA)) %>% 
  group_by() %>% 
  summarize(n_plans_assessment = n_distinct(plan_id),
            n_countries_assessment = n_distinct(country))


# Design ---------------------------------------------------------------
# All design information
review_stat %>% 
  filter(category %in% c("design", "adaptive design") ) %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`)*100) 

# Resilience not for climate change
review_stat_wide %>% 
  filter(design_resilience == 1 & design_cc_any == 0) %>% 
  group_by() %>% 
  summarize(n = n_distinct(plan_id),
            pct = n/171*100)

# No resilience but climate strategy
review_stat_wide %>% 
  filter(design_resilience == 0 & design_cc_any != 0) %>% 
  group_by() %>% 
  summarize(n = n(),
            plans = unique(plan_id))

# Type
review %>% 
  filter(type == "detail") %>% 
  filter(q_code == "design_type") %>% 
  group_by(entry) %>% 
  count()

# Both adaptive zoning and adaptive boundaires
review_stat_wide %>% 
  filter(design_adaptive_fully == 1 & design_adaptive_zoning == 1) %>% 
  group_by() %>% 
  summarize(n = n_distinct(plan_id),
            pct = n/171)

# Climate adaptation details
climate_design <- review %>% 
  filter(plan_id %in% review_stat_wide$plan_id[review_stat_wide$design_cc_any == 1 | review_stat_wide$design_adaptive_climate == 1]) %>% 
  filter(category %in% c("design", "adaptive design")) %>% 
  #filter(!(is.na(entry))) %>% 
  filter(!(entry == 0)) %>% 
  filter(q_code == "design_adaptive_climate")

# Monitoring ---------------------------------------------------------------

review_stat %>% 
  filter(category == "monitoring") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`))

# Monitoring objectives but no actual monitoring
review_stat_wide %>% 
  filter(monitor_any == 1 & monitor_sci == 0 & monitor_soc == 0)


review_stat %>% 
  filter(category == "management") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(`1`+`0`)) %>% 
  print(., n = 22)

# Climate Research
review_stat_wide %>% 
  filter(obj_cc_research == 1 | monitor_exp_research == 1) %>% 
  group_by() %>% 
  summarize(n = n(),
            pct = n()/171*100)

# Threats ---------------------------------------------------------------
threats <- review_stat %>% 
  filter(category == "management") %>% 
  group_by(q_code, entry) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = entry, values_from = n) %>% 
  mutate(pct_1 = `1`/(171)*100) %>% 
  mutate(q_code = case_when(q_code == "threat_cc" ~ "threat_any_cc",
                            q_code == "threat_strat" ~ "threat_any_strat",
                            !(q_code %in% c("threat_cc", "threat_strat")) ~ q_code)) %>% 
  separate(q_code,c("mgmt", "category", "type"), "_")  %>% 
  filter(mgmt == "threat") %>% 
  select(category, type, `1`, pct_1) %>% 
  mutate(type = factor(case_when(type == "cc" ~ "Climate Action",
                                 type == "strat" ~ "Recommended Action",
                                 is.na(type) ~ "General Awareness"),
                       levels = c("Climate Action", "Recommended Action", "General Awareness"))) %>% 
  mutate(category = recode_factor(category,
                                  "habitat" = "Habitat Degradation",
                                  "invasive" = "Invasive Species",
                                  "pollution" = "Marine Debris or Pollution",
                                  "tourism" = "Tourism",
                                  "compliance" = "Lack of Compliance")) %>%
  mutate(pct_label = round(pct_1, 1)) %>% 
  filter(!(category == "any"))

ggplot(data = threats,
       aes(y = category, x = pct_1)) +
  geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
  geom_text(data = threats %>% filter(!(type == "Climate Action")),
            aes(label = pct_label, color = type, hjust = 1.05), 
            position = position_dodge2(0.6), vjust = -0.6,
            show.legend = F, size = 3.5)+
  geom_text(data = threats %>% filter(type == "Climate Action"),
            aes(label = pct_label, color = type, hjust = -0.1), 
            position = position_dodge2(0.6), 
            vjust = 2.7, show.legend = F, size = 3.5)+
  scale_y_discrete(expand = c(0,0))+ 
  scale_fill_manual(values = c("#c9daf8","#6d9eeb", "#1155CC"),
                    breaks = c("General Awareness", "Recommended Action", "Climate Action"))+
  scale_color_manual(values = c("grey25", "grey25", "grey25"),
                     breaks = c("General Awareness", "Recommended Action", "Climate Action")) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0))+
  labs(x = "Percent of Management Plans",
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0, "mm"),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        legend.box.margin = margin(0,-10,-10,-10),
        strip.text = element_text(hjust = 0, face = "bold", size = 9),
        plot.margin = margin(2, 10, 2, 2),
        #legend.box.background = element_rect(color = "grey55"),
        #legend.margin = margin(2,4,0,2),
        legend.position = "top")


