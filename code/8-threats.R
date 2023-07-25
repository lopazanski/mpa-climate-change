# Threats Plot

# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(gt)

# Directories
data.dir <- file.path("data", "processed")

# Read Data -------------------------------------------------------------------
review  <- readRDS(file.path(data.dir, "processed-doc-review.Rds"))


# Build Data -------------------------------------------------------------------
review_stat <- review[review$type == "stat",]

review_stat_wide <- review_stat %>% 
  pivot_wider(names_from = q_code, values_from = entry, id_cols = plan_id) %>% 
  mutate(across(.cols = c("climate_mention":"threat_habitat_cc"), .fns = as.numeric))


mgmt_stat <- review_stat_wide %>% 
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
  select(mgmt_adapt, 
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
                                 is.na(type) ~ "General Awareness"),
                       levels = c("Climate Action", "Recommended Action", "General Awareness"))) %>% 
  mutate(subcat = recode_factor(subcat,
                                "invasive" = "Invasive Species",
                                "tourism" = "Tourism",
                                "compliance" = "Lack of Compliance",
                                "pollution" = "Marine Debris or Pollution",
                                "habitat" = "Habitat Degradation"))

# Create Plot --------------------------------------------------------------------------------
mgmt_plot <- ggplot(data = mgmt_stat %>% 
                      filter(!(subcat %in% c("adapt","any"))), 
                    aes(y = subcat, x = pct)) +
  geom_bar(stat = "identity", aes(fill = type), position = "dodge") +
  geom_text(data = mgmt_stat %>% 
              filter(!(subcat %in% c("adapt","any"))),
            aes(label = pct, color = type, hjust = -0.1), 
            position = position_dodge2(0.9), 
            #vjust = 2.7, 
            show.legend = F, size = 2) +
  scale_y_discrete(expand = c(0,0))+ 
  scale_fill_manual(values = c("#c9daf8","#6d9eeb", "#1155CC"),
                    breaks = c("General Awareness", "Recommended Action", "Climate Action"))+
  scale_color_manual(values = c("grey25", "grey25", "grey25"),
                     breaks = c("General Awareness", "Recommended Action", "Climate Action")) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0))+
  labs(x = "Percent of management plans",
       y = NULL,
       fill = NULL) +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0, "mm"),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.direction = "vertical",
        legend.box.margin = margin(0,-10,-10,-10),
        strip.text = element_text(hjust = 0, face = "bold", size = 9),
        plot.margin = margin(-5, 8, 2, 2),
        #legend.box.background = element_rect(color = "grey55"),
        #legend.margin = margin(2,4,0,2),
        legend.position = "top")
mgmt_plot
ggsave(file.path("figs", "Fig4_Threats.png"), width = 3.25, height = 3.25, units = c("in"), dpi = 300)

