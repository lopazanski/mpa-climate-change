# Export extracted climate change strateges

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


# Select only details
review_detail <- review %>% 
  filter(type == "detail") %>% 
  filter(str_detect(q_code, "cc") | str_detect(q_code, "climate")) %>% 
  filter(!(is.na(entry))) %>% 
  filter(!(q_code %in% c("climate_mention", "climate_action"))) 

strategy_export <- review_detail %>% 
  select(plan_id, name, category, q_long, entry) %>% 
  mutate(q_long = str_remove_all(q_long, "-"))  %>% 
  mutate(q_long = str_trim(q_long)) %>% 
  mutate(category = str_to_sentence(category)) %>% 
  unique() %>% 
  rename("Plan" = plan_id,
         "Name" = name,
         "Component" = category,
         "Indicator" = q_long,
         "Strategy" = entry)

# 
# strategy_export%>% 
#   gt() %>% 
#   tab_header(title = md("**Table S5.** All explicit climate change strategies")) %>% 
#   cols_label(plan_id = "Plan ID",
#              name = "Name",
#              category = "Component",
#              q_long = "Question",
#              entry = "Explicit Strategy",
#              ) %>% 
#   opt_table_font(font = list(google_font("Arial"))) %>% 
#   tab_options(
#     table.border.top.color = "transparent",
#     table.border.bottom.color = "transparent",
#     column_labels.border.top.color = "transparent",
#     column_labels.border.top.width = px(3),
#     heading.align = "left",
#     data_row.padding = px(3)) %>% 
#   gtsave(., filename = file.path("figs", "SI_TabS5_strategies.docx"))


write.csv(strategy_export, file.path("figs", "Supp_Info_TableS5.csv"),
          row.names = F, )
        