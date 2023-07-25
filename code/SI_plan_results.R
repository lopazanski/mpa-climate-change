# Export all plans results as table


# Read 
all_by_plan <- readRDS(file.path("data", "processed", "all_stat_by_plan.Rds"))


supp_table <- all_by_plan %>% 
  select(plan_id, category, stat, type, entry) %>% 
  mutate(type = str_to_lower(type),
         category = str_to_lower(category)) %>% 
  rename("Plan ID" = plan_id,
         "Criteria" = stat,
         "Value" = entry,
         "Climate Change Inclusion" = type,
         "Component" = category)

write.csv(supp_table, file.path("figs", "Supp_Info_TableS4.csv"), row.names = F)

# supp_table %>% 
#   gt() %>% 
#   tab_header(title = md("**Table S5.** Results for each management plan")) %>% 
#   cols_label(plan_id = "Plan ID",
#              stat = "Criteria",
#              entry = "Value",
#              type = "Climate Change Inclusion",
#              category = "Component") %>% 
#   opt_table_font(font = list(google_font("Arial"))) %>% 
#   tab_options(
#     table.border.top.color = "transparent",
#     table.border.bottom.color = "transparent",
#     column_labels.border.top.color = "transparent",
#     column_labels.border.top.width = px(3),
#     heading.align = "left",
#     data_row.padding = px(3)) %>% 
#   gtsave(., filename = file.path("figs", "SI_TabS4_plans.docx"))
