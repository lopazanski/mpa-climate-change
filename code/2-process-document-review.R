# Process Document Collection Data 
# Cori Lopazanski
# 29 June 2022

# Setup  ----------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
drive.dir <- "/Volumes/GoogleDrive/My Drive/Research/MPA Climate Change - Conservation International/mpa-climate-change-review"
data.dir <- here::here("data", "raw-ish")
    # Called "raw-ish" because some very minimal things are done here.
    # Deep processing should occur in individual scripts (aka this one).
out.dir <- here::here("data", "processed")

# Read Data -------------------------------------------------------------------
review <- readRDS(file.path(data.dir, "exported-doc-review.Rds"))



# Build Data ------------------------------------------------------------------

# Assign new row names
new_rows <- c(paste("Q", seq(1:82), sep = "_"))
rownames(review) <- new_rows

# Extract question key column
q_key <- data.frame(q_id = new_rows,
                    q_code = review$code,
                    q_long = review$`plan-id`)

# Transpose
review_t <- as.data.frame(t(review)) 

# General processing
data <- review_t %>% 
  # Drop old ID rows
  filter(Q_1 != "name") %>% 
  # Add identifier columns
  mutate(plan_id = rep(1:174, each = 2)) %>% 
  select(plan_id, Q_1:Q_82) 

# Remove row names
rownames(data) = NULL

# Add categories to question key
key_detailed <- q_key %>% 
  mutate(q_num = as.numeric(str_remove(q_id, "Q_"))) %>% 
  mutate(category = case_when(q_num %in% c(1:6, 79:82) ~ "metadata",
                              q_num %in% c(7:12) ~ "climate",
                              q_num %in% c(13:30) ~ "objectives",
                              q_num %in% c(31:33) ~ "assessment",
                              q_num %in% c(34:46) ~ "design",
                              q_num %in% c(49:58) ~ "monitoring",
                              q_num %in% c(61:78) ~ "threats",
                              q_num %in% c(47, 48, 59, 60) ~ "management"))

# Lengthen review
review_long <- data %>% 
  pivot_longer(cols = Q_3:Q_82, 
               names_to = "q_id",
               values_to = "entry") %>% 
  rename(name = Q_1,
         type = Q_2) %>% 
  mutate(q_num = as.numeric(str_remove(q_id, "Q_"))) %>% 
  select(plan_id, name, type, q_id, q_num, entry)

# Match review to key 
data <- left_join(review_long, key_detailed)


# Export processed data -------------------------------------------------------
saveRDS(data, file = file.path(out.dir, "processed-doc-review.Rds"))

