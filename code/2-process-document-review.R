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
new_rows <- c(paste("Q", seq(1:83), sep = "_"))
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
  mutate(plan_id = rep(1:175, each = 2)) %>% 
  select(plan_id, Q_1:Q_83) 

# Remove row names
rownames(data) = NULL

# Add categories to question key
key_detailed <- q_key %>% 
  mutate(q_num = as.numeric(str_remove(q_id, "Q_"))) %>% 
  mutate(category = case_when(q_num %in% c(1:6, 80:83) ~ "metadata",
                              q_num %in% c(7:12) ~ "climate",
                              q_num %in% c(13:30) ~ "objectives",
                              q_num %in% c(31:33) ~ "assessment",
                              q_num %in% c(34:47) ~ "design",
                              q_num %in% c(50:59) ~ "monitoring",
                              q_num %in% c(62:79) ~ "threats",
                              q_num %in% c(48, 49, 60, 61) ~ "management"))

# Lengthen review
review_long <- data %>% 
  pivot_longer(cols = Q_3:Q_83, 
               names_to = "q_id",
               values_to = "entry") %>% 
  rename(name = Q_1,
         type = Q_2) %>% 
  mutate(q_num = as.numeric(str_remove(q_id, "Q_"))) %>% 
  select(plan_id, name, type, q_id, q_num, entry)

# Match review to key 
data <- left_join(review_long, key_detailed)

# Changes to the data -------------------------------------------------------

# Exclude the two NZ conservation management strategies
data <- data %>% 
  filter(!(plan_id %in% c(173, 174)))

# Exclude Rickett's Point (Plan = 128) because plan was revoked
data <- data %>% 
  filter(!(plan_id == 128))

# Exclude Comboios (Plan = 103) because plan does not apply to marine area
# at time of writing
data <- data %>% 
  filter(!(plan_id == 103))


# Export processed data -------------------------------------------------------
saveRDS(data, file = file.path(out.dir, "processed-doc-review.Rds"))

