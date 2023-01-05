# Script to create a one year statsys dataset inlcuding multilple SDG columns
# Procedure:
# A (simple step): Add sdg_main_goal_code and sdg_main_target_code to statsys data frame
# B (complex step): Add columns for all SDG-goals and SDG-targets to statsys data frame
# Written by Einar Tornes, 26. March 2021

# Loading packages and relevant data --------------------------------------
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(forcats)
library(writexl)
library(here)

# Noradstats-package: install from github
#library(devtools)
#install_github("einartornes/noradstats")
library(noradstats)

# Find available aid datasets from noradstats google drive
#noradstats::find_aiddata()

# Choose dataset to download. Save in subdirectory (/data)
noradstats::get_aiddata("statsys_ten.csv", here("data", "statsys_ten.csv"))

# Read aid data
df_orig <- noradstats::read_aiddata("data/statsys_ten.csv")

# Make clean column names
df_orig <- janitor::clean_names(df_orig)

# Last year only
df_orig <- df_orig %>% filter(year == max(year))

# A. (simple step): Add sdg_main_goal_code and sdg_main_target_code to statsys data frame ----
df_orig <- df_orig %>%
  mutate(sdg_main_goal_code = str_replace_all(string=df_orig$sdg_focus,pattern="\\..*$",replacement="")) %>%
  mutate(sdg_main_target_code = str_replace_all(string=df_orig$sdg_main_target, pattern="\\ .*$",replacement=""))


# B. (complex step): Add columns for all SDG-goals and SDG-targets to statsys data frame ----
# Procedure:
# Create temporary data frame of only agremment nr and sdg focus
# Split column sdg_focus by semi column in new rows and pivot wider into sdg goals and targets
# Add new columns to original statsys data frame

# Long data frame of agreement_nr and SDG focus only-----------------------------

# Select columns and keep only unique rows
df_sdg <- df_orig %>%
  select(agreement_number, sdg_focus) %>%
  distinct()

# Remove last character in all cells (the last semi-colon)
df_sdg <- df_sdg %>%
  mutate(sdg_focus = str_sub(.$sdg_focus, end = -2))

# Split column cells (sdg_focus) into several rows by semi-column
df_sdg <- separate_rows(df_sdg, sdg_focus, sep = ";")


# Wide data frame with SDG-targets in columns -----------------------------------

# Create variable sdg_dummy to identify agreements with sdg - for pivoting
df_target <- df_sdg %>%
  mutate(sdg_dummy = if_else(!is.na(sdg_focus), TRUE, FALSE))

# Pivot wider to one column per sdg target. Naming NA-values as "FALSE".
df_target <- df_target %>%
  pivot_wider(names_from = sdg_focus,
              values_from = sdg_dummy,
              names_prefix = "sdg_",
              values_fill = FALSE)

# Remove column sdg_NA, not sure why it appears
df_target <- df_target %>%
  select(-sdg_NA)

# Wide data frame with SDG goals in columns -------------------------------------

# Keep only goal and remove target info by using regex
df_goal <- df_sdg %>%
  mutate(sdg_focus = str_replace_all(string=df_sdg$sdg_focus,pattern="\\..*$",replacement=""))

# Create variable sdg_dummy to identify agreements with sdg - for pivoting
df_goal <- df_goal %>%
  mutate(sdg_dummy = if_else(!is.na(sdg_focus), TRUE, FALSE))

# Keep uniqe rows only by removing duplicates
df_goal <- df_goal %>%
  distinct()

# Pivot wider to one column per sdg target. Naming NA-values as "FALSE".
df_goal <- df_goal %>%
  pivot_wider(names_from = sdg_focus,
              values_from = sdg_dummy,
              names_prefix = "sdg_",
              values_fill = FALSE)

# Remove column sdg_NA, not sure why it appears
df_goal <- df_goal %>%
  select(-sdg_NA)

# Include SDG columns (goal and target) in original data frame -------------------

# Join goal and target data frames
df_goal_target <- left_join(x = df_goal, y = df_target,  by = "agreement_number")

# Include columns in original data frame and save
df_orig_new <- left_join(x = df_orig, y = df_goal_target,  by = "agreement_number")



# Main, relevant, not relevant for each 17 SDGs ---------------------------

#SDG 1
df_orig_new <- df_orig_new %>%
  mutate(sdg_1_main_rel = case_when(
    sdg_main_goal_code == 1 ~ "Main",
    sdg_main_goal_code != 1 & sdg_1 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_1_main_rel = fct_relevel(sdg_1_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 2
df_orig_new <- df_orig_new %>%
  mutate(sdg_2_main_rel = case_when(
    sdg_main_goal_code == 2 ~ "Main",
    sdg_main_goal_code != 2 & sdg_2 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_2_main_rel = fct_relevel(sdg_2_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 3
df_orig_new <- df_orig_new %>%
  mutate(sdg_3_main_rel = case_when(
    sdg_main_goal_code == 3 ~ "Main",
    sdg_main_goal_code != 3 & sdg_3 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_3_main_rel = fct_relevel(sdg_3_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 4
df_orig_new <- df_orig_new %>%
  mutate(sdg_4_main_rel = case_when(
    sdg_main_goal_code == 4 ~ "Main",
    sdg_main_goal_code != 4 & sdg_4 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_4_main_rel = fct_relevel(sdg_4_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 5
df_orig_new <- df_orig_new %>%
  mutate(sdg_5_main_rel = case_when(
    sdg_main_goal_code == 5 ~ "Main",
    sdg_main_goal_code != 5 & sdg_5 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_5_main_rel = fct_relevel(sdg_5_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 6
df_orig_new <- df_orig_new %>%
  mutate(sdg_6_main_rel = case_when(
    sdg_main_goal_code == 6 ~ "Main",
    sdg_main_goal_code != 6 & sdg_6 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_6_main_rel = fct_relevel(sdg_6_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 7
df_orig_new <- df_orig_new %>%
  mutate(sdg_7_main_rel = case_when(
    sdg_main_goal_code == 7 ~ "Main",
    sdg_main_goal_code != 7 & sdg_7 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_7_main_rel = fct_relevel(sdg_7_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 8
df_orig_new <- df_orig_new %>%
  mutate(sdg_8_main_rel = case_when(
    sdg_main_goal_code == 8 ~ "Main",
    sdg_main_goal_code != 8 & sdg_8 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_8_main_rel = fct_relevel(sdg_8_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 9
df_orig_new <- df_orig_new %>%
  mutate(sdg_9_main_rel = case_when(
    sdg_main_goal_code == 9 ~ "Main",
    sdg_main_goal_code != 9 & sdg_9 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_9_main_rel = fct_relevel(sdg_9_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 10
df_orig_new <- df_orig_new %>%
  mutate(sdg_10_main_rel = case_when(
    sdg_main_goal_code == 10 ~ "Main",
    sdg_main_goal_code != 10 & sdg_10 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_10_main_rel = fct_relevel(sdg_10_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 11
df_orig_new <- df_orig_new %>%
  mutate(sdg_11_main_rel = case_when(
    sdg_main_goal_code == 11 ~ "Main",
    sdg_main_goal_code != 11 & sdg_11 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_11_main_rel = fct_relevel(sdg_11_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 12
df_orig_new <- df_orig_new %>%
  mutate(sdg_12_main_rel = case_when(
    sdg_main_goal_code == 12 ~ "Main",
    sdg_main_goal_code != 12 & sdg_12 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_12_main_rel = fct_relevel(sdg_12_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 13
df_orig_new <- df_orig_new %>%
  mutate(sdg_13_main_rel = case_when(
    sdg_main_goal_code == 13 ~ "Main",
    sdg_main_goal_code != 13 & sdg_13 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_13_main_rel = fct_relevel(sdg_13_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 14
df_orig_new <- df_orig_new %>%
  mutate(sdg_14_main_rel = case_when(
    sdg_main_goal_code == 14 ~ "Main",
    sdg_main_goal_code != 14 & sdg_14 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_14_main_rel = fct_relevel(sdg_14_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 15
df_orig_new <- df_orig_new %>%
  mutate(sdg_15_main_rel = case_when(
    sdg_main_goal_code == 15 ~ "Main",
    sdg_main_goal_code != 15 & sdg_15 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_15_main_rel = fct_relevel(sdg_15_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 16
df_orig_new <- df_orig_new %>%
  mutate(sdg_16_main_rel = case_when(
    sdg_main_goal_code == 16 ~ "Main",
    sdg_main_goal_code != 16 & sdg_16 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_16_main_rel = fct_relevel(sdg_16_main_rel, "Main", "Relevant", "Not relevant"))

#SDG 17
df_orig_new <- df_orig_new %>%
  mutate(sdg_17_main_rel = case_when(
    sdg_main_goal_code == 17 ~ "Main",
    sdg_main_goal_code != 17 & sdg_17 == TRUE ~ "Relevant",
    TRUE ~ as.character("Not relevant")
  )) %>%
  mutate(sdg_17_main_rel = fct_relevel(sdg_17_main_rel, "Main", "Relevant", "Not relevant"))



# Sorting columns for each sdg_col_main_rel

df_orig_new <- df_orig_new %>%
  mutate(sdg_1_main_rel_sort = case_when(sdg_1_main_rel == "Main" ~ 1,
                                         sdg_1_main_rel == "Relevant" ~ 2,
                                         sdg_1_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_2_main_rel_sort = case_when(sdg_2_main_rel == "Main" ~ 1,
                                         sdg_2_main_rel == "Relevant" ~ 2,
                                         sdg_2_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_3_main_rel_sort = case_when(sdg_3_main_rel == "Main" ~ 1,
                                         sdg_3_main_rel == "Relevant" ~ 2,
                                         sdg_3_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_4_main_rel_sort = case_when(sdg_4_main_rel == "Main" ~ 1,
                                         sdg_4_main_rel == "Relevant" ~ 2,
                                         sdg_4_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_5_main_rel_sort = case_when(sdg_5_main_rel == "Main" ~ 1,
                                         sdg_5_main_rel == "Relevant" ~ 2,
                                         sdg_5_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_6_main_rel_sort = case_when(sdg_6_main_rel == "Main" ~ 1,
                                         sdg_6_main_rel == "Relevant" ~ 2,
                                         sdg_6_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_7_main_rel_sort = case_when(sdg_7_main_rel == "Main" ~ 1,
                                         sdg_7_main_rel == "Relevant" ~ 2,
                                         sdg_7_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_8_main_rel_sort = case_when(sdg_8_main_rel == "Main" ~ 1,
                                         sdg_8_main_rel == "Relevant" ~ 2,
                                         sdg_8_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_9_main_rel_sort = case_when(sdg_9_main_rel == "Main" ~ 1,
                                         sdg_9_main_rel == "Relevant" ~ 2,
                                         sdg_9_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_10_main_rel_sort = case_when(sdg_10_main_rel == "Main" ~ 1,
                                          sdg_10_main_rel == "Relevant" ~ 2,
                                          sdg_10_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_11_main_rel_sort = case_when(sdg_11_main_rel == "Main" ~ 1,
                                          sdg_11_main_rel == "Relevant" ~ 2,
                                          sdg_11_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_12_main_rel_sort = case_when(sdg_12_main_rel == "Main" ~ 1,
                                          sdg_12_main_rel == "Relevant" ~ 2,
                                          sdg_12_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_13_main_rel_sort = case_when(sdg_13_main_rel == "Main" ~ 1,
                                          sdg_13_main_rel == "Relevant" ~ 2,
                                          sdg_13_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_14_main_rel_sort = case_when(sdg_14_main_rel == "Main" ~ 1,
                                          sdg_14_main_rel == "Relevant" ~ 2,
                                          sdg_14_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_15_main_rel_sort = case_when(sdg_15_main_rel == "Main" ~ 1,
                                          sdg_15_main_rel == "Relevant" ~ 2,
                                          sdg_15_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_16_main_rel_sort = case_when(sdg_16_main_rel == "Main" ~ 1,
                                          sdg_16_main_rel == "Relevant" ~ 2,
                                          sdg_16_main_rel == "Not relevant" ~ 3)) %>%
  mutate(sdg_17_main_rel_sort = case_when(sdg_17_main_rel == "Main" ~ 1,
                                          sdg_17_main_rel == "Relevant" ~ 2,
                                          sdg_17_main_rel == "Not relevant" ~ 3))




# Save dataset as xlsx file in subfolder "./output". Create folder if not present.
if(file.exists("./output") == TRUE) {
  writexl::write_xlsx(df_orig_new, path = "output/sdg_dataset.xlsx")
} else {
  dir.create(file.path("./output"))
  writexl::write_xlsx(df_orig_new, path = "output/sdg_dataset.xlsx")
}
