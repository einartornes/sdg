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
library(writexl)

# Noradstats-package: install from github
#library(devtools)
#install_github("einartornes/noradstats")
library(noradstats)

# Find available aid datasets from noradstats google drive
#noradstats::find_aiddata()

# Choose dataset to download. Save in subdirectory (/data)
#noradstats::download_aiddata("oda_ten.csv", subdir = TRUE)

# Read aid data
df_orig <- noradstats::read_aiddata("data/oda_ten.csv")

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

# Save dataset as xlsx file in subfolder "./output". Create folder if not present.
if(file.exists("./output") == TRUE) {
  writexl::write_xlsx(df_orig_new, path = "output/sdg_dataset.xlsx")
} else {
  dir.create(file.path("./output"))
  writexl::write_xlsx(df_orig_new, path = "output/sdg_dataset.xlsx")
}