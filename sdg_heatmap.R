# The script produces a dataset of the overlap of each SDG main goal and SDG relevant goal. The overlap is measured in disbursements.


# Load packages and sdg dataset -------------------------------------------

library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(forcats)

df <- readxl::read_excel("output/sdg_dataset.xlsx")

# Function to reshape the SDG dataset from wide to long ----------------------------

# Objective: For the chosen SDG main goal, return a dataframe of the overlap in disbursement with each SDG relevant goal
f_sdg <- function(main_goal = NULL) {
  
  # Elements to map over: SDG relevant goals
  cols_rel <- paste0("sdg_", 0:17)
  
  # Mapping procedure and return a dataframe
  table <- map(syms(cols_rel), ~df |>  # rlang syntax
                    filter(type_of_flow == "ODA") |> 
                    filter(type_of_agreement != "Rammeavtale") |> 
                    filter(sdg_main_goal_code == main_goal) |>
                    filter(!!.x == TRUE) |>  # rlang syntax
                    summarise(nok_mill = sum(disbursed_mill_nok))) |> 
    list_rbind()
  
  # Add label columns and arrange factor levels
  table <- table |> 
    add_column(sdg_main = paste0("sdg_", main_goal),
               sdg_relevant = cols_rel, .before = TRUE) |> 
    mutate(sdg_main = factor(sdg_main, levels = paste0("sdg_", 0:17)),
           sdg_relevant = factor(sdg_relevant, levels = paste0("sdg_", 0:17)))
  
  return(table)
}

# Apply function to each main goal and return as a long dataframe-------------------

df_sdg <- map(0:17, f_sdg) |> 
  list_rbind()

# Plot -----------------------------------------------------------------------------
